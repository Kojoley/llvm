//===--- DeprecatedIteratorBaseCheck.cpp - clang-tidy ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "DeprecatedIteratorBaseCheck.h"
#include "../utils/LexerUtils.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "llvm/ADT/Sequence.h"

using namespace clang::ast_matchers;
using namespace clang::tidy::utils::lexer;

namespace clang {
namespace tidy {
namespace modernize {

/// Returns the first child of the CXXRecordDecl
static const Decl *getTopDecl(const CXXRecordDecl &RD) {
  auto It = RD.decls_begin();
  // The first decl of CXXRecordDecl is self-reference, skip it
  assert(It->getKind() == Decl::CXXRecord && "expecting self-reference");
  ++It;
  return It != RD.decls_end() ? *It : nullptr;
}

/// Returns an AccessSpecDecl if it is the first child of the CXXRecordDecl
static const AccessSpecDecl *getTopAccSpecDecl(const CXXRecordDecl &RD) {
  return dyn_cast_or_null<AccessSpecDecl>(getTopDecl(RD));
}

/// Returns an AccessSpecDecl child of the DeclContext, if any
static const AccessSpecDecl *getAnyAccSpecDecl(const DeclContext &DC) {
  for (const Decl *D : DC.decls())
    if (auto const *ASD = dyn_cast<AccessSpecDecl>(D))
      return ASD;
  return nullptr;
}

/// Returns a non-AccessSpecDecl child of the CXXRecordDecl, if any
static const Decl *getAnyNonAccSpecDecl(const CXXRecordDecl &RD) {
  auto It = RD.decls_begin();
  // The first decl of CXXRecordDecl is self-reference, skip it
  assert(It->getKind() == Decl::CXXRecord && "expecting self-reference");
  ++It;
  for (; It != RD.decls_end(); ++It)
    if (It->getKind() != Decl::AccessSpec)
      return *It;
  return nullptr;
}

/// Returns default visibility for the TagDecl children
static AccessSpecifier defaultAccessSpecifierFor(const TagDecl &TD) {
  switch (TD.getTagKind()) {
  case TTK_Struct:
    return AS_public;
  case TTK_Class:
    return AS_private;
  default:
    llvm_unreachable("unexpected tag kind");
  }
}

/// Returns a base removal fixit
static FixItHint createBaseRemoval(const CXXRecordDecl &RD,
                                   const CXXBaseSpecifier &Base,
                                   const SourceManager &SM,
                                   const LangOptions &LangOpts) {
  SourceRange R = Base.getSourceRange();
  if (RD.getNumBases() == 1) {
    // class RD : public Base {
    //         ^^^^^^^^^^^^^^
    // TODO: Is there a way to get the location without lexer gymnastics?
    SourceLocation Colon =
        findPreviousTokenKind(R.getBegin(), SM, LangOpts, tok::colon);
    Token Tok = getPreviousToken(Colon, SM, LangOpts, /*SkipComments=*/true);
    assert(!Tok.is(tok::unknown));
    R.setBegin(Lexer::getLocForEndOfToken(Tok.getLocation(), 0, SM, LangOpts));
  } else if (RD.bases_end() - 1 == &Base) {
    // class RD : ... , public Base {
    //               ^^^^^^^^^^^^^^
    SourceLocation Loc = (RD.bases_end() - 2)->getEndLoc();
    R.setBegin(Lexer::getLocForEndOfToken(Loc, 0, SM, LangOpts));
  } else {
    // class RD : ... , public Base, ... {
    //                  ^^^^^^^^^^^^^
    auto It =
        llvm::find_if(RD.bases(), [&Base](const CXXBaseSpecifier &Candidate) {
          return &Candidate == &Base;
        });
    R.setEnd(std::next(It)->getBeginLoc().getLocWithOffset(-1));
  }
  return FixItHint::CreateRemoval(R);
}

/// Returns an aliases insertion location for the CXXRecordDecl
static SourceLocation getInsertLoc(const CXXRecordDecl &RD) {
  const AccessSpecDecl *TopAS = getTopAccSpecDecl(RD);
  if (TopAS && TopAS->getAccessUnsafe() == AS_public) {
    // class/struct {
    // public:
    //        ^
    return TopAS->getColonLoc().getLocWithOffset(1);
  } else {
    // class/struct {
    //               ^
    return RD.getBraceRange().getBegin().getLocWithOffset(1);
  }
}

static bool isOpeningRequired(const CXXRecordDecl &RD) {
  const AccessSpecDecl *TopAS = getTopAccSpecDecl(RD);
  if (TopAS && TopAS->getAccessUnsafe() == AS_public) {
    // class/struct {
    // public:
    // ^^^^^^^
    // Public access specifier is already at the top
    return false;
  }
  if (RD.isStruct()) {
    // struct : public ... {
    // public:  vvvvvv
    // ^^^^^^^<<<<<<<<
    // When a user types public access specifier for a struct base
    // it is highly likely to also find it at the struct body top
    for (const CXXBaseSpecifier &Base : RD.bases())
      switch (Base.getAccessSpecifierAsWritten()) {
      case AS_public:
        return true;
      case AS_none:
        return false;
      default:
        break;
      }
  }
  return defaultAccessSpecifierFor(RD) != AS_public;
}

static bool isClosingRequired(const CXXRecordDecl &RD) {
  if (getTopAccSpecDecl(RD)) {
    // class/struct {
    // public/private/protected:
    // ^^^^^^^^^^^^^^^^^^^^^^^^^
    // No need to emit a closing access specifier because there is already one
    return false;
  } else {
    return defaultAccessSpecifierFor(RD) != AS_public;
  }
}

static TemplateSpecializationTypeLoc getTSTLoc(TypeLoc TL) {
  if (auto TSTL = TL.getAsAdjusted<TemplateSpecializationTypeLoc>())
    return TSTL;
  if (auto TDTL = TL.getAs<TypedefTypeLoc>())
    return getTSTLoc(
        TDTL.getTypedefNameDecl()->getTypeSourceInfo()->getTypeLoc());
  llvm_unreachable("failed to desugar TemplateSpecializationTypeLoc");
}

namespace {

AST_MATCHER_P(Type, asTST,
              ast_matchers::internal::Matcher<TemplateSpecializationType>,
              InnerMatcher) {
  if (const auto *TST = Node.getAs<TemplateSpecializationType>())
    return InnerMatcher.matches(*TST, Finder, Builder);
  return false;
}

enum StdIteratorArg {
  ARG_iterator_category = 0,
  ARG_value_type,
  ARG_difference_type,
  ARG_pointer,
  ARG_reference,
  ARG_num,
};

} // namespace

void DeprecatedIteratorBaseCheck::registerMatchers(MatchFinder *Finder) {
  // Requires C++.
  if (!getLangOpts().CPlusPlus)
    return;

  // Match record declarations which have std::iterator base.
  Finder->addMatcher(
      cxxRecordDecl(
          anyOf(isClass(), isStruct()), isDefinition(),
          unless(ast_matchers::isTemplateInstantiation()),
          hasDirectBase(cxxBaseSpecifier(
                            hasType(asTST(templateSpecializationType(
                                              hasDeclaration(namedDecl(
                                                  hasName("::std::iterator"))))
                                              .bind("tst"))))
                            .bind("base")))
          .bind("subj"),
      this);
}

void DeprecatedIteratorBaseCheck::check(
    const MatchFinder::MatchResult &Result) {
  SourceManager &SM = *Result.SourceManager;
  ASTContext &Context = *Result.Context;
  const auto &Subj = *Result.Nodes.getNodeAs<CXXRecordDecl>("subj");
  const auto &Base = *Result.Nodes.getNodeAs<CXXBaseSpecifier>("base");
  const auto &TST = *Result.Nodes.getNodeAs<TemplateSpecializationType>("tst");
  const auto TSTL = getTSTLoc(Base.getTypeSourceInfo()->getTypeLoc());

  auto Diag = diag(Base.getBaseTypeLoc(),
                   "inheriting from 'std::iterator' is deprecated");

  // Non public inheritance from std::iterator? Skip the strange beast.
  if (Base.getAccessSpecifier() != AS_public)
    return;

  StringRef IndentAccSpec;
  if (const AccessSpecDecl *ASD = getAnyAccSpecDecl(Subj))
    IndentAccSpec = Lexer::getIndentationForLine(ASD->getLocation(), SM);
  else
    IndentAccSpec = Lexer::getIndentationForLine(Subj.getBeginLoc(), SM);

  StringRef Indent;
  if (const Decl *D = getAnyNonAccSpecDecl(Subj))
    Indent = Lexer::getIndentationForLine(D->getLocation(), SM);
  else
    Indent = IndentAccSpec;

  auto GetRealRange = [&](SourceRange Range) {
    return Lexer::makeFileCharRange(CharSourceRange::getTokenRange(Range), SM,
                                    getLangOpts());
  };

  static const StringRef Names[] = {
      "iterator_category", "value_type       ", "difference_type  ",
      "pointer          ", "reference        ",
  };

  SourceRange Overrides[std::size(Names)];
  llvm::transform(Names, std::begin(Overrides), [&](StringRef Name) {
    SourceRange Range;
    if (const auto *ND = selectFirst<const NamedDecl>(
            "arg", match(cxxRecordDecl(
                             has(namedDecl(hasName(Name.rtrim())).bind("arg"))),
                         Subj, Context))) {
      Range = GetRealRange(ND->getSourceRange()).getAsRange();
      Range.setEnd(findNextTerminator(Range.getEnd(), SM, getLangOpts())
                       .getLocWithOffset(1));
    }
    return Range;
  });

  auto ArgToVal = [&](unsigned Idx) -> std::string {
    if (Idx < TSTL.getNumArgs()) {
      SourceRange Range = TSTL.getArgLoc(Idx).getSourceRange();
      CharSourceRange CharRange = GetRealRange(Range);
      return Lexer::getSourceText(CharRange, SM, getLangOpts()).str();
    }
    switch (Idx) {
    case ARG_difference_type:
      return "std::ptrdiff_t";
    case ARG_pointer:
      return Context.getPointerType(TST.getArg(ARG_value_type).getAsType())
          .getAsString(getLangOpts());
    case ARG_reference:
      return Context
          .getLValueReferenceType(TST.getArg(ARG_value_type).getAsType())
          .getAsString(getLangOpts());
    default:
      llvm_unreachable("unexpected argument index");
    }
  };
  auto GenAliasForArg = [&](unsigned Idx) {
    return (llvm::Twine("using ") + Names[Idx] + " = " + ArgToVal(Idx) + ";")
        .str();
  };
  auto EmitterAnchor =
      llvm::find_if(Overrides, [](SourceRange SR) { return SR.isValid(); });
  if (EmitterAnchor != std::end(Overrides)) {
    const auto B = std::begin(Overrides);
    for (auto I = B; I != EmitterAnchor; ++I)
      Diag << FixItHint::CreateInsertion(
          EmitterAnchor->getBegin(),
          (llvm::Twine(GenAliasForArg(std::distance(B, I))) + "\n" + Indent)
              .str());

    for (auto I = std::next(EmitterAnchor); I != std::end(Overrides); ++I)
      if (I->isValid())
        EmitterAnchor = I;
      else
        Diag << FixItHint::CreateInsertion(
            EmitterAnchor->getEnd(),
            (llvm::Twine("\n") + Indent + GenAliasForArg(std::distance(B, I)))
                .str());
  } else {
    SmallString<256> Buf;
    llvm::raw_svector_ostream StrOS(Buf);

    if (isOpeningRequired(Subj))
      StrOS << '\n' << IndentAccSpec << "public:";

    for (auto I : llvm::seq<unsigned>(0, ARG_num))
      StrOS << '\n' << Indent << GenAliasForArg(I);

    StrOS << '\n';

    if (isClosingRequired(Subj))
      StrOS << '\n' << IndentAccSpec << "private:";

    Diag << FixItHint::CreateInsertion(getInsertLoc(Subj), StrOS.str());
  }
  Diag << createBaseRemoval(Subj, Base, SM, getLangOpts());
}

} // namespace modernize
} // namespace tidy
} // namespace clang
