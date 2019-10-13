// RUN: %check_clang_tidy %s modernize-deprecated-iterator-base %t 

namespace std {
using ptrdiff_t = int;
struct input_iterator_tag;
template <class C, class T, class D = ptrdiff_t, class P = T*, class R = T&>
struct iterator {
  using iterator_category = C;
  using value_type        = T;
  using difference_type   = D;
  using pointer           = P;
  using reference         = R;
};
}


using iterator_alias = std::iterator<std::input_iterator_tag, int>;
typedef std::iterator<std::input_iterator_tag, long> iterator_typedef;


// Sugar

// CHECK-FIXES: struct from_alias {
// CHECK-MESSAGES: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct from_alias: iterator_alias {};

// CHECK-FIXES: struct from_typedef {
// CHECK-MESSAGES: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct from_typedef: iterator_typedef {};


// False-positive

// CHECK-FIXES: struct indirect_base: from_alias {};
// CHECK-MESSAGES-NOT: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct indirect_base: from_alias {};


// Unsupported

// CHECK-FIXES: class skipif_non_public_inheritance: iterator_alias {};
// CHECK-MESSAGES: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
class skipif_non_public_inheritance: iterator_alias {};


// Base removal

struct A {};
struct B {};

struct collection {
  template <class...>
  struct iterator;
};

// CHECK-FIXES: template <> struct collection::iterator<> {
// CHECK-MESSAGES: :[[@LINE+1]]:45: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
template <> struct collection::iterator<> : iterator_alias {};
// CHECK-FIXES: template <> struct collection::iterator<A> : A {
// CHECK-MESSAGES: :[[@LINE+1]]:49: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
template <> struct collection::iterator<A> : A, iterator_alias {};
// CHECK-FIXES: template <> struct collection::iterator<B> : B {
// CHECK-MESSAGES: :[[@LINE+1]]:46: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
template <> struct collection::iterator<B> : iterator_alias, B {};
// CHECK-FIXES: template <> struct collection::iterator<A, B> : A, B {
// CHECK-MESSAGES: :[[@LINE+1]]:52: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
template <> struct collection::iterator<A, B> : A, iterator_alias, B {};

// CHECK-FIXES: struct do_not_strip_final final {
// CHECK-MESSAGES: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct do_not_strip_final final : iterator_alias {};

// CHECK-FIXES: struct iteratorZ // iterator_alias
// CHECK-FIXES: {
// CHECK-MESSAGES: :[[@LINE+2]]:5: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iteratorZ   // iteratorZ
  : iterator_alias // iterator_alias
{};
// CHECK-FIXES: struct iteratorA   // iteratorA
// CHECK-FIXES:   : A // iterator_alias
// CHECK-FIXES: {
// CHECK-MESSAGES: :[[@LINE+3]]:5: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iteratorA   // iteratorA
  : A              // A
  , iterator_alias // iterator_alias
{};
// CHECK-FIXES: struct iteratorB   // iteratorB
// CHECK-FIXES:   : B              // B
// CHECK-FIXES: {
// CHECK-MESSAGES: :[[@LINE+2]]:5: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iteratorB   // iteratorB
  : iterator_alias // iterator_alias
  , B              // B
{};
// CHECK-FIXES: struct iteratorAB  // iteratorAB
// CHECK-FIXES:   : A              // A
// CHECK-FIXES:   , B              // B
// CHECK-FIXES: {
// CHECK-MESSAGES: :[[@LINE+3]]:5: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iteratorAB  // iteratorAB
  : A              // A
  , iterator_alias // iterator_alias
  , B              // B
{};
// CHECK-FIXES: struct iterator0Z // iterator_alias
// CHECK-FIXES: {
// CHECK-MESSAGES: :[[@LINE+2]]:5: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator0Z : // iterator0Z
    iterator_alias  // iterator_alias
{};
// CHECK-FIXES: struct iterator0A : // iterator0A
// CHECK-FIXES:     A // iterator_alias
// CHECK-FIXES: {
// CHECK-MESSAGES: :[[@LINE+3]]:5: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator0A : // iterator0A
    A,              // A
    iterator_alias  // iterator_alias
{};
// CHECK-FIXES: struct iterator0B : // iterator0B
// CHECK-FIXES:     B               // B
// CHECK-FIXES: {
// CHECK-MESSAGES: :[[@LINE+2]]:5: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator0B : // iterator0B
    iterator_alias, // iterator_alias
    B               // B
{};
// CHECK-FIXES: struct iterator0AB : // iterator0AB
// CHECK-FIXES:     A,               // A
// CHECK-FIXES:     B                // B
// CHECK-FIXES: {
// CHECK-MESSAGES: :[[@LINE+3]]:5: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator0AB : // iterator0AB
    A,               // A
    iterator_alias,  // iterator_alias
    B                // B
{};


// Opening/closing placement

// CHECK-FIXES:      class iterator00 {
// CHECK-FIXES-NEXT: public:
// CHECK-FIXES:      private:
// CHECK-MESSAGES: :[[@LINE+1]]:27: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
class iterator00 : public iterator_alias {
  int dummy;
};
// CHECK-FIXES:      class iterator01 {
// CHECK-FIXES-NEXT: public:
// CHECK-FIXES-NOT:  private:
// CHECK-MESSAGES: :[[@LINE+1]]:27: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
class iterator01 : public iterator_alias {
protected:
};
// CHECK-FIXES:      class iterator02 {
// CHECK-FIXES-NEXT: public:
// CHECK-FIXES-NOT:  private:
// CHECK-MESSAGES: :[[@LINE+1]]:27: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
class iterator02 : public iterator_alias {
public:
protected:
};

// CHECK-FIXES:      struct iterator10 {
// CHECK-FIXES-NEXT: public:
// CHECK-FIXES-NOT:  private:
// CHECK-MESSAGES: :[[@LINE+1]]:28: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator10 : public iterator_alias {
  int dummy;
};
// CHECK-FIXES:      struct iterator11 {
// CHECK-FIXES-NEXT: public:
// CHECK-FIXES-NOT:  private:
// CHECK-MESSAGES: :[[@LINE+1]]:28: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator11 : public iterator_alias {
protected:
};
// CHECK-FIXES:      struct iterator12 {
// CHECK-FIXES-NEXT: public:
// CHECK-FIXES-NOT:  private:
// CHECK-MESSAGES: :[[@LINE+1]]:28: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator12 : public iterator_alias {
public:
protected:
};

// CHECK-FIXES:      struct iterator20 {
// CHECK-FIXES-NEXT: using iterator_category = std::input_iterator_tag;
// CHECK-FIXES-NOT:  private:
// CHECK-MESSAGES: :[[@LINE+1]]:21: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator20 : iterator_alias {
  int dummy;
};
// CHECK-FIXES:      struct iterator21 {
// CHECK-FIXES-NEXT: using iterator_category = std::input_iterator_tag;
// CHECK-FIXES-NOT:  private:
// CHECK-MESSAGES: :[[@LINE+1]]:21: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator21 : iterator_alias {
protected:
};
// CHECK-FIXES:      struct iterator22 {
// CHECK-FIXES-NEXT: public:
// CHECK-FIXES-NOT:  private:
// CHECK-MESSAGES: :[[@LINE+1]]:21: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct iterator22 : iterator_alias {
public:
protected:
};


// Typedefs

// CHECK-FIXES:      struct basic {
// CHECK-FIXES-NEXT:   using iterator_category = std::input_iterator_tag;
// CHECK-FIXES-NEXT:   using value_type        = int;
// CHECK-FIXES-NEXT:   using difference_type   = std::ptrdiff_t;
// CHECK-FIXES-NEXT:   using pointer           = int {{\*}};
// CHECK-FIXES-NEXT:   using reference         = int &;
// CHECK-FIXES-NEXT-EMPTY:
// CHECK-MESSAGES: :[[@LINE+1]]:16: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct basic : std::iterator<std::input_iterator_tag, int> {};

// CHECK-FIXES:      class nontempl
// CHECK-FIXES-NEXT: {
// CHECK-FIXES-NEXT:   public:
// CHECK-FIXES-NEXT:     using iterator_category = std::input_iterator_tag;
// CHECK-FIXES-NEXT:     using value_type        = int;
// CHECK-FIXES-NEXT:     using difference_type   = long;
// CHECK-FIXES-NEXT:     using pointer           = int const{{\*}};
// CHECK-FIXES-NEXT:     using reference         = int const&;
// CHECK-FIXES-NEXT-EMPTY:
// CHECK-MESSAGES: :[[@LINE+2]]:12: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
class nontempl
  : public std::iterator
    < std::input_iterator_tag    // iterator_category
    , int                        // value_type
    , long                       // difference_type
    , int const*                 // pointer
    , int const&                 // reference
    >
{
  private:
    int dummy;
};

// CHECK-FIXES:      class templ
// CHECK-FIXES-NEXT: {
// CHECK-FIXES-NEXT:   public:
// CHECK-FIXES-NEXT:     using iterator_category = typename T::C;
// CHECK-FIXES-NEXT:     using value_type        = typename T::V;
// CHECK-FIXES-NEXT:     using difference_type   = typename T::D;
// CHECK-FIXES-NEXT:     using pointer           = typename T::P;
// CHECK-FIXES-NEXT:     using reference         = typename T::R;
// CHECK-FIXES-NEXT-EMPTY:
// CHECK-MESSAGES: :[[@LINE+3]]:12: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
template <class T>
class templ
  : public std::iterator
    < typename T::C              // iterator_category
    , typename T::V              // value_type
    , typename T::D              // difference_type
    , typename T::P              // pointer
    , typename T::R              // reference
    >
{
  protected:
    int dummy;
};

// CHECK-FIXES:      struct redeclared
// CHECK-FIXES-NEXT: {
// CHECK-FIXES-NEXT:   using iterator_category = std::input_iterator_tag;
// CHECK-FIXES-NEXT:   using value_type        = void ;
// CHECK-FIXES-NEXT:   using difference_type   = long;
// CHECK-FIXES-NEXT:   struct pointer {} ;
// CHECK-FIXES-NEXT:   using reference         = int&;
// CHECK-FIXES-NEXT: };
// CHECK-MESSAGES: :[[@LINE+2]]:12: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
struct redeclared
  : public std::iterator<
      std::input_iterator_tag,   // iterator_category
      int,                       // value_type
      long,                      // difference_type
      int*,                      // pointer
      int&>                      // reference
{
  using value_type        = void ;
  struct pointer {} ;
};


// Indentation

// CHECK-FIXES:      {{^  class indent_use_rec {}}
// CHECK-FIXES-NEXT: {{^  public:}}
// CHECK-FIXES-NEXT: {{^  using}}
// CHECK-MESSAGES: :[[@LINE+1]]:33: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
  class indent_use_rec : public iterator_alias {
  };
// CHECK-FIXES:      {{^  class indent_use_acc {}}
// CHECK-FIXES-NEXT: {{^    public:}}
// CHECK-FIXES-NEXT: {{^    using}}
// CHECK-MESSAGES: :[[@LINE+1]]:33: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
  class indent_use_acc : public iterator_alias {
    public:
  };
// CHECK-FIXES:      {{^  class indent_use_memb {}}
// CHECK-FIXES-NEXT: {{^  public:}}
// CHECK-FIXES-NEXT: {{^    using}}
// CHECK-MESSAGES: :[[@LINE+1]]:34: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
  class indent_use_memb : public iterator_alias {
    int dummy;
  };
// CHECK-FIXES:      {{^  class indent_use_both {}}
// CHECK-FIXES-NEXT: {{^    public:}}
// CHECK-FIXES-NEXT: {{^      using}}
// CHECK-MESSAGES: :[[@LINE+1]]:34: warning: inheriting from 'std::iterator' is deprecated [modernize-deprecated-iterator-base]
  class indent_use_both : public iterator_alias {
    protected:
      int dummy;
  };
