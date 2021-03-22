// RUN: %clangxx -target x86_64-unknown-unknown -g \
// RUN:   %s -emit-llvm -S -o - | FileCheck %s

// RUN: %clangxx -target x86_64-unknown-unknown -g \
// RUN:   -fno-elide-constructors %s -emit-llvm -S -o - | \
// RUN:   FileCheck %s -check-prefix=NOELIDE

struct Foo {
  Foo() = default;
  Foo(Foo &&other) { x = other.x; }
  int x;
};
void some_function(int);
Foo getFoo() {
  Foo foo;
  foo.x = 41;
  some_function(foo.x);
  return foo;
}

int main() {
  Foo bar = getFoo();
  return bar.x;
}

// Check that NRVO variables are stored as a pointer with deref if they are
// stored in the return register.

// CHECK: call void @llvm.dbg.declare
// NOELIDE: call void @llvm.dbg.declare