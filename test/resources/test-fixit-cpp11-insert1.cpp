struct A {
  explicit operator int();
};


void x() {
  switch(A()) {
  }
}
