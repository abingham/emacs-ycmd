struct A {
  explicit operator int();
};


void x() {
  switch(static_cast<int>(A())) {
  }
}
