namespace test_fixit_multiple {
  class foo { ~foo() { } }; class bar { ~bar(); }; bar::~bar() { }
}
