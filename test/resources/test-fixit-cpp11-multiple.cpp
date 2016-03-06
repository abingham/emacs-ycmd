namespace test_fixit_multiple {
  class foo { ~bar() { } }; class bar { ~bar(); }; ~bar::bar() { }
}
