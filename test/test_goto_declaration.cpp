struct Foo {
    void bar();
};

void Foo::bar() {}

int main(int, char**) {
    Foo f;
    f.bar();
}
