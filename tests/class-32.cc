class A final : B
{
public:
    void foo() override;
};

namespace
{
    cc_test::cc_test (Arg arg)
        : base {arg}
    {
    }
}

struct Base2 {
    virtual void f() final;
};

class cc_test final
    : public base
{
    cc_test ()
        : base ()             // old style base/member initialization syntax
        , arg_ {}
    {
    }
    cc_test ()
        : base {}             // new style (C++11) base/member initialization
        , arg_ {}
    {
    }
};
