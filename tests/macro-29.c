int f(void)
{
#define A(b)                                    \
    int abc ## b;                               \
    g()
#define A(b)                                    \
    int abc                                     \
        ## b;                                   \
    g()
#define B(s)                                    \
    =                                           \
        # s ;
}
