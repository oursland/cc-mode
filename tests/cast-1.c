void f() {
    x (id);
    (id, id);
    (id, id) x;			/* Invalid syntax. */
    x (id, id);
    x (id, id) x;		/* Invalid syntax. */
    (id);
    x (t1) x;			/* Invalid syntax. */
    (*x) (id);
    (*x) (t2) x;		/* Invalid syntax. */
    (t3*) (id);
    (t4*) (t5) x;		/* Invalid syntax. */
    /* The following might be a function call too, but since the
     * parenthesis around "t1" can just as well be removed then, we
     * always treat it as a cast. */
    (t6) (id);
    (t7) x;
    (t8*) x;
    (x * x);
    (t9) (t10) (1);
    (t11()[n]) x;
    x (y) (1);
    x (y) (z) (1);
}
