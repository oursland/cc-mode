int main() {
    t1 (id) = 1;
    t2 (*id) = NULL;
    t3 (id)[n] = {1};
    t4 (*id)[n] = NULL;

    t1 (id) (1);
    v1 (id) (1);

    t2 (*id) (NULL);
    v2 (*id) (NULL);
    v9  *id  (NULL);

    t2 (*id) (t7*x);		// The last paren could also be an initializer.
    v10(*id) (v12*x);
    v11 *id  (v13*x);

    t3 (*id) (NULL) + 1;
    v3 (*id) (NULL) + 1;
    v10 *id  (NULL) + 1;

    v4 ();
    t5 (*);			// Not valid in a block.
    t6 ()[n];			// Not valid in a block.
    v5 (id);
    v6 (id)[n];
    v7 [n];
    v8 [n] = 1;

    t8 * id;
    v14 *;
    (v15 * id);
    x (a*b, c*d);
    v16 (*id);
}
