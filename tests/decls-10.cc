int main() {
    t1 (id) = 1;
    t2 (*id) = NULL;
    t3 (id)[n] = {1};
    t4 (*id)[n] = NULL;

    t1 (id) (1);
    v1 (id) (1);

    t2 (*id) (NULL);
    v2 (*id) (NULL);
    t5  *id  (NULL);

    t3 (*id) (NULL) + 1;
    v3 (*id) (NULL) + 1;
    t6  *id  (NULL) + 1;	// Invalid syntax.

    v4 ();
    t5 (*);			// Not valid in a block.
    t6 ()[n];			// Not valid in a block.
    v5 (id);
    v6 (id)[n];
    v7 [n];
    v8 [n] = 1;
}
