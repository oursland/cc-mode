void f() {
    FOO1
	/* a */
	{
	    x;
	}
    FOO2()
	/* a */
	{
	    x;
	}
    FOO3 (a, b)
	/* a */
	{
	    x;
	}
    int FOO4
	/* a */
    {
	x;
    }
    int FOO5()
	/* a */
    {
	x;
    }
    /* The following case is currently not recognized correctly. */
    int *FOO6()
	/* a */
	{
	    x;
	}
    if (foo)
	{
	    x;
	}
}
