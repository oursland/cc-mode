void f()
    {
    foo = new foo[]
	{
	a, b,
	    {
	    c, d,
	    { e, f },
	    g, h
	    },
	i, j
	};
    foo = new foo[]
	{
	    {
		{ e, f },
		g, h
	    },
	    i, j
	};
    foo = new foo[] {
	a, b, {
	    c, d, {
		e, f },
	    g, h
	    },
	i, j
	};
    foo = new foo[] {
	    { { e, f },
	      g, h
	    },
	    i, j
	};
    }

/* Local Variables: */
/* c-file-style: "whitesmith" */
/* End: */
