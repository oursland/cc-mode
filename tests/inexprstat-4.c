int foo() {
    FOO (
	{
	    a;
	}
	);
    BAR (a,
	 {
	     a;
	 });
    FOO (
	if (a)
	    x;
	);
    BAR (a,
	 if (a)
	     x;
	);
}
