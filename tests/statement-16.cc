int foo()
{
    int a; if (a)
	       printf ("hmm\n");
    int b; b =
	       printf ("hmm\n");
  x:
  foo: if (a)
      printf ("hmm\n");		// Ought to compensate for the label.
  bar: b=
      printf ("hmm\n");		// Same here.
}
