void|int foo (mapping(int:string) test)
{
    int a = catch
    {
	write (foo);
	yeti();
    };
    write (1 + sin(7),
	   foo, gauge {
	       innit();
	       yeti();
	   },
	   gluu
	);
    write (1 + sin(7),
	   foo, gauge
	   {
	       innit();
	   }
	);
    write (
	foo, gauge {
	    innit();
	},
	gluu
	);
    write (
	foo, gauge
	{
	    innit();
	}
	);
}
