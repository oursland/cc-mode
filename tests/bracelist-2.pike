mixed foo (a,
	   b, c
    );
mixed foo (
    a,
    b, c
    );
mixed foo = {a,
	     b, c
};
mixed foo = ({a,
	      b, c
});
mixed foo = ([
    a:1,
    b:"foo", c:3
]);
mixed foo = (<a,
	      b, c
>);
a = {
    a, b,
    {a, {a,
	 b
    }},
    foo ({a,
	  b},
	 c), ([a,
	       b,
	       (<
		   a, b, c
	       >)
	 ])}
