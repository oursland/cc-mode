Type var = init, x = Type();
Type (*var) = init;
Type var[3 * peq] = init;
Type[xyz] var = init;		// FIXME: Java only?
Type (var) = init;		// Currently interpreted as a function call.
Type int = "int";		// int

const Type var;
const Type (*var);
const Type var[3 * peq];
const Type (var);		// Recognized as decl due to the const keyword.

Type (*foo) (Type *,
	     Type (*)[x],
	     Type (*var)[x],
	     Type (var*)[x],	// An incorrect one.
	     Type &);
