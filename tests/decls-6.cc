__INLINE__ FOO Type var, x;
__INLINE__ FOO Type (*var);
__INLINE__ FOO Type var[3 * peq];
__INLINE__ FOO Type (var);

__INLINE__ FOO Type var = init, x = Type();
__INLINE__ FOO Type (*var) = init;
__INLINE__ FOO Type var[3 * peq] = init;
__INLINE__ FOO Type (var) = init;
__INLINE__ FOO Type int = "int"; // int

Type var = init, x = Type();
Type (*var) = init;
Type var[3 * peq] = init;
Type (var) = init;
Type int = "int";		// int

const Type var;
const Type (*var);
const Type var[3 * peq];
const Type (var);

Type (*foo) (Type *,
	     Type (*)[x],
	     Type (*var)[x],
	     // An incorrect one that gets "var" recorded as a type.
	     Type (var*)[x],
	     Type &);

Type2 var;
Type var;
Type (*var);
Type var[3 * peq];
Type (var);
::Type var;

unsigned foo bar;
long long x;
long double y;
int x y;			// Invalid
int int y;			// Invalid

// This should be last to check a certain case.
#define low_assign_multiset_index(TO, NODE) do {			\
    struct svalue *_ms_index_to2_ = (TO);				\
  } while (0)
