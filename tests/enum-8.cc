enum class Enumeration
{
    Val1,
    Val2,
    Val3 = 100,
    Val4 /* = 101 */
};

enum class Enum2 : unsigned int
{
    Val1,
    Val2
};

enum Enum3 : unsigned long {Val1 = 1, Val2};

enum Enum2 : unsigned int;    //Legal in C++0x.
enum class Enum3;             //Legal in C++0x, because enum class
                              //declarations have a default type of
                              //"int".
enum class Enum4: unsigned int; //Legal C++0x.
