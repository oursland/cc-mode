@interface Foo
@end

@interface Foo : Bar
@end

@interface Foo (Bar)
@end

@interface Foo <X, Y, Z>
@end

@interface Foo : Bar <X, Y, Z>
@end

@interface Foo (Bar) <X, Y, Z>
@end

@implementation Foo
@end

@implementation Foo : Bar
@end

@implementation Foo (Bar)
@end

@implementation Foo <X, Y, Z>
@end

@implementation Foo : Bar <X, Y, Z>
@end

@implementation Foo (Bar) <X, Y, Z>
@end

@protocol Foo
@end

@class Foo;
@class Foo, Bar;
