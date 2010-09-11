package {
    public class Foo extends Object {
	public var foo = "Hello,";
    }
    public class Bar extends Foo {
	public var bar = "World!";
    }
}

var bar = new Bar();
print(bar.foo, bar.bar);
