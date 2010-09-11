function hello(free) {
    return function (arg) {
	print(free + arg);
    }
}

var func = hello("Hello, ");
func("World!");

