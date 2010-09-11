-*- outline -*-

Tamacola - Cola (Scheme like dynamic compiler) on Tamarin

This program is tested with Flash Player 10 and Tamarin 714

* commands

- bin/tamacc Tamacola Compiler
- bin/tamacola Tamacola Shell
- bin/mkswf SWF Archiver
- bin/abcsx ABCSX assembler

* build

** Run all examples and demos
make run-example

** Recompile commands
make -C bin

** Boot from Ian Piumarta's cola compiler
make boot	# Commands in bin/ directory is replaced.

* directories

- bin		: Commands are stored
- abcsx		: ABC S-expression assembler
- cola		: Original Cola source tree
- boot		: Tamacola written in Cola
- runtime	: Tamacola written in Tamacola itself
- stage2	: A working directory for boot strapping
- tools		: Profiler for avmshell and Squeak Moshi
- example
- ws    	: Workspace on the web browser

* Make target

- make		: Recompile
- make boot	: Boot strapping from Cola
- make test	: Run all test cases
- make all	: Do everything
- make count	: Current LOC in the project

* Special form

... : repeat
[]  : optional

** (), #t, #f, #self, and #undefined

null, true, false, this, undefined in ActionScript respectively.

** (let ((tmp1 value1) (tmp2 value2) ...) exprs ...)
** (let (tmp1 tmp2 ...) exprs ...)

Evaluate expressions with local variables tmp1, tmp2, ...

** (class name (super interfaces ...) (members ...) [constructor])

Create an ActionScript class.

Example:
(class Foo (Object) (first second)
       (lambda (arg) (slot-set! #self first arg)))

** (constructsuper args ...)

Call the constructor of the base class.

** (send object message arguments ...)

Send the message to the object.

** (slot-get   object string-or-number)
** (slot-getq  object name)
** (slot-set!  object string-or-number value)
** (slot-setq! object name value)

Slot accessors.

** (new class args ...)

Make a new instance of the class.

** (not expr)

Boolean negation.

** (instanceof object type)

Return True if object's prototype chain includes a type

** (try-catch variable type try-block finally-block)

When an exception happens in try-block, and it is matched the type, finally-block is run.
See example/exception.k

BUG: try-catch can be used only inside a function (lambda).

** (undefined? name)

Returns true if name is undefined or not defined in this scope.

** (import namespace)

Add the namespace to access variables.

Unlinke the library in R6RS, namespace is a single name instead of a
list of name (I might change it someday). Use name.space instead of
(name space).

Also, slash "/" is used as the delimiter between the namespace and the
local name (Cola/Clojure style). Note that unlike Clojure, slash
does'n mean a class member accessor. Use (slot-getq Math PI) instead
of Math/PI.

** (library namespace exprs ...)

Declare the namespace. All definitions in the exprs are defined in the
namespace.

Example:

(library myproj.mylib
  (define myvar "Hello"))

is equivalent to (define myproj.mylib/myvar "Hello")

* See also

Tamarin
source code: http://hg.mozilla.org/tamarin-central
documentation: https://developer.mozilla.org/En/Tamarin/Tamarin_Build_Documentation

* NOTE: How to build tamarin.

$ hg clone http://hg.mozilla.org/tamarin-central/
$ cd tamarin-central
$ mkdir objdir-release
$ cd objdir-release
$ python ../configure.py --enable-shell --enable-debugger

(in case of Snow Leopard)
$ python ../configure.py --enable-shell --enable-debugger --target=x86_64-darwin

* NOTE: How to run tamacola shell on MinGW

For some reason, the tamacola shell can not handle MinGW nor emacs
console. You should run tamacola from cmd.exe 

Open cmd.exe
Type following commands in the console

C:\msys\1.0\bin\sh
PATH=$PATH:/bin:/usr/local/bin
cd stage2/
make tamacola
./tamacola

* NOTE: How to bootstrap Tamacola

(This source tree doesn't include the COLA distribution which is not
published yet for open public, sorry!)

For full-bootstrapping, you need to checkout the COLA distribution to
cola/ directory. COLA.patch must be applied to the COLA source tree
before it is built. COLA.patch allows you to use Scheme style hex
numbers.

$ svn checkout http://cola-c3-trunk-repository cola
(If it answers just svn: REPORT of ...: 200 OK, do it again)

$ (cd cola; patch -p0 < ../cola.diff)
$ make boot

* Download

Tamacola source code:
http://www.vpri.org/vp_wiki/index.php/Tamacola

Demos
* Forall Workspace: http://tinlizzie.org/~takashi/tamacola/forall/Workspace.swf
* COLA Workspace: http://tinlizzie.org/~takashi/tamacola/ws/Workspace.html
		
Tamarin virtual machine
* For Mac OS X: http://tinlizzie.org/~takashi/tamacola/avmshell-mac.zip
* For Windows: http://tinlizzie.org/~takashi/tamacola/avmshell-win.zip
