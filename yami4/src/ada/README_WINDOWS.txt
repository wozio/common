Note for GNAT users on Windows:

In order to compile the Ada library on Windows the core library
needs to be compiled with the compiler toolchain that is provided
with the GNAT compiler.
Unfortunately, the GNAT distribution does not contain the make program,
so that has to be downloaded by the user separately.

Follow these steps to successfully compile YAMI4 Ada library
and accompanying tests and examples:

1. Download mingw32-make program from the MinGW website.
   Adding it to the GNAT\2009\bin directory is a good way to have it
   easily accessible.

2. Go to ..\core and compile the core library by running:
   mingw32-make -f Makefile.MinGW

3. Go back to this directory (src\ada) and compile the Ada library:
   gnatmake -Pyami

Both tests and examples can be then compiled without problems.
The operating system should be automatically recognized as "Windows_NT" -
if that is not the case, add the -XOS=Windows option when invoking gnatmake.
