# The Ire programming language
ire is a general-purpose compiled programming language.

### Installing the compiler
There are 3 steps to installing the compiler:
  - generating config files: ./build.py configure
  - building the runtime and compiler: ./build.py build 
  - installing: sudo ./build.py install
  
Note that for development, add "dev" to the end of configure. Using "make" and "stack build/run/test" is better than
running build.py build dev when developing on ire itself.

the ire compiler needs llvm & lld as runtime dependencies. the configure script should detect them, 
and warn if they are not installed.

### Language documentation
See docs/language.md, or tests/ for worked examples (these may not be idiomatic, they are designed as test cases)


### Usage and options
usage: irec \[options\] file

irec -h for options.
irec has support for cross-compilation. right now the only supported targets are "linux-amd64" and "linux-aarch64".
use --target=\<target\>  to select a target 

### Compiler documentation
See docs/compiler.md for details how the compiler is structured.


