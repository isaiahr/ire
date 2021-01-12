# The Ire programming language
ire is a general-purpose compiled programming language.

### Building the compiler
There are 3 steps to building the compiler:
  - generating config files: ./configure.py
  - building the runtime: cd runtime && make && cd ../
  - compiling the compiler: stack build

the ire compiler needs llvm & lld as runtime dependencies. the configure script should detect them, 
and warn if they are not installed.

### Language documentation
See docs/language.md, or tests/ for worked examples (these may not be idiomatic, they are designed as test cases)


### Usage and options
usage: irec \[options\] file

irec -h for options.
irec has support for cross-compilation. right now the only supported targets are amd64 linux and arm64 linux.
use -target=\<target\>  to select a target 

### Compiler documentation
See docs/compiler.md for details how the compiler is structured.


