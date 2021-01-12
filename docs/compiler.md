# Compiler

The ire compiler is structured into many passes that take an input variable
and produce an output. The eventual output of these passes is an llvm ir file.

The first step in compilation is to read the input file. 
## Datatypes used in compilation
there are a few intermediate representations the source goes through:
  - tokenstream. see lexing below for details
  - AST. this is a polymorphic type parameterized by its identifiers. 
  - IR. this is an intermediate representation sitting between ast and llvm to enable easier transformations before generating llvm
  - LLVM. this is generated them dumped for llc to read.

## Passes 

### Lexing (Parser/Lexer.hs)
The lexer takes the raw text and splits into tokens (also known as lexemes). 
this recognizes numbers, strings, and identifiers, and keywords. there are other single-character
tokens, like commas, brackets, infix operaters like +, etc. tokens are also annotated with there source string,
and line in the file they occur at.

### Parsing (Parser/)
the parser takes the tokenstream and produces the AST (abstract syntax tree), parameterized with Strings for identifiers.

### Name resolution (Pass/Namer.hs)
the strings in the AST are then replaced with Name, which is unique within the file. 

### Type inference
the types are then inferred. this is done by generating constraints for every expression, and then
performing unification on all the constraints.
for example, an assignment like x = 0 would create the equivalence relation x ~ 0. unification would solve
by using the subsitution x = 0.

### IR generation
the AST is the lowered to the IR

### IR passes
before the IR can be lowered to LLVM, there are a few problems to deal with. One is that llvm does not allow
nested functions, but ire does. do solve this the functions can be "lifted" into global functions. There is 
a caveat here where if a function captures a variable ("closure"), the function must be further parameterized by
an environment containing the free variables. normal stack allocated variables do not suffice as an assignment to them
will not propagate back up to the parent function. As a result, the first step is to identify the free variables in
nested functions, and to promote them to a variable living on the heap. all uses and writes to the variable need to be
updated to pointer derefence first. then, the functions can be lifted to the top level and fvs placed in an env.
note all functions become a struct with the funtion pointer and environment to have uniform calling convension across them.
there is another optimization to prevent this before these passes, where top-level functions directly called are given
a special note on the IR tree.

### LLVM generation
after this, the LLVM code can be generated from the IR


## Compiler directory layout
  - /runtime: runtime library that is linked with all binaries from irec. this provides important functionality like memory allocation etc.
  - /tests: test cases for irec
  - /CLI: command line interface (frontend). Main is the main cli, and Tester is the runner of files in /tests/
  - /compiler: main compiler directory
  - /docs: documentation
  - /stdlib: standard library. this is a git subtree (for now)
  - /build: build files written by tools like configure.py
  
