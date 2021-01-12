# Ire language

### Expressions
 - variable: "name"
 - function call: "f x"
 - literal: "1", or "(a, b)" (tuple literal), or "\x -> e" (function literal) 
 - infix operation, "e1+e2"
 - if: "if e1 then e2 else e3"
 - block expression "{stmt1;stmt2;...stmtn}" (note: semicolon and line break are considered semantically equivalent.)
expressions can be composed. 

### Definitions
 definitions take the form name:type = expression. the type can be ommitted, and will be inferred by the compiler.
 
### Statements
Statements are used in blocks (see expressions above)
 - expression
 - yield: "yield ex". this exits the block, and the blocks value is then ex.
 - assignment "name = ex". this reassigns name to the value expression
 - definition (see above)
 - return: "return ex". this exits the function, returning the value ex.


### Layout
an ire file consists of zero or more definitions. top level definitions must be functions (at the moment).

### Using functions defined in other files
to use a function in another file, the file must be imported using an import at the top of the file, 
like "import "foo.ire"". the file foo then needs to export the function using export. "export bar" for example.
