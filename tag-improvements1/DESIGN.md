# Design of the SubTiger Compiler

The source code of the compiler is distributed across the following directories and files:

-   `ir/ir.sml`: Contains structure of the Intermediate Representation (IR)
-   `ir/regAlloc.sml`: Greedy register allocation
-   `ir/translate.sml`: Contains the code for translating the source of Tiger AST to the Intermediate Representation and the code for translating the Intermediate Representation to the assembly code
-   `target/convToMIPS.sml`: Contains helper functions to create MIPS assembly statements
-   `target/mips.sig` and `target/mips.sml`: Signature and structure of the MIPS AST
-   `target/prettyMips.sml`: Contains helper functions for the pretty printing of the MIPS assembly code
-   `tiger/ast.sig` and `tiger/ast.sml`: Signature and structure of the Tiger AST
-   `tiger/convToTiger.sml`: Contains helper functions to create Tiger expressions
-   `tiger/prettyTigerAST.sml`: Contains helper functions for the pretty printing of the Tiger expressions
-   `tiger/tiger.grm`: Grammar of the Tiger language
-   `tiger/tiger.lex`: Lexical analysis file
-   `utils/env.sml`: Structure to create an environment from user variable to temporaries
-   `utils/temp.sml`: Structure to create new temporary values and labels
-   `utils/utils.sml`: General purpose helper functions
-   `tc.sml`: The main code where the execution begins. This uses the lexer-parser, translator and pretty printers to generate MIPS assembly from the Tiger source code

# Design Choices

We have used the following design choices:

1. Currently register allocation is implemented as a greedy register allocation and if the compiler runs out of registers just, it just flags an error
2. There are built-in functions for printing expression values, they are `print` and `println` which print the value without or with a newline respectively
3. The error checking mechanism is quite week as of now
