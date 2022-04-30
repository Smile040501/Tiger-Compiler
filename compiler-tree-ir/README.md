# Design of the SubTiger Compiler (With Tree IR)

The source code of the compiler is distributed across the following directories and files:

-   `ir/`
    -   `canon.sml`: Contains the code for Tree IR canonization
    -   `frame.sml`: Represents the stack frame used while translating the AST to Tree IR
    -   `info.sml`: Contains the info required while translating the AST to Tree IR
    -   `prettyTree.sml`: Contains the code for pretty printing the Tree IR code
    -   `translate.sml`: Contains the code for translating the AST to Tree IR
    -   `tree.sig`: Contains the signature of the Tree IR
    -   `tree.sml`: Contains the structure of the Tree IR
-   `target/`
    -   `codeGen.sml`: Contains the code for generating the MIPS code from the Canonicalized Tree IR
    -   `convToMIPS.sml`: Contains helper functions to create MIPS assembly statements
    -   `mips.sig` and `mips.sml`: Signature and structure of the MIPS AST
    -   `mipsInst.sml`: Contains the structure for **Mips-Inst-Basic-Blocks**
    -   `prettyMips.sml`: Contains helper functions for the pretty printing of the MIPS assembly code
-   `tiger/`
    -   `ast.sig` and `ast.sml`: Signature and structure of the Tiger AST
    -   `convToTiger.sml`: Contains helper functions to create Tiger expressions
    -   `prettyTigerAST.sml`: Contains helper functions for the pretty printing of the Tiger expressions
    -   `tiger.grm`: Grammar of the Tiger language
    -   `tiger.lex`: Lexical analysis file
-   `utils/`
    -   `basicBlocks.sml`: Contains the code for the basic blocks assignment
    -   `graph.sml`: Contains the code for the graphs assignment
    -   `temp.sml`: Structure to create new temporary values and labels
    -   `utils.sml`: General purpose helper functions
-   `tc.mlb`: The ML-Basis file for compilation
-   `tc.sml`: The main code where the execution begins. This uses the lexer-parser, translator and pretty printers to generate MIPS assembly from the Tiger source code

# Design Choices

We have used the following design choices:

1. Everything would be organized on the stack, even the variables that are declared at the top level are organized on the stack since the top level code is itself is thought to be inside a function declaration.
2. Currenlty, we don't have a register allocation algorithm, hence most computation stores the value into the stack, retrieves the value into temporary registers, performs the computation and places the result into the stack. This causes the compiled program to be somewhat slow.
3. There are built-in functions for printing expression values, they are `print` and `println` which print the value without or with a newline respectively
4. The error checking mechanism is quite week as of now

# Restrictions on the Tree Intermediate Code

These restrictions help in less work to be left for the canonization and code generation.

-   BINOP can take only temporaries as input
-   All expressions should be of the form Tree.ESEQ (stmt, resultTemp)
    -   This way we will always extract the statement from the ESEQ and the resultTemp and we will have no ESEQ statements in the final Tree IR code
-   CALL can only be used by the `print` and `println` functions for actual function call. These will be the only functions that will have argument list with one register. For the rest of the functions, arguments will be pushed onto the stack.
-   CALL are always the child of the EXP
-   We can return the return values in the return value register
-   MOVE can happen between MEM and TEMP, TEMP and MEM, TEMP and BINOP applied on TEMPs, TEMP and CONST
