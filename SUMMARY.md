# Summary of the Compilers Lab

Make sure to read the [README.md](README.md) before starting for a better understanding of this repository.

This repository contains the following:

-   [Reverse-Polish-Extension](others/reverse-polish/)
    -   2022-01-27-Reverse-Polish-Compiler-Update
-   [Tiger-Compiler-Without-Tree-IR](tag-improvements1/)
    -   2022-02-10-MIPS-AST
    -   2022-02-17-Language-with-Variables
    -   2022-02-24-ForLoops
-   [Tiger-Compiler-With-Tree-IR](compiler-tree-ir/)
    -   2022-03-03-Tree-Intermediate-Representation
    -   2022-03-24-Basic-Blocks
    -   2022-03-31-Graph-for-Compilers

**See the README files of both the versions of the Tiger Compiler in their respective directories for more information about the design of the compiler.**

## Features of the two versions of the Tiger Compiler

-   The Tiger compiler currently supports the following features:
    -   Support for integers
    -   Support for operators `+, -, *, \` along with parentheses, precedence and associativity
    -   Support for assignment (`:=`) statements
    -   Support for **for-loops** and **nested for-loops**
    -   Support for `print` and `println` statements

## Work Done

-   Extended the [**reverse-polish**](others/reverse-polish/) compiler
-   Two version of the tiger compiler as mentioned above
    -   **Fully completed parsing, IR code generation, and MIPS assembly program generations**
    -   Both the versions support the same syntax of the Tiger Language as mentioned in [SYNTAX.md](SYNTAX.md)
    -   Both the compilers are **fully working** and are able to compile to the `MIPS` assembly code that can be run by the `SPIM` simulator
    -   All the given assignments have been fully completed with fully functioning code and there are no bugs in them
    -   In the **first version of the Tiger Compiler**, all the phases which include `Tiger -> IR -> MIPS` have been implemented
    -   In the **second version of the Tiger Compiler**, all the phases which include `Tiger -> Tree IR -> Canonicalized Tree IR -> MIPS` have been implemented
-   The **basicBlocks** assignment. See **files**:
    -   [compiler-tree-ir/utils/basicBlocks.sml](compiler-tree-ir/utils/basicBlocks.sml)
    -   [compiler-tree-ir/target/mipsInst.sml](compiler-tree-ir/target/mipsInst.sml)
-   The **graphs** assignment. See **file**:
    -   [compiler-tree-ir/utils/graph.sml](compiler-tree-ir/utils/graph.sml)
-   Added `Makefile` and **tests** for both the versions of the Tiger Compiler
-   Maintained the [CHANGELOG.md](CHANGELOG.md) file with proper **git-tags** throughout the development of the project

## TODO List

These are the additional work that can be done on the Tiger compiler **(but were not there in the assignment)**:

-   Support for strings and floating point numbers
-   Support for comments
-   Support for features like nested functions, while loops, control statements like if-then, if-then-else, and let blocks.
