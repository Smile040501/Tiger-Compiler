# Changelog

All the weekly changes made in the project as a summary.

## [Unreleased]

## [2022-03-31] - 2022-04-07

### Added

-   `utils/graph.sml`: **MkGraph** functor to represent a graph

### Updated

-   `tc.mlb`: Includes the new files added in their respective order

## [2022-03-24] - 2022-03-31

### Added

-   `utils/basicBlocks.sml`: Captures the signatures and functor for basicBlocks computation
-   `target/mipsInst.sml`: Basic blocks for MIPS program

### Updated

-   `tc.mlb`: Includes the new files added in their respective order

## [Improvements1] - 2022-03-29

### Added

-   Added workflow for Makefile tests and `tests` directory to contain all the unit tests.
-   `SYNTAX.md` for the specifications of syntax for the SubTiger language.
-   `DESIGN.md` for the information about the design of the compiler.
-   `notes/CommandLine.sml` and `notes/OS.sml`: Contains notes for referencing some SML built-in structures.
-   `target/mips`: Added more utility functions to create different types of records
-   Added support for `println` command in Tiger language
-   Added input file extension validation for the compiler.

### Updated

-   The compiler now by default creates a file `filename.s` for the generated assembly where the filename is the input `filename.tig` without the extension.
-   Lexer now supports better syntax error handling
-   `INSTRUCTIONS.md` for better instruction usage.
-   `.gitignore` file to ignore some more temporary files generated.
-   `README.md` file for the project.
-   Better comments and indentation in many files

### Fixed

-   Fixed **For-loops** always checking the greater than condition for loop termination in the generated assembly code. Now it checks based on the start and the end of the loop.
-   Fixed pretty printing of ascii directives in MIPS assembly.

## [2022-02-24] - 2022-03-15

### Added

-   Support for parsing the **For-loops** in the Tiger language.
-   Some more utility functions in the **Utils** structure and the **ConvToMips** structure

### Modified

-   Moved register allocation phase at the time of new temporary assignment
-   Changed single environment to list of environments while translating input Tiger program for supporting nested sub-blocks (in **Translate** structure)
-   Changed the return type of functions to now return two list of statements, one for the statements in the `main` body of MIPS and one for the statements in the `data` section of the MIPS
-   Fixed `label` type for the **MIPS** statement

## [2022-02-17] - 2022-03-14

### Added

-   Fully functional compiler code for the SubTiger Language. The compiler only supports integers and variables as of now and only the assignment and print statements.
-   **Directories**:
    -   `tiger`: Contains the code for capturing the AST of the Tiger Language, the parser and lexer for the Tiger Language, and some other utility structures for pretty printing Tiger AST and conversion to the Tiger AST.
    -   `ir`: Contains the code for translating the source of Tiger AST to the Intermediate Representation and the code for greedy register allocation.
    -   `utils`: Contains the code for some helper utility structures.
    -   `notes`: Contains notes for referencing some SML built-in structures.
-   **Other files**:
    -   `target/prettyMips.sml`: The code for pretty printing of the MIPS AST has been moved to its separate file in a separate SML structure.
    -   `target/convToMIPS.sml`: Contains code for easy conversion of the input values to the MIPS AST.
    -   `tc.mlb`: ML-Basis file for building the project using **MLton**
    -   `INSTALL.md`: Contains the installation instructions for the compiler.
    -   `INSTRUCTIONS.md`: Contains the instructions for using the compiler and the Makefile.

### Updated

-   `README.md` to now contain better information of the repository.
-   `tc.sml`: Removed dummy basic code and now contains the code for the **Tiger Driver** for building the tiger compiler.
-   `Makefile`: Now contains the full workflow for building the project.
-   Structure `MIPS` as follows:
    -   Changed casing of the register names
    -   Refactored the code for the AST by creating different types for records and splitting the types of instructions based on the type of records

## [2022-02-10] - 2022-02-17

### Added

-   `target/mips.sml` file in the root of the repository containing the AST of the MIPS assembly language.

## [2022-01-27] - 2022-02-05

### Added

-   `reverse-polish` directory for the reverse polish compiler.
-   Support for division and parentheses to evaluate expressions using reverse-polish.
    -   DIV has been added to the AST of the source.
    -   DIV, LPAREN and RPAREN have been added to the list of terminals in `expr.grm` in reverse polish
    -   Regex for division and parentheses have been added to the lexer.
-   Sample test cases for division and parentheses in `test.expr` in reverse polish
-   `ANSWERS.md` in reverse polish which contains answers to the questions asked in README.md of the reverse-polish lab statement.

### Modified

-   Global `.gitignore` to ignore more temporary files

## [0.0.1] - 2022-01-27

### Added

-   `tc.sml`: An sml program that when executed prints "hello wordl"
-   `Makefile`: A basic make file with targets `all` and `clean`.
    -   `make all`: should build the executable `tc` from `tc.sml`
    -   `make clean`: remove all generated/intermediate files from the repository (in this case only the executable file `tc`)
-   `.gitignore`: Global gitignore file.

[unreleased]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/2022-03-31...master
[2022-03-31]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/2022-03-24...2022-03-31
[2022-03-24]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/improvements1...2022-03-24
[improvements1]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/2022-02-24...improvements1
[2022-02-24]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/2022-02-17...2022-02-24
[2022-02-17]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/2022-02-10...2022-02-17
[2022-02-10]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/2022-01-27...2022-02-10
[2022-01-27]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/v0.0.1...2022-01-27
[0.0.1]: https://gitlab.com/singlamayank001/111901030-compilers/-/releases#v0.0.1
