# Changelog

All the weekly changes made in the project as a summary.

## [Unreleased]

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

[unreleased]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/2022-01-27...master
[2022-01-27]: https://gitlab.com/singlamayank001/111901030-compilers/-/compare/v0.0.1...2022-01-27
[0.0.1]: https://gitlab.com/singlamayank001/111901030-compilers/-/releases#v0.0.1
