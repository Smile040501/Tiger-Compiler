# Installing the SubTiger compiler provided in this repository

-   The source for the compiler is written in `Standard ML`, hence make sure to install the essential software tools on your system.

    ```bash
    # install Standard ML interpreter
    sudo apt install smlnj smlnj-doc libsmlnj-smlnj ml-build

    # install Standard ML compiler
    sudo apt install mlton

    # install required utilities for building parser and lexers
    sudo apt install ml-yacc ml-lex ml-burg ml-lpt

    # install the SPIM simulator
    sudo apt install spim
    ```

-   Clone this repository and execute the command `make` from the terminal in the root directory of this project to generate the binary executable
-   See [INSTRUCTIONS.md](INSTRUCTIONS.md) and [SYNTAX.md](SYNTAX.md) for more information on how to use the compiler
