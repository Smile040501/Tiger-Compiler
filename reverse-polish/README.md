# Compiling to Reverse Polish notation (RPN)

A compiler is program that takes as input a code written in a _source
language_ `Lₛ` (typically a high level language like SML, C, etc) and
translates it into a _target language_ `Lₜ` (typically a low level
language like the machine language of your computer). Compilers are
some of the most complex programs and in this course, we look at how
they are built. As part of the lab component of this course, we will
build a compiler whose source language `Lₛ` is Tiger language as
defined in the book ["Modern Compiler Implementation"][book], and the
target language `Lₜ` is the [MIPS assembly language][mips].

Before we embark on this ambitious journey, we would like to start
with a modest yet illustrative compiler.  In this directory, we give a
toy compiler whose source language `Lₛ` is the language of expressions
and the target language `Lₜ` is the language of expressions in reverse
polish notation. The program is structured like a classical compiler
and more complex compilers can follow this high level structure.

## Contents of this directory

This directory contains the source code for two standalone programs.

`ec`
:   The expression compiler which compiles normal expressions into
    reverse polish notation. You should think of expressions as your
    high level language and reverse polish notation as your low level
    language.

`rp`
:   The reverse polish machine, which interprets the expression in
    reverse polish notation. This is given to test your compiler. You
    can think of this as the machine on which the compiled programs
    run.

Build these programs using the following make command

```
make all
```

## The Reverse Polish machine.

The *reverse polish* assembly language takes a sequence of *machine
instructions* which consists of

1. A number with an optional sign [+-~]. One can use the ~ sign for
   negation following the Standard ML convention.

2. Single character commands `p` (for printing the top of the stack)
   `s` for printing the entire stack and `c` for clearing the stack.

3. Line comments starting with the character `#`.

The executable `rp` that this repository provides can be used to run a
_reverse polish script_. For example, here is a "machine language
program" for `rp` that illustrates the syntax.


```
# This is a sample script for rpn
# We compute the answer to life universe and everything

2 40 +ps

```

Save the above script into a file say `test.inp` and run it using the
command

```
./rp test.inp

```

The above program prints 42 (because of the `p`) followed by `[42]`
(because of the `s`).



## Compilers as tree functions.

The input program to the compiler is a string and the generated
executable is also a string. So a compiler is nothing but a string
translation function. However, strings are inconvenient to work with
and hence programs (in various languages) are represented by trees
called the _abstract syntax trees_ (AST for short). Each program
written in a language `L` can be represented uniquely by (labelled)
trees and the heart of the compiler is therefore a function that
translates from the AST of the source language to the AST of the
target language. The `ec` compiler follows this overall structure. For
real compilers, a direct translation like the one mentioned above is
often too complicated. Most such compilers transform from the source
language `Lₛ` to `Lₜ` through a series of intermediate languages `Lₛ =
I₀,...,Iₙ = Lₜ`, each with their own ASTs.

To start the process, there is the _front-end_ of the compiler that
converts from the input string to the AST of the source Language
`Lₛ`. This is followed by a series of transformations between the
intermediate languages to get an AST of the target language
`Lₜ`. Finally the _back-end_ converts this AST to string.

## How to read the source code ?

The expression compiler `ec` performs a direct translation between the
expression language to the reverse polish notation. We suggest the
following reading order for the source code.

1. The ASTs of the source and target language are captured by the SML
   types [`Ast.Expr`][ast] and [Machine.Program][machine] respectively
   and are available inside the files [ast.sml][ast] and
   [machine.sml][machine]. Read these files first to understand how
   one captures the ASTs using standard ML `datatypes`.

2. The compiler is thus captured by the SML function `compile :
   Ast.Expr -> Machine.Program` and is given in
   [translate.sml][translate] file.

3. The front end is written using tools like [`mlyacc`][mlyacc] and
   [`mllex`][mllex] which takes the specification of the parser and
   lexer.

The suggested reading order is therefore [`ast.sml`][ast],
[`machine.sml`][machine], and [`translate.sml`][translate]. The actual
input to the [`mlyacc`][mlyacc] and [`mllex`][mllex] tool is files
[`expr.grm`][expr.grm] and [`expr.lex`][expr.lex], which you can skip
in the first reading.

[ast]: <ast.sml>
[machine]: <machine.sml>
[translate]: <translate.sml>
[expr.grm]: <expr.grm>
[expr.lex]: <expr.lex>
[mlyacc]: <http://mlton.org/MLYacc>
[mllex]: <http://mlton.org/MLLex>
[book]: <https://www.cs.princeton.edu/~appel/modern/> "Modern Compiler Implementation"
[mips]: <https://en.wikipedia.org/wiki/MIPS_architecture>
