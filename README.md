# lexluthor

lexluthor is a library for building lexical analyzers, written in
Standard ML and primarily designed to explore how lexer generators
work "from the ground up", i.e. without using any existing tools (such
as ML-Lex) or any existing regular expression matching engines (such
as the one provided with SML/NJ).

## a library, not a lexer generator

A front-end is not included, so there's no parser for regular
expressions, although the one from SML/NJ's library can be
used. Furthermore there is no code generation: regular expressions are
translated to NFAs and then to DFAs in memory at start up. This
greatly reduces the complexity of the code, but makes it inappropriate
for real world use.

## current status

The core functionality of the lexer is complete, albeit pretty
rough. Some of the code (the NFA to DFA conversion code especially) is
kind of bad, and I'd like to re-write it to make it more
readable. It's also unbearably slow for anything interesting, due to
the naive list-based implementation of NFAs (NFA to DFA conversion for
a classic identifier regex like `[a-z][a-z0-9]*` takes ~23s with
SML/NJ, ~6s with MLton)

## Standard ML

It's written in Standard ML mostly because I wrote it while reading
[Andrew Appel's Modern Compiler Implementation in ML][0], but also
because I think ML is nice, and I know SML better than OCaml. As of
Jan 2013 it compiles under the latest releases of [SML/NJ \(110.75\)][1]
and [MLton \(20100608\)][2], but I haven't tested it with any other SML
compilers.

## tests

To build and test execute `scripts/runTests.sh $compiler $test` where
`$compiler` is either `smlnj` or `mlton`, and `$test` is a path to a
`.cm` or `.mlb` file (such as `tests/all.cm`).

[0]: http://www.cs.princeton.edu/~appel/modern/ml/ "Modern Compiler Implementation in ML"
[1]: http://www.smlnj.org/dist/working/110.75/index.html "latest SML/NJ"
[2]: http://mlton.org/Release20100608 "latest MLton"
