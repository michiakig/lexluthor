# lexluthor

lexluthor is a library for building lexical analyzers, written in
Standard ML and primarily designed to explore how lexer generators
work "from the ground up", i.e. without using any existing tools (such
as lex or ML-Lex) or any existing regular expression libraries (such
as the one provided with SML/NJ).

## current status

Right now it's just a DFA-based regular expression matching engine,
without any front end or actual lexer component. Some of the code (the
NFA to DFA conversion code especially) is kind of bad, and needs to be
re-written with the goal of clarity and pedagogy in mind.

## Standard ML

It's written in Standard ML mostly because I wrote it while reading
[Andrew Appel's Modern Compiler Implementation in ML][0], but also
because I think ML is nice, and I know SML better than OCaml. As of
Jan 2013 it compiles under the latest releases of [SML/NJ \(110.75\)][1]
and [MLton \(20100608\)][2], but I haven't tested it with any other SML
compilers.

## tests

To run the tests under SML/NJ, execute the script `runTests.sh` from
`tests/`. Alternatively, compile with MLton and run with: `mlton
lexluthor.mlb ; ./lexluthor`.

[0]: http://www.cs.princeton.edu/~appel/modern/ml/ "Modern Compiler Implementation in ML"
[1]: http://www.smlnj.org/dist/working/110.75/index.html "latest SML/NJ"
[2]: http://mlton.org/Release20100608 "latest MLton"