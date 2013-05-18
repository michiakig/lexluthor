(* tests for regular expression matching function only *)
structure MatchTests =
struct

open BasicRegExpSyntax
open Test

structure EmptyLexerSpec =
   struct
      type token = unit
      val tokens = [(Epsilon,())]
   end

structure LexLuthor = LexLuthorFn(EmptyLexerSpec: LEXER_SPEC)
val match = LexLuthor.match

val tests =
   let
      val a = Literal #"a"
      val b = Literal #"b"
      val assert = Test.polyAssertEq {show=Show.option (Show.sq Show.string)}
   in
      group ("regexps", assert,
             [{actual = match (a, "a"),              expected = SOME ("a", "")},
              {actual = match (Epsilon, "a"),        expected = SOME ("", "a")},
              {actual = match (Concat(a, b), "abc"), expected = SOME ("ab", "c")},
              {actual = match (Altern(a, b), "ab"),  expected = SOME ("a", "b")},
              {actual = match (Repeat a, "aaaab"),   expected = SOME ("aaaa", "b")}])
   end

end
