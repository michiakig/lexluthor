(* tests for regular expression matching function only *)
structure MatchTests =
struct

open BasicRegExpSyntax

structure EmptyLexerSpec =
   struct
      type token = unit
      val tokens = [(Epsilon,())]
   end

structure LexLuthor = LexLuthorFn(EmptyLexerSpec: LEXER_SPEC)
val match = LexLuthor.match

val regexTests =
   let
      val a = Symbol #"a"
      val b = Symbol #"b"
   in
      ("regexps",
       [{actual = match (a, "a"),              expected = SOME ("a", "")},
        {actual = match (Epsilon, "a"),        expected = SOME ("", "a")},
        {actual = match (Concat(a, b), "abc"), expected = SOME ("ab", "c")},
        {actual = match (Altern(a, b), "ab"),  expected = SOME ("a", "b")},
        {actual = match (Repeat a, "aaaab"),   expected = SOME ("aaaa", "b")}])
   end

val assert = Test.polyAssertEq {show=Show.option (Show.sq Show.string)}

fun doTestRun v = Test.runTestSuite assert v regexTests

end
