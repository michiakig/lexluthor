(* tests for regular expression matching function only *)
structure MatchTests: TESTS =
struct

open BasicRegExpSyntax

structure EmptyLexerSpec =
   struct
      type token = unit
      val tokens = [(Epsilon,())]
   end

structure LexLuthor = LexLuthorFn(EmptyLexerSpec: LEXER_SPEC)

structure MatchShow = OptionShowFn(structure Show=SqShowFn(structure Show=StringShow))
structure MatchEq = OptionEqFn(structure Eq=SqEqFn(structure Eq=StringEq))
structure M = TestFn (structure Show = MatchShow
                      structure Eq = MatchEq)
open M
val match = LexLuthor.match

val regexTests =
   let
      val a = Symbol #"a"
      val b = Symbol #"b"
   in
      M.TGroup
          ("regexps",
           [M.Case ("symbol", {actual=match(a, "a"), expect=SOME ("a","")}),

            M.Case ("epsilon",
                    {actual=match(Epsilon, "a"), expect=SOME ("","a")}),

            M.Case ("concat", {actual=match(Concat(a, b), "abc"),
                               expect=SOME ("ab","c")}),

            M.Case ("altern", {actual=match(Altern(a, b), "ab"),
                               expect=SOME ("a", "b")}),

            M.Case ("repeat", {actual=match(Repeat(a), "aaaab"),
                               expect=SOME ("aaaa", "b")})
          ])
   end

fun doTestRun v = M.runTests v regexTests

end
