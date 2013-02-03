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

structure MatchShowEq =
   struct
      type t = (string * string) option
      val eq = (op =)
      fun show NONE = "NONE"
        | show (SOME (x, y)) = "SOME (\"" ^ x ^ "\",\"" ^ y ^ "\")"
   end
structure M = TestFn (structure Show = MatchShowEq
                      structure Eq = MatchShowEq)
val match = LexLuthor.match
val regexTests =
   let
      val a = Symbol #"a"
      val b = Symbol #"b"
   in
      M.TGroup
          ("regexps",
           [M.Case ("symbol", match(a, "a"), SOME ("a","")),
            M.Case ("epsilon", match(Epsilon, "a"), SOME ("","a")),
            M.Case ("concat", match(Concat(a, b), "abc"), SOME ("ab","c")),
            M.Case ("altern", match(Altern(a, b), "ab"), SOME ("a", "b")),
            M.Case ("repeat", match(Repeat(a), "aaaab"), SOME ("aaaa", "b"))
          ])
   end

fun doTestRun v = M.runTests v regexTests

end
