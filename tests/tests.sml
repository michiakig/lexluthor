structure MatchTests =
struct

structure LexerSpec: LEXER_SPEC =
   struct
      open Regexp
      datatype token = Num | Id
      val tokens = [(Repeat(Altern(Altern(Symbol #"z", Symbol #"x"), Symbol #"y")),Num)
                   ,(Repeat(Altern(Altern(Symbol #"a", Symbol #"b"), Symbol #"c")),Id)]
   end

structure LexLuthor = LexLuthorFn(LexerSpec)

structure MatchShowEq =
   struct
      type t = (string * string) option

      fun eq (NONE, NONE) = true
        | eq (SOME (x1,x2), SOME (y1,y2)) = x1 = y1 andalso x2 = y2
        | eq _ = false

      fun show NONE = "NONE"
        | show (SOME (x, y)) = "SOME (\"" ^ x ^ "\",\"" ^ y ^ "\")"
   end

structure Test = TestFn (structure Show = MatchShowEq
                         structure Eq = MatchShowEq)

open Test
open LexLuthor

val tests =
   let
      val a = Symbol #"a"
      val b = Symbol #"b"
   in
      TGroup
          ("regexps",
           [Case ("symbol", match(a, "a"), SOME ("a","")),
            Case ("concat", match(Concat(a, b), "abc"), SOME ("ab","c")),
            Case ("altern", match(Altern(a, b), "ab"), SOME ("a", "b")),
            Case ("repeat", match(Repeat(a), "aaaab"), SOME ("aaaa", "b"))])
   end


fun main _ = (runTests true tests
              ; OS.Process.success)

end
