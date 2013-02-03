structure MatchTests =
struct

open BasicRegExpSyntax

structure LexerSpec =
   struct
      datatype token = Num | Id
      local
         (* binary numbers *)
         val num = Concat(Symbol #"1",Repeat(Altern(Symbol #"0",Symbol #"1")))
         (* identifiers *)
         val id = Repeat(Altern(Symbol #"a",Altern(Symbol #"b",Symbol #"c")))
      in
         val tokens = [(num, Num), (id, Id)]
      end
   end

structure LexLuthor = LexLuthorFn(LexerSpec: LEXER_SPEC)

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
            M.Case ("repeat", match(Repeat(a), "aaaab"), SOME ("aaaa", "b"))])
   end

structure LexerShowEq =
   struct
      type t = (LexerSpec.token * string) list

      val eq = (op =)

      fun show lst =
         let
            fun s (LexerSpec.Id, str) = "(" ^ str ^ ",Id)"
              | s (LexerSpec.Num, str) = "(" ^ str ^ ",Num)"
            val strings = ExtList.interleave (List.map s lst) ","
         in
            "[" ^ (String.concat strings) ^ "]"
         end
   end

structure L = TestFn (structure Show = LexerShowEq
                      structure Eq = LexerShowEq)

val lex = LexLuthor.lex

val lexerTests =
   L.TGroup
       ("lexer",
        [L.Case ("single num", lex "1010", [(LexerSpec.Num, "1010")]),
         L.Case ("single id", lex "ab", [(LexerSpec.Id, "ab")]),
         L.Case ("mixed", lex "100100abab", [(LexerSpec.Num, "100100"),
                                             (LexerSpec.Id, "abab")])])

fun main _ = (M.runTests true regexTests
              ; L.runTests true lexerTests
              ; OS.Process.success)

end
