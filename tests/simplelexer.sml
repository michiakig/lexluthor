structure SimpleLexerTests: TESTS =
struct

open BasicRegExpSyntax

(* simple spec with regexp ast only *)
structure SimpleLexerSpec =
   struct
      datatype token = Num | Id
      local
         (* binary numbers *)
         val num = Concat(Symbol #"1",Repeat(Altern(Symbol #"0",Symbol #"1")))
         (* identifiers consisting only of a,b,c *)
         val id = Repeat(Altern(Symbol #"a",Altern(Symbol #"b",Symbol #"c")))
      in
         val tokens = [(num,Num),(id,Id)]
      end
   end

structure SimpleLexLuthor = LexLuthorFn(SimpleLexerSpec)

structure SimpleLexerShowEq =
   struct
      type t = (SimpleLexerSpec.token * string) list
      val eq = (op =)
      fun show lst =
         let
            fun s (SimpleLexerSpec.Id, str) = "(" ^ str ^ ",Id)"
              | s (SimpleLexerSpec.Num, str) = "(" ^ str ^ ",Num)"
            val strings = ExtList.interleave (List.map s lst) ","
         in
            "[" ^ (String.concat strings) ^ "]"
         end
   end

structure S = TestFn (structure Show = SimpleLexerShowEq
                      structure Eq = SimpleLexerShowEq)

val lex = SimpleLexLuthor.lex

val lexerTests =
   S.TGroup
       ("simple lexer",
        [S.Case ("single num", lex "1010", [(SimpleLexerSpec.Num, "1010")]),
         S.Case ("single id", lex "ab", [(SimpleLexerSpec.Id, "ab")]),
         S.Case ("mixed", lex "100100abab", [(SimpleLexerSpec.Num, "100100"),
                                             (SimpleLexerSpec.Id, "abab")])])

fun doTestRun v = S.runTests v lexerTests

end
