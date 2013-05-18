structure SimpleLexerTests =
struct

open BasicRegExpSyntax

(* simple spec with regexp ast only *)
structure SimpleLexerSpec =
   struct
      datatype token = Num | Id | WSpace
      local
         (* binary numbers *)
         val num = Concat(Altern(Symbol #"0",Symbol #"1"),
                          Repeat(Altern(Symbol #"0",Symbol #"1")))
         (* identifiers consisting only of a,b,c *)
         val id = Repeat(Altern(Symbol #"a",Altern(Symbol #"b",Symbol #"c")))
         val whitespace = Repeat(Altern(Symbol #" ",Symbol #"\n"))
      in
         val tokens = [(num,Num),(id,Id),(whitespace, WSpace)]
      end
   end

structure SimpleLexLuthor = LexLuthorFn(SimpleLexerSpec)

fun showToken SimpleLexerSpec.Num = "Num"
  | showToken SimpleLexerSpec.Id = "Id"

local
   val lexer = SimpleLexLuthor.mkLexer ()
in
   fun lex s = SimpleLexLuthor.lex (lexer, s)
end

local
   open Show
   open Test
in
   val assert = Test.polyAssertEq {show=(list (triple (showToken, string, int)))}
   val tests =
       group ("simple lexer", assert,
              [
                {actual=lex "0",       expected=[(SimpleLexerSpec.Num, "0", 1)]},
                {actual=lex "1",       expected=[(SimpleLexerSpec.Num, "1", 1)]},
               {actual=lex "1010",       expected=[(SimpleLexerSpec.Num, "1010", 1)]},
               {actual=lex "ab",         expected=[(SimpleLexerSpec.Id, "ab", 1)]},
               {actual=lex "100100abab", expected=[(SimpleLexerSpec.Num, "100100", 1),
                                                   (SimpleLexerSpec.Id, "abab", 1)]},

               {actual=lex "100 011\nab cb",
                expected=[(SimpleLexerSpec.Num, "100", 1),
                          (SimpleLexerSpec.WSpace, " ", 1),
                          (SimpleLexerSpec.Num, "011", 1),
                          (SimpleLexerSpec.WSpace, "\n", 2),
                          (SimpleLexerSpec.Id, "ab", 2),
                          (SimpleLexerSpec.WSpace, " ", 2),
                          (SimpleLexerSpec.Id, "cb", 2)]}

             ])
end

end
