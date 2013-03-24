structure TigerLexerTests: TESTS =
struct

open BasicRegExpSyntax

structure TigerLexerSpec =
   struct
      type token = string
      local
         val strs = ["type",
                     "var",
                     "function",
                     "break",
                     "of",
                     "end",
                     "in",
                     "nil",
                     "let",
                     "do",
                     "to",
                     "for",
                     "while",
                     "else",
                     "then",
                     "if",
                     "array",
                     ":=",
                     "\\|",
                     "&",
                     ">=",
                     ">",
                     "<=",
                     "<",
                     "<>",
                     "=",
                     "/",
                     "\\*",
                     "-",
                     "\\+",
                     "\\.",
                     "{",
                     "}",
                     "[",
                     "]",
                     "\\(",
                     "\\)",
                     ";",
                     ":",
                     ","]
      in
         val tokens = map (fn x => (x,x)) strs
      end
   end


end
