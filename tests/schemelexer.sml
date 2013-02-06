structure SchemeLexerTests(* : TESTS *) =
struct

open BasicRegExpSyntax

structure SchemeLexerSpec =
   struct

      datatype token = LPAREN | RPAREN | IDENT | WSPACE
      local
         val regexes = [("\\(",LPAREN),
                        ("\\)",RPAREN),
                        ("[a-z][a-z0-9]*",IDENT),
                        (" ",WSPACE)]
         fun parse (re,tok) = (unsafeDesugar re, tok)
      in
         val tokens = map parse regexes
      end
   end

structure LexLuthor = LexLuthorFn(SchemeLexerSpec)

open SchemeLexerSpec

fun doTestRun v =
    let
       val lexer = LexLuthor.mkLexer ()
       val tokens = LexLuthor.lex (lexer, "(foo bar (baz) qux)")

       fun show (LPAREN, _) = "LPAREN"
         | show (RPAREN, _) = "RPAREN"
         | show (IDENT, id) = "IDENT:" ^ id
         | show (WSPACE, _) = ""

       val strings = map show tokens
       fun p s = if size s > 0
                    then print (s ^ " ")
                 else ()
    in
       (List.app p strings; print "\n")
    end                                               
end
