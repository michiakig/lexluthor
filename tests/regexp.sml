structure RegExpTests =
struct

open BasicRegExpSyntax

(* Alternation can be in any order *)
fun eq (Altern (a,b), Altern (c,d)) = eq (a,c) andalso eq (b,d) orelse
                                      eq (a,d) andalso eq (b,c)
  | eq (r1,r2) = r1 = r2

fun show Epsilon = "Epsilon"
  | show (Literal ch) = "Literal " ^ Char.toString ch
  | show (Concat (a, b)) = "Concat (" ^ show a ^ ", " ^ show b ^ ")"
  | show (Altern (a, b)) = "Altern (" ^ show a ^ ", " ^ show b ^ ")"
  | show (Repeat a) = "Repeat (" ^ show a ^ ")"

val d = unsafeDesugar

val tests =
    Test.concat [
    Test.group
       ("desugar", Test.genAssertEq {eq = eq, show = show},
        [{actual = d "a",      expected = Literal #"a"},
         {actual = d "a*",     expected = Repeat(Literal #"a")},
         {actual = d "a|b",    expected = Altern(Literal #"a", Literal #"b")},
         {actual = d "ab",     expected = Concat(Literal #"a", Literal #"b")},
         {actual = d "[abc]",  expected = Altern(Literal #"a", Altern(Literal #"c", Literal #"b"))},
         {actual = d "a+",     expected = Altern(Literal #"a", Repeat(Literal #"a"))},
         {actual = d "a?",     expected = Altern(Literal #"a", Epsilon)},
         {actual = d "(a|b)*", expected = Repeat(Altern(Literal #"a", Literal #"b"))},
         {actual = d "(ab)?",  expected = Altern(Concat(Literal #"a", Literal #"b"),Epsilon)},
         {actual = d "(ab)*",  expected = Repeat(Concat(Literal #"a", Literal #"b"))}])
    , Test.group ("re.size", Test.polyAssertEq
                                {show = fn {states, edges} =>
                                           "{states=" ^ Int.toString states ^
                                           ",edges=" ^ Int.toString edges ^ "}"},
                  [{actual = size Epsilon,                             expected = {states = 2, edges = 1}},
                   {actual = size (Literal #"a"),                       expected = {states = 2, edges = 1}},
                   {actual = size (Repeat (Literal #"a")),              expected = {states = 2, edges = 3}},
                   {actual = size (Altern (Literal #"a", Literal #"b")), expected = {states = 6, edges = 6}},
                   {actual = size (Concat (Literal #"a", Literal #"b")), expected = {states = 4, edges = 3}}
    ])]

end
