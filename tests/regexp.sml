structure RegExpTests =
struct

open BasicRegExpSyntax

(* Alternation can be in any order *)
fun eq (Altern (a,b), Altern (c,d)) = eq (a,c) andalso eq (b,d) orelse
                                      eq (a,d) andalso eq (b,c)
  | eq (r1,r2) = r1 = r2

fun show Epsilon = "Epsilon"
  | show (Symbol ch) = "Symbol " ^ Char.toString ch
  | show (Concat (a, b)) = "Concat (" ^ show a ^ ", " ^ show b ^ ")"
  | show (Altern (a, b)) = "Altern (" ^ show a ^ ", " ^ show b ^ ")"
  | show (Repeat a) = "Repeat (" ^ show a ^ ")"

val d = unsafeDesugar

val tests =
    Test.concat [
    Test.group
       ("desugar", Test.genAssertEq {eq = eq, show = show},
        [{actual = d "a",      expected = Symbol #"a"},
         {actual = d "a*",     expected = Repeat(Symbol #"a")},
         {actual = d "a|b",    expected = Altern(Symbol #"a", Symbol #"b")},
         {actual = d "ab",     expected = Concat(Symbol #"a", Symbol #"b")},
         {actual = d "[abc]",  expected = Altern(Symbol #"a", Altern(Symbol #"c", Symbol #"b"))},
         {actual = d "a+",     expected = Altern(Symbol #"a", Repeat(Symbol #"a"))},
         {actual = d "a?",     expected = Altern(Symbol #"a", Epsilon)},
         {actual = d "(a|b)*", expected = Repeat(Altern(Symbol #"a", Symbol #"b"))},
         {actual = d "(ab)?",  expected = Altern(Concat(Symbol #"a", Symbol #"b"),Epsilon)},
         {actual = d "(ab)*",  expected = Repeat(Concat(Symbol #"a", Symbol #"b"))}])
    , Test.group ("re.size", Test.polyAssertEq
                                {show = fn {states, edges} =>
                                           "{states=" ^ Int.toString states ^
                                           ",edges=" ^ Int.toString edges ^ "}"},
                  [{actual = size Epsilon,                             expected = {states = 2, edges = 1}},
                   {actual = size (Symbol #"a"),                       expected = {states = 2, edges = 1}},
                   {actual = size (Repeat (Symbol #"a")),              expected = {states = 2, edges = 3}},
                   {actual = size (Altern (Symbol #"a", Symbol #"b")), expected = {states = 6, edges = 6}},
                   {actual = size (Concat (Symbol #"a", Symbol #"b")), expected = {states = 4, edges = 3}}
    ])]

fun doTestRun v = Test.runTestSuite (v, tests)

end
