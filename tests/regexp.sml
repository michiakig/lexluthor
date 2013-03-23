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
    ("desugar",
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

val assert = Test.genAssertEq {eq = eq, show = show}

fun doTestRun v = Test.runTestSuite assert v tests

end
