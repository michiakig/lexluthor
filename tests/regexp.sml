structure RegExpTests: TESTS =
struct

open BasicRegExpSyntax

structure RegExpShowEq =
   struct
      type t = syntax
      (* Alternation can be in any order *)
      fun eq (Altern (a,b), Altern (c,d)) = eq (a,c) andalso eq (b,d) orelse
                                            eq (a,d) andalso eq (b,c)
        | eq (r1,r2) = r1 = r2
      fun show Epsilon = "Epsilon"
        | show (Symbol ch) = "Symbol " ^ Char.toString ch
        | show (Concat (a, b)) = "Concat (" ^ show a ^ ", " ^ show b ^ ")"
        | show (Altern (a, b)) = "Altern (" ^ show a ^ ", " ^ show b ^ ")"
        | show (Repeat a) = "Repeat (" ^ show a ^ ")"
   end

structure R = TestFn (structure Show = RegExpShowEq
                      structure Eq = RegExpShowEq)

val d = unsafeDesugar

val tests =
    R.TGroup
       ("desugar",
        [R.Case ("char", {actual = d "a", expect = Symbol #"a"}),
         R.Case ("star", {actual = d "a*", expect = Repeat(Symbol #"a")}),
         R.Case ("alt", {actual = d "a|b", expect = Altern(Symbol #"a", Symbol #"b")}),
         R.Case ("concat", {actual = d "ab", expect = Concat(Symbol #"a", Symbol #"b")}),

         R.Case ("matchset", {actual = d "[abc]",
                              expect = Altern(Symbol #"a", Altern(Symbol #"c", Symbol #"b"))}),

         R.Case ("plus", {actual = d "a+",
                          expect = Altern(Symbol #"a", Repeat(Symbol #"a"))}),

         R.Case ("option", {actual = d "a?",
                            expect = Altern(Symbol #"a", Epsilon)}),

         R.Case ("groups1", {actual = d "(a|b)*",
                             expect = Repeat(Altern(Symbol #"a", Symbol #"b"))}),

         R.Case ("groups2", {actual = d "(ab)?",
                             expect = Altern(Concat(Symbol #"a", Symbol #"b"),Epsilon)}),

         R.Case ("groups3", {actual=d "(ab)*", expect=Repeat(Concat(Symbol #"a", Symbol #"b"))})])

fun doTestRun v = R.runTests v tests

end
