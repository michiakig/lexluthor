(*
 * a very lightweight testing framework
 *)

structure Test =
   struct
      (* general index, requiring an equality predicate and a show fn  *)
      type 'a genidx = {eq: 'a Eq.t, show: 'a Show.t}
      (* eqtype index, requiring only a show fn *)
      type ''a polyidx = {show: ''a Show.t}

      type 'a testcase = {actual: 'a, expected: 'a}
      datatype result = Pass | Fail of string
      type 'a assert = 'a testcase -> result

      (* a suite is a list of thunks returning (name, verbose, concise) *)
      (* note suites can be heterogeneous, ie contain test cases of different types *)
      type testsuite = (unit -> string * string * string) list

      (* assert fn constructors *)
      val genAssertEq: 'a genidx -> 'a assert =
       fn {eq, show} =>
          fn (data as {actual, expected}) =>
             if eq (actual, expected)
                then Pass
             else Fail ("expected: " ^ show expected ^ ", but got: " ^ show actual)

      val polyAssertEq: ''a polyidx -> ''a assert =
       fn {show} => genAssertEq {eq=op =,show=show}

      (* applies an assertion to a test case *)
      val apply: string * 'a assert * 'a testcase -> string * string * string =
          fn (name, assert, testcase) =>
             case assert testcase of
                 Pass => (name, "pass", ".")
               | Fail msg => (name, "FAIL " ^ msg, "F")

      val concat: testsuite list -> testsuite = List.concat

      (* make a singleton test suite *)
      val single: string * 'a assert * 'a testcase -> testsuite =
       fn t => [fn () => apply t]

      (* make a test suite from a group of testcases *)
      val group: string * 'a assert * 'a testcase list -> testsuite =
       fn (name, assert, cases) =>
          map (fn c => fn () => apply (name, assert, c)) cases

      local
         val bool = genAssertEq {eq = Eq.bool, show = Show.bool}
      in
         (* makes test suite from boolean assertions... *)
         val assertTrue: string * bool -> testsuite =
             fn (name, actual) =>
                [fn () => apply (name, bool, {actual = actual, expected = true})]

         val assertFalse: string * bool -> testsuite =
             fn (name, actual) =>
                [fn () => apply (name, bool, {actual = actual, expected = false})]

         (* ... and lists of boolean assertions *)
         val assertAllTrue: string * bool list -> testsuite =
          fn (name, actuals) => concat (map (fn a => assertTrue (name, a)) actuals)

         val assertAllFalse: string * bool list -> testsuite =
          fn (name, actuals) => concat (map (fn a => assertFalse (name, a)) actuals)
      end

      (* actually run a test suite, printing either verbose or concise results *)
      val runTestSuite: bool * testsuite -> unit =
       fn (verbose, tests) =>
          let
             val results = map (fn f => f ()) tests
             fun p (n, v, c) =
                 if verbose
                    then print ("[" ^ n ^ "] " ^ v ^ "\n")
                 else print c
          in
           (List.app p results; print "\n")
          end

   end

(* example use *)
(*
structure Main =
   struct
      local
         open Test
         val int = genAssertEq {eq = Eq.int, show = Show.int} ;
         val tests = [single ("plus", int, {actual = 1 + 1, expected = 2}),
                      group ("mult", int,
                             [{actual = 1 * 1, expected = 1},
                              {actual = 2 * 4, expected = 6}]),
                      assertTrue ("lt", 2 < 3),
                      assertFalse ("gt", 2 > 1),
                      assertAllTrue ("eq",
                                     [1 = 1,
                                      2 = 2,
                                      3 = 4])]
      in
         fun main _ = (runTestSuite (false, concat tests);
                       OS.Process.success)
      end
   end
*)
