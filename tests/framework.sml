(*
 * a very lightweight testing framework
 *)

structure Test =
   struct
      type 'a genidx = {eq: 'a Eq.t, show: 'a Show.t}
      type 'a testcase = {actual: 'a, expected: 'a}
      type ''a polyidx = {show: ''a Show.t}

      datatype 'a result = Pass of 'a testcase
                         | Fail of string * 'a testcase

      val genAssertEq: 'a genidx -> 'a testcase -> 'a result =
       fn {eq, show} =>
          fn (data as {actual, expected}) =>
             if eq (actual, expected)
                then Pass data
             else Fail ("expected: " ^ show expected ^
                        ", but got: " ^ show actual, data)

      val polyAssertEq: ''a polyidx -> ''a testcase -> ''a result =
       fn {show} => genAssertEq {eq=op =,show=show}

      fun collectResults assert (tests: 'a testcase list) = map assert tests

      fun showResult (Pass _) = ("pass", ".")
        | showResult (Fail (msg, _)) = ("FAIL " ^ msg, "F")

      fun runTestSuite assert v (name, tests) =
          let
             val (verbose, concise) =
                 ListPair.unzip (map showResult
                                     (collectResults assert tests))
          in
             (print ("[" ^ name ^ "] ")
             ; if v
               then print ("\n" ^ (concat (ExtList.interleave verbose "\n")))
               else print (concat concise)
             ; print "\n")
          end
   end

(* example use *)

(*
structure TestExample =
   struct
      structure IntShowEq =
         struct
            type t = int
            val show = Int.toString
            val eq = (op =)
         end

      structure Test = TestFn(structure Show = IntShowEq
                              structure Eq   = IntShowEq)
      open Test

      val tests =
         TGroup ("simple arith",
                 [TGroup ("literal", [Case ("zero", 0, 0)]),
                   Case ("plus", 1 + 1, 2),
                  Case ("sub" , 2 - 3, 0), (* failing test for demo *)
                  Case ("mult", 2 * 3, 6)]);

      fun main _ = (runTests true tests
                    ; OS.Process.success)
   end
*)
