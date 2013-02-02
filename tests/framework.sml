(*
 * a very lightweight testing framework
 *)

functor TestFn(structure Show: SHOW
               structure Eq: EQ
               sharing type Show.t = Eq.t) =
   struct
      open Show
      open Eq
      type t = Show.t

      datatype test =
         Case of string * t * t
       | TGroup of string * test list

      datatype result =
         Pass of test
       | Fail of test
       | RGroup of string * result list

      fun test (c as Case (_, actual, expected)) =
         if eq (actual, expected)
            then Pass c
         else Fail c
       | test (g as TGroup (name, tests)) =
          RGroup (name, map test tests)

      fun showResults (Pass (Case (name, _, _))) =
         ("Passed: " ^ name, ".")
       | showResults (Fail (Case (name, actual, expected))) =
          ("FAILED: " ^ name ^
           " expected: " ^ (show expected) ^
           ", but actually got: " ^ (show actual),
           "F")
        | showResults (RGroup (name, results)) =
           let
              val results = map showResults results
              val verbose = map Pair.first results
              val concise = map Pair.second results
           in
              (String.concat (ExtList.interleave (name :: verbose) "\n"),
               name ^ ": " ^ (String.concat concise)) 
           end

      fun runTests verbose tests =
         let
            val (v, c) = (showResults (test tests))
         in
            (if verbose
                then print v
             else print c
             ; print "\ndone.\n")
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
                 [Case ("plus", 1 + 1, 2),
                  Case ("sub" , 2 - 3, 0), (* failing test for demo *)
                  Case ("mult", 2 * 3, 6)]);

      fun main _ = (runTests true tests
                    ; OS.Process.success)
   end
*)
