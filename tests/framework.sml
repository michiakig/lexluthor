(*
 * a very lightweight testing framework
 *)

signature TESTS =
   sig
      val doTestRun: bool -> unit
   end

functor TestFn(structure Show: SHOW
               structure Eq: EQ
               sharing type Show.t = Eq.t) =
   struct
      open ExtList
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

      fun showResults result =
          let
             fun show' d x = concat (replicate d " ") ^ show x
             fun showResults' depth (Pass (Case (name, _, _))) =
                 let
                    val verbose = concat (replicate depth " ") ^ "Passed: " ^ name
                    val concise = "."
                 in
                    (verbose, concise)
                 end
               | showResults' depth (Fail (Case (name, actual, expected))) =
                 let
                    val verbose = concat (replicate depth " ") ^
                                  "FAILED: " ^ name ^
                                  ", expected: " ^ (show expected) ^
                                  ", but actually got: " ^ (show actual)
                    val concise = "F"
                 in
                    (verbose, concise)
                 end
               | showResults' depth (RGroup (name, results)) =
                 let
                    val results = map (showResults' (depth+1)) results
                    val verbose = map Pair.first results
                    val concise = map Pair.second results
                 in
                    (concat (replicate depth " ") ^
                     concat (interleave (name :: verbose) "\n"),
                     name ^ ": " ^ concat concise)
                 end
          in
             showResults' 0 result
          end

      fun runTests verbose tests =
         let
            val (v, c) = (showResults (test tests))
         in
            (if verbose
                then print v
             else print c
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
