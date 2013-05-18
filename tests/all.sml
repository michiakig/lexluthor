(* entry point to run all tests *)
structure Main =
   struct
      val tests = [
         MatchTests.tests
         , RegExpTests.tests
         , SimpleLexerTests.tests
         , ListGraphTests.tests
         , GraphSearchTests.tests
      ]

      fun main (_, []) = (List.app (fn t => Test.runTestSuite (false, t)) tests
                         ; OS.Process.success)
        | main (_, v :: _) =
          (List.app (fn t => Test.runTestSuite (v = "verbose", t)) tests
          ; OS.Process.success)
   end
