(* entry point to run all tests *)
structure Main =
   struct
      fun main _ = (MatchTests.doTestRun true
                    ; RegExpTests.doTestRun true
                    ; SimpleLexerTests.doTestRun true
                    ; ListGraphTests.doTestRun true
                    ; GraphSearchTests.doTestRun true
                    ; OS.Process.success)
   end
