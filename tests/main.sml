(* entry point to run all tests *)
structure Main =
   struct
      fun main _ = (MatchTests.doTestRun false
                    ; RegExpTests.doTestRun false
                    ; SimpleLexerTests.doTestRun false
                    ; OS.Process.success)
   end
