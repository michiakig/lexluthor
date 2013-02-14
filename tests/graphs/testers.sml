structure IntOptionShowEq =
   struct
      type t = int option
      fun show NONE = "NONE"
        | show (SOME x) = "SOME " ^ Int.toString x
      val eq = op =
   end

structure IntOptionTester = TestFn(structure Show = IntOptionShowEq
                                   structure Eq = IntOptionShowEq)
