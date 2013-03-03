(* typeclass-style signatures, related boilerplate *)

signature SHOW =
   sig
      type t
      val show : t -> string
   end

signature EQ =
   sig
      type t
      val eq : t * t -> bool
   end

structure Int =
   struct
      open Int
      type t = int
      val show = toString
      fun eq (x,y) =
          case Int.compare (x,y) of
              EQUAL => true
            | _ => false
   end

functor OptionShowFn(Show: SHOW) =
   struct
      type t = Show.t option
      fun show NONE = "NONE"
        | show (SOME x) = "SOME " ^ Show.show x
   end

functor OptionEqFn(Eq: EQ) =
   struct
      type t = Eq.t
      fun eq (NONE,NONE) = true
        | eq (NONE,SOME _) = false
        | eq (SOME _,NONE) = false
        | eq (SOME x,SOME y) = Eq.eq (x,y)
   end

functor ListShowFn(Show: SHOW) =
   struct
      type t = Show.t list
      fun show xs = "[" ^ concat (ExtList.interleave (map Show.show xs) ",") ^ "]"
   end

functor ListEqFn(Eq: EQ) =
   struct
      type t = Eq.t
      val eq = ListPair.allEq Eq.eq
   end
