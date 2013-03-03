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

functor OptionShowFn(Show: SHOW): SHOW =
   struct
      type t = Show.t option
      fun show NONE = "NONE"
        | show (SOME x) = "SOME " ^ Show.show x
   end

functor OptionEqFn(Eq: EQ): EQ =
   struct
      type t = Eq.t option
      fun eq (NONE,NONE) = true
        | eq (NONE,SOME _) = false
        | eq (SOME _,NONE) = false
        | eq (SOME x,SOME y) = Eq.eq (x,y)
   end

functor ListShowFn(Show: SHOW): SHOW =
   struct
      type t = Show.t list
      fun show xs = "[" ^ concat (ExtList.interleave (map Show.show xs) ",") ^ "]"
   end

functor ListEqFn(Eq: EQ): EQ =
   struct
      type t = Eq.t list
      val eq = ListPair.allEq Eq.eq
   end

functor MapShowFn(structure Map: ORD_MAP
                  structure K: SHOW
                  structure V: SHOW
                  sharing type Map.Key.ord_key = K.t): SHOW =
   struct
      type t = V.t Map.map
      fun show m =
          let
             val items = Map.listItemsi m
             fun showPair (k, v) = "(" ^ K.show k ^ "," ^ V.show v ^ ")"
             val strs = map showPair items
          in
             "{" ^ String.concat (ExtList.interleave strs ",") ^ "}"
          end
   end

functor SetShowFn(structure Set: ORD_SET
                  structure Show: SHOW
                  sharing type Set.Key.ord_key = Show.t): SHOW =
   struct
      type t = Set.set
      fun show s =
          let
             val items = Set.listItems s
             val strs = map Show.show items
          in
             "{" ^ String.concat (ExtList.interleave strs ",") ^ "}"
          end
   end
