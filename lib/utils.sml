(* extensions to the standard basis and SML/NJ lib *)

structure ExtList =
   struct

      fun interleave l i =
         let
            fun recur [] acc = rev acc
              | recur (x :: []) acc = recur [] (x :: acc)
              | recur (x :: xs) acc = recur xs (i :: x :: acc)
         in
            recur l []
         end

      fun allEq [] = true
        | allEq l = List.all (fn x => x = hd l) l

   end

structure Pair =
   struct
      fun first (x,_) = x
      fun second (_,y) = y
   end

functor ExtOrdMapFn(Map: ORD_MAP) =
   struct
      open Map
      fun unsafeFind m k = Option.valOf(Map.find(m, k))
      fun keys m = List.map Pair.first (Map.listItemsi m)
   end
