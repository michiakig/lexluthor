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

structure HigherOrder =
   struct

      fun fixedPoint (init, step, compare) =
         let
            fun loop (old, new) =
               if compare (old, new)
                  then old
               else loop (new, step new)
         in
            loop (init, step init)
         end

      (*
      SICP 1.3.3 http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.3

      fun sqrt x =
         let
            fun average (x, y) = (x + y) / 2.0
            fun step y = average (y, x / y)
            fun compare (x, y) = Real.abs (x - y) < 0.00001
         in
            fixedPoint (1.0, step, compare)
         end
      *)

      end
