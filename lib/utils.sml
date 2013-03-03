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

      (* returns true if all elements are equal (equality types only) *)
      fun allEq [] = true
        | allEq l = List.all (fn x => x = hd l) l

      (* accumulates results of f called with progressively larger prefixes of xs *)
      fun maptake f xs n =
          let
             fun maptake' (acc, n') =
                 if n' >= n
                 then rev acc
                 else maptake' (f (List.take (xs, n')) :: acc, n' + 1)
          in
             maptake' ([], 1)
          end

      (* like foldl, but instead calls f with progressively longer prefixes of xs *)
      val foldtake: ('a list * 'b -> 'b) * 'b * 'a list -> 'b =
       fn (f, init, xs) =>
          let
             val len = length xs
             fun foldtake' (acc, n) =
                 if n > len
                    then acc
                 else (foldtake' (f (List.take (xs, n), acc), n + 1))
          in
             foldtake' (init, 1)
          end

      (* like foldl, but instead calls f with progressively shorter suffixes of xs *)
      val folddrop: ('a list * 'b -> 'b) * 'b * 'a list -> 'b =
       fn (f, init, xs) =>
          let
             val len = length xs
             fun folddrop' (acc, n) =
                 if n = len
                    then acc
                 else (folddrop' (f (List.drop (xs, n), acc), n + 1))
          in
             folddrop' (init, 0)
          end

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

functor MapShowFn(structure Map: ORD_MAP
                  val showKey: Map.Key.ord_key -> string) =
   struct
      fun showMap showVal m =
          let
             val items = Map.listItemsi m
             fun showPair (k, v) = "(" ^ showKey k ^ "," ^ showVal v ^ ")"
             val strs = map showPair items
          in
             "{" ^ String.concat (ExtList.interleave strs ",") ^ "}"
          end
   end

functor SetShowFn(structure Set: ORD_SET
                  val show: Set.item -> string) =
   struct
      fun showSet s =
          let
             val items = Set.listItems s
             val strs = map show items
          in
             "{" ^ String.concat (ExtList.interleave strs ",") ^ "}"
          end
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
