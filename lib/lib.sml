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

      fun replicate n x =
          let
             fun replicate' acc 0 = rev acc
               | replicate' acc n = replicate' (x::acc) (n-1)
          in
             replicate' [] n
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

structure IntShow: SHOW =
   struct
      type t = int
      val show = Int.toString
   end

structure IntEq: EQ =
   struct
      type t = int
      fun eq (x,y) =
          case Int.compare (x,y) of
              EQUAL => true
            | _ => false
   end

structure StringShow: SHOW =
   struct
      type t = string
      fun show x = "\"" ^ x ^ "\""
   end

structure StringEq: EQ =
   struct
      type t = string
      fun eq (x,y) =
          case String.compare (x,y) of
              EQUAL => true
            | _ => false
   end

functor PairShowFn(structure A: SHOW
                   structure B: SHOW): SHOW =
   struct
      type t = A.t * B.t
      fun show (a,b) = "(" ^ A.show a ^ "," ^ B.show b ^ ")"
   end

functor PairEqFn(structure A: EQ
                 structure B: EQ): EQ =
   struct
      type t = A.t * B.t
      fun eq ((a,b),(a',b')) = A.eq (a,a') andalso B.eq (b,b')
   end

functor SqEqFn(structure Eq: EQ) =
   struct
      local
         structure S = PairEqFn(structure A=Eq
                                structure B=Eq)
      in
         open S
      end
   end

functor SqShowFn(structure Show: SHOW) =
   struct
      local
         structure S = PairShowFn(structure A=Show
                                  structure B=Show)
      in
         open S
      end
   end

functor OptionShowFn(structure Show: SHOW): SHOW =
   struct
      type t = Show.t option
      fun show NONE = "NONE"
        | show (SOME x) = "SOME " ^ Show.show x
   end

functor OptionEqFn(structure Eq: EQ): EQ =
   struct
      type t = Eq.t option
      fun eq (NONE,NONE) = true
        | eq (NONE,SOME _) = false
        | eq (SOME _,NONE) = false
        | eq (SOME x,SOME y) = Eq.eq (x,y)
   end

functor ListShowFn(structure Show: SHOW): SHOW =
   struct
      type t = Show.t list
      fun show xs = "[" ^ concat (ExtList.interleave (map Show.show xs) ",") ^ "]"
   end

functor ListEqFn(structure Eq: EQ): EQ =
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
