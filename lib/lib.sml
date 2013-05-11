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

structure Eq =
   struct
      type 'a t = 'a * 'a -> bool
      val unit: unit t = op =
      val int: int t = op =
      val word: word t = op =
      val char: char t = op =
      val real: real t = Real.==
      val string: string t = op =
      val bool: bool t = op =
      val list: 'a t -> 'a list t = fn eq => ListPair.allEq eq
      val option: 'a t -> 'a option t =
       fn eq =>
          fn (NONE, SOME _) => false
           | (SOME _, NONE) => false
           | (SOME x, SOME y) => eq (x, y)
           | _ => true
      val pair: 'a t * 'b t -> ('a * 'b) t =
       fn (eqa,eqb) => fn ((a,b),(a',b')) => eqa (a,a') andalso eqb (b,b')
      val sq: 'a t -> ('a * 'a) t =
       fn eq => fn ((a,a'),(a'',a''')) => eq (a,a'') andalso eq (a',a''')
   end

structure Show =
   struct
      type 'a t = 'a -> string
      val unit: unit t = fn _ => "()"
      val int: int t = Int.toString
      val word: word t = Word.toString
      val char: char t = Char.toString
      val real: real t = Real.toString
      val string: string t = fn s => "\"" ^ s ^ "\""
      val bool: bool t = Bool.toString
      val list: 'a t -> 'a list t =
       fn show => fn xs => "[" ^ concat (ExtList.interleave (map show xs) ",") ^ "]"
      val option: 'a t -> 'a option t =
       fn show => fn NONE => "NONE" | (SOME x) => "SOME " ^ show x

      val pair: 'a t * 'b t -> ('a * 'b) t =
       fn (showa,showb) => fn (a,b) => "(" ^ showa a ^ "," ^ showb b ^ ")"

      val triple: 'a t * 'b t * 'c t -> ('a * 'b * 'c) t =
       fn (showa,showb,showc) => fn (a,b,c) => "(" ^ showa a ^ ","
                                               ^ showb b ^ ","
                                               ^ showc c ^ ")"

      val sq: 'a t -> ('a * 'a) t =
       fn (show) => fn (a,a') => "(" ^ show a ^ "," ^ show a' ^ ")"
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

functor MapEqFn(structure Map: ORD_MAP
                structure V: EQ): EQ =
   struct
      type t = V.t Map.map
      local
         structure ExtOrdMap = ExtOrdMapFn(Map)
      in
         fun eq (m,m') =
             if Map.numItems m <> Map.numItems m'
                then false
             else
                let
                   val keys = Map.listItemsi m
                   fun p (k,v) = case Map.find (m', k) of
                                     NONE => false
                                   | SOME v' => V.eq (v,v')
                in
                   List.all p keys
                end
      end
   end

(* returns a structure conforming to ORD_MAP for keys of type (a * b) *)
functor SqListMapFn(structure A: ORD_KEY
                    structure B: ORD_KEY): ORD_MAP =
   struct
      local
         structure SqOrdKey: ORD_KEY = struct
            type ord_key = A.ord_key * B.ord_key
            fun compare ((a,b), (a',b')) =
                case A.compare (a, a') of
                    LESS => LESS
                  | GREATER => GREATER
                  | EQUAL => B.compare (b, b')
         end
         structure Map = ListMapFn(SqOrdKey)
      in
         open Map
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

functor SetEqFn(structure Set: ORD_SET
                structure Eq: EQ
                sharing type Set.Key.ord_key = Eq.t): EQ =
   struct
      type t = Set.set
      fun eq (x,y) =
          case Set.compare (x,y) of
              EQUAL => true
            | _ => false
   end
