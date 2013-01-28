structure Utils =
   struct

      fun interleave l i =
         let
            fun recur [] acc = rev acc
              | recur (x :: []) acc = recur [] (x :: acc)
              | recur (x :: xs) acc = recur xs (i :: x :: acc)
         in
            recur l []
         end

      fun first (x,_) = x
      fun second (_,y) = y

      fun allEq [] = true
        | allEq (x :: []) = true
        | allEq (x :: y :: xs) =
           x = y andalso allEq xs

   end

functor MapUtilsFn(Map: ORD_MAP) =
   struct
      fun unsafeFind m k = Option.valOf(Map.find(m, k))
      fun keys m = map Utils.first (Map.listItemsi m)
   end
