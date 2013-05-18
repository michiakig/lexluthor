(*
 * signature for functional NFAs
 *)
signature NFA =
sig
   type 'a t
   eqtype vtx
   datatype edge = Eps | Ch of char
   val id : unit -> vtx
   val cons : {start : vtx,
               edges : (vtx * edge) list,
               finals : (vtx * 'a) list} -> 'a t
   val add : 'a t * vtx * vtx * edge -> 'a t
   val neighbors : 'a t * vtx * edge -> vtx list
   val start : 'a t -> vtx
   val setStart : 'a t * vtx -> 'a t
   val finals : 'a t -> (vtx * 'a) list
   val setFinals : 'a t * (vtx * 'a) list -> 'a t
   val isFinal : 'a t * vtx -> 'a option
   val merge : 'a t * 'a t * vtx -> 'a t
end

(*
 * List-based implementation. Slightly less terrible than using only
 * lists, since it uses a map from state to list of edges, instead of
 * a list of all edges
 *)
structure NFA :> NFA =
struct
   structure M = IntListMap
   type vtx = int
   datatype edge = Eps | Ch of char
   (* vertices, start, finals *)
   datatype 'a nfa = NFA of (vtx * edge) list M.map * vtx * 'a M.map
   type 'a t = 'a nfa
   fun insertList (m, alist) =
       foldl (fn ((k, v), acc) => M.insert (acc, k, v)) m alist
   local
      val id_ = ref 0
   in
      fun id () =
          let
             val x = ! id_
          in
             (id_ := x + 1
             ; x)
          end
   end
   fun cons {start, edges, finals} =
       NFA (M.insert (M.empty, start, edges), start, insertList (M.empty, finals))
   fun start (NFA (_, start, _)) = start
   fun setStart (NFA (vs, _, fs), s) = NFA (vs, s, fs)
   fun isFinal (NFA (_, _, finals), s) =
       M.find (finals, s)
   fun add (NFA (m, s, f), x, y, ch) =
       let
          val xedges = Option.getOpt (M.find (m, x), [])
          val yedges = Option.getOpt (M.find (m, y), [])
       in
          NFA (M.insert (M.insert (m, x, (y, ch) :: xedges),
                         y, yedges), s, f)
       end
   exception NoSuchVertex
   fun neighbors (NFA (m, _, _), x, ch) =
       case M.find (m, x) of
           NONE => raise NoSuchVertex
         | SOME edges => map (fn (x, _) => x)
                             (List.filter (fn (x', ch') => ch = ch') edges)
   fun finals (NFA (_, _, finals)) = M.listItemsi finals
   fun setFinals (NFA (vs, s, _), finals) =
       NFA (vs, s, insertList (M.empty, finals))
   exception NonDistinctStates
   fun merge (NFA (m1, s1, f1), NFA (m2, s2, f2), s) =
       let
          fun error _ = raise NonDistinctStates
       in
          NFA (M.unionWith error (m1, m2), s, M.unionWith error (f1, f2))
       end
end
