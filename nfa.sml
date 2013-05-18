(*
 * signature for functional NFAs
 *)
signature NFA =
sig
   type t
   eqtype vtx
   datatype edge = Eps | Ch of char
   val id : unit -> vtx
   val cons : {start : vtx,
               edges : (vtx * edge) list,
               finals : vtx list} -> t
   val add : t * vtx * vtx * edge -> t
   val neighbors : t * vtx * edge -> vtx list
   val start : t -> vtx
   val finals : t -> vtx list
   val isFinal : t * vtx -> bool
   val merge : t * t * vtx -> t
end

(*
 * List-based implementation. Slightly less terrible than using only
 * lists, since it uses a map from state to list of edges, instead of
 * a list of all edges
 *)
structure NFA :> NFA =
struct
   structure M = IntListMap
   structure S = IntListSet
   type vtx = int
   datatype edge = Eps | Ch of char
   (* vertices, start, finals *)
   datatype nfa = NFA of (vtx * edge) list M.map * vtx * S.set
   type t = nfa
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
       NFA (M.insert (M.empty, start, edges), start, S.addList (S.empty, finals))
   fun start (NFA (_, start, _)) = start
   fun isFinal (NFA (_, _, finals), s) =
       S.member (finals, s)
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
   fun finals (NFA (_, _, finals)) = S.listItems finals
   exception NonDistinctStates
   fun merge (NFA (m1, s1, f1), NFA (m2, s2, f2), s) =
       NFA (M.unionWith (fn (x, y) => raise NonDistinctStates) (m1, m2),
            s,
            S.union (f1, f2))
end
