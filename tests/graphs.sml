structure IntOptionShowEq =
   struct
      type t = int option
      fun show NONE = "NONE"
        | show (SOME x) = "SOME " ^ Int.toString x
      val eq = op =
   end

structure IntOptionTester = TestFn(structure Show = IntOptionShowEq
                                   structure Eq = IntOptionShowEq)

structure SimpleGraphTests =
struct

structure G = DirectedGraphFn(ListGraph)

val tree = G.addEdges(G.empty, [(1,2),(1,3),(2,4),(2,5),(3,6),(3,7)])
val cycle = G.addEdges(G.empty, [(1,2),(2,3),(3,4),(4,5),(5,6),(6,1)])
val complete = G.addEdges(G.empty, [(1,2),(2,1),(1,3),(3,1),(2,3),(3,2)])

fun notMember [] x = true
  | notMember (y :: ys) x =
    if x = y
       then false
    else notMember ys x

(* adapted from Peter Norvig's Paradigms of AI Programming *)
fun search ([], visited, goal, successors, combiner) = NONE
  | search (x :: xs, visited, goal, successors, combiner) =
    if goal x
       then SOME x
    else let
            val frontier = combiner (successors x, xs)
         in
            search (List.filter (notMember visited) frontier,
                    x :: visited, goal, successors, combiner)
         end
local
   val append = (op @)
   fun prepend (xs, ys) = ys @ xs
in
   fun depthFirstSearch (g, start, goal) =
      let
         fun goalFn x = goal = x
         fun successors x = G.neighbors (g, x)
      in
         search ([start], [], goalFn, successors, append)
      end

   fun breadthFirstSearch (g, start, goal) =
      let
         fun goalFn x = goal = x
         fun successors x = G.neighbors (g, x)
      in         
         search ([start], [], goalFn, successors, prepend)
      end
end

structure I = IntOptionTester

val tests =
    I.TGroup ("simple graphs",
            [I.Case ("tree1", depthFirstSearch (tree, 1, 7), SOME 7),
             I.Case ("tree2", depthFirstSearch (tree, 1, 9), NONE),
             I.Case ("tree3", breadthFirstSearch (tree, 1, 9), NONE),
             I.Case ("tree4", breadthFirstSearch (tree, 1, 7), SOME 7)])

fun doTestRun v = I.runTests v tests

end

functor DirectedWeightedGraphTesterFn(G: DIRECTED_WEIGHTED_GRAPH) =
   struct
      structure I = IntOptionTester
      fun doTestRun v =
          let
             val g = G.addEdge(G.empty, 1, 2, "foo")
          in
             I.runTests v (I.TGroup ("graph test functor",
                                     [I.Case ("move", G.move (g, 1, "foo"), SOME 2)]))
          end
   end

structure ListGraphTests = DirectedWeightedGraphTesterFn(ListGraph)
