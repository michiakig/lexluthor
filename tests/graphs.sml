structure SimpleGraphTests =
struct

structure IntShowEq =
   struct
      type t = int option
      fun show NONE = "NONE"
        | show (SOME x) = "SOME " ^ Int.toString x
      val eq = op =
   end

structure IntTester = TestFn(structure Show = IntShowEq
                             structure Eq = IntShowEq)

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

val tests =
    IntTester.TGroup ("simple graphs",
            [IntTester.Case ("tree1", depthFirstSearch (tree, 1, 7), SOME 7)])

fun doTestRun v = IntTester.runTests v tests

end
