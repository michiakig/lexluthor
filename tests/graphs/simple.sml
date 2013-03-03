
functor DirectedWeightedGraphTesterFn(G: DIRECTED_WEIGHTED_GRAPH) =
   struct
      structure I = IntOptionTester
      fun doTestRun v =
          let
             val g = G.addEdge(G.empty, 1, 2, "foo")
             val g1 = G.addEdges(G.empty, [(1,2,"foo"), (2,3,"bar")])
          in
             I.runTests v (I.TGroup ("graph test functor",
                                     [I.Case ("move", {actual=G.move (g, 1, "foo"), expect=SOME 2}),
                                      I.Case ("move1", {actual=G.move (g, 3, "baz"), expect=NONE}),
                                      I.Case ("move2", {actual=G.move (g1, 2, "bar"), expect=SOME 3})]))
          end
   end

structure ListGraphTests = DirectedWeightedGraphTesterFn(ListGraph)
