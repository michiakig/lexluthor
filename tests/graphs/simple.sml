
functor DirectedWeightedGraphTesterFn(G: DIRECTED_WEIGHTED_GRAPH) =
   struct
      fun doTestRun v =
          let
             val g = G.addEdge(G.empty, 1, 2, "foo")
             val g1 = G.addEdges(G.empty, [(1,2,"foo"), (2,3,"bar")])
          in
             Test.runTestSuite (Test.polyAssertEq {show=Show.option Show.int}) v
                               ("graph test functor",
                                [{actual=G.move (g, 1, "foo"), expected=SOME 2},
                                 {actual=G.move (g, 3, "baz"), expected=NONE},
                                 {actual=G.move (g1, 2, "bar"), expected=SOME 3}])
          end
   end

structure ListGraphTests = DirectedWeightedGraphTesterFn(ListGraph)
