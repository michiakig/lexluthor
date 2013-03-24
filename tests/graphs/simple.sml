functor DirectedWeightedGraphTesterFn(G: DIRECTED_WEIGHTED_GRAPH) =
struct
   local
      val assert = Test.polyAssertEq {show=Show.option Show.int}
      val g = G.addEdge(G.empty, 1, 2, "foo")
      val g1 = G.addEdges(G.empty, [(1,2,"foo"), (2,3,"bar")])
      val tests =
          ("simple graph tests",
           [{actual = G.move (g, 1, "foo"), expected = SOME 2},
            {actual = G.move (g, 1, "bar"), expected = NONE},
            {actual = G.move (g, 3, "baz"), expected = NONE},
            {actual = G.move (g1, 2, "bar"), expected = SOME 3}])
   in
   fun doTestRun v =
       Test.runTestSuite assert v tests
   end
end

structure ListGraphTests = DirectedWeightedGraphTesterFn(ListGraph)
