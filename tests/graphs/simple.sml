functor DirectedWeightedGraphTesterFn(G: DIRECTED_WEIGHTED_GRAPH) =
struct
open Test
val g = G.addEdge(G.empty, 1, 2, "foo")
val g1 = G.addEdges(G.empty, [(1,2,"foo"), (2,3,"bar")])
val tree = G.addEdges(G.empty,
                      [(1,2,"a"),(1,3,"b"),(2,4,"e"),(2,5,"f"),(3,6,"d"),(3,7,"c")])
val tests =
    concat
       [group
           ("move", polyAssertEq {show=Show.option Show.int},
            [{actual = G.move (g, 1, "foo"), expected = SOME 2},
             {actual = G.move (g, 1, "bar"), expected = NONE},
             {actual = G.move (g, 3, "baz"), expected = NONE},
             {actual = G.move (g1, 2, "bar"), expected = SOME 3},
             {actual = G.move (g1, 2, "bar"), expected = SOME 3}]),
        assertAllTrue
           ("adjacency",
            [G.adjacent (g, 1, 2),
             G.adjacent (g1, 1, 2),
             G.adjacent (g1, 2, 3)]),
        assertAllFalse
           ("adjacency",
            [G.adjacent (g, 1, 1),
             G.adjacent (g, 2, 2),
             G.adjacent (g, 0, 1),
             G.adjacent (g, 1, 0),
             G.adjacent (g, 0, 0),
             G.adjacent (g1, 1, 3),
             G.adjacent (g1, 3, 1)]),
        group
           ("neighbors", polyAssertEq {show=Show.list Show.int},
            [{actual = G.neighbors (g, 1, "foo"), expected = [2]},
             {actual = G.neighbors (g, 1, "bar"), expected = []},
             {actual = G.neighbors (g1, 1, "foo"), expected = [2]},
             {actual = G.neighbors (g1, 2, "bar"), expected = [3]}]),
        group
           ("allNeighbors", polyAssertEq
                               {show=Show.list (Show.pair (Show.string, Show.int))},
            [{actual = G.allNeighbors (tree, 1), expected = [("a",2), ("b",3)]}
       ])]

end

structure ListGraphTests = DirectedWeightedGraphTesterFn(ListGraph)
