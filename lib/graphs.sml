(* directed, weighted (ie with labeled edges) *)

signature DIRECTED_WEIGHTED_GRAPH =
   sig
      eqtype (''n, ''l) t

      val empty: (''n, ''l) t
      val addEdge: (''n, ''l) t * ''n * ''n * ''l -> (''n, ''l) t
      val addEdges: (''n, ''l) t * (''n * ''n * ''l) list -> (''n, ''l) t
      val move: (''n, ''l) t * ''n * ''l -> ''n option
      val allNeighbors: (''n, ''l) t * ''n -> (''l * ''n) list
      val neighbors: (''n, ''l) t * ''n * ''l -> ''n list
      val adjacent: (''n, ''l) t * ''n * ''n -> bool
      val merge: (''n, ''l) t * (''n, ''l) t -> (''n, ''l) t
   end

signature DIRECTED_GRAPH =
   sig
      type ''n t
      val empty: ''n t
      val addEdge: ''n t * ''n * ''n -> ''n t
      val addEdges: ''n t * (''n * ''n) list -> ''n t
      val neighbors: ''n t * ''n -> ''n list
      val adjacent: ''n t * ''n * ''n -> bool
      val merge: ''n t * ''n t -> ''n t
   end

functor DirectedGraphFn(G: DIRECTED_WEIGHTED_GRAPH): DIRECTED_GRAPH =
   struct
      type ''n t = (''n, int) G.t

      val empty: ''n t = G.empty

      fun addEdge (g, n1, n2) = G.addEdge (g, n1, n2, 0)

      fun addEdges (g, newEdges) =
         let
            fun add ((n1, n2), g) = addEdge (g, n1, n2)
         in
            foldl add g newEdges
         end

      fun neighbors (g, n) = G.neighbors (g, n, 0)

      fun adjacent (g, n1, n2) = G.adjacent (g, n1, n2)

      fun merge (g, g') = G.merge (g, g')
   end

structure ListGraph :> DIRECTED_WEIGHTED_GRAPH =
   struct
      datatype (''n, ''l) edge = Edge of {start: ''n
                                         ,stop: ''n
                                         ,label: ''l}
      datatype (''n, ''l) graph = Graph of (''n, ''l) edge list
      type (''n, ''l) t = (''n, ''l) graph

      val empty = Graph []

      fun addEdge (Graph edges, start, stop, label) =
         let
            val newEdge = Edge {start=start, stop=stop, label=label}
         in
            Graph (newEdge :: edges)
         end

      fun addEdges (g, newEdges) =
         let
            fun add ((start, stop, label), g) = addEdge (g, start, stop, label)
         in
            foldl add g newEdges
         end

      fun move (Graph edges, n, l) =
         let
            fun move' [] = NONE
              | move' (Edge {start, stop, label} :: es) =
                 if start = n andalso label = l
                    then SOME stop
                 else move' es
         in
            move' edges
         end

      fun allNeighbors (Graph edges, n) =
         let
            fun f (Edge {start, stop, label}, acc) =
               if start = n
                  then (label, stop) :: acc
               else acc
         in
            foldl f [] edges
         end

      fun neighbors (Graph edges, n, l) =
         let
            fun f (Edge {start, stop, label}, acc) =
               if start = n andalso label = l
                  then stop :: acc
               else acc
         in
            foldl f [] edges
         end

      fun adjacent (Graph edges, n1, n2) =
         let
            fun f (Edge {start, stop, label}) =
               start = n1 andalso stop = n2 orelse
               start = n2 andalso stop = n1
         in
            List.exists f edges
         end

      fun merge (Graph edges, Graph edges') = Graph (edges @ edges')
   end
