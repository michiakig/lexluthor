functor NFATests(N : NFA) =
struct
local
   open Test
   val deg = fn x => "-" (* degenerate `show` fn *)
   val vtx = polyAssertEq {show = deg}
   val alist = polyAssertEq {show=Show.list (Show.pair (deg, Show.string))}
   fun literal ch =
       let
          val start = N.id ()
          val stop = N.id ()
       in
          N.cons {start = start,
                  finals = [(stop, "tok")],
                  edges = [(stop, N.Ch ch)]}
       end
in
   val tests =
       let
          val s1 = N.id ()
          val s2 = N.id ()
          val s3 = N.id ()
          val s4 = N.id ()
          val nfa1 = N.cons {start = s1,
                             finals = [(s2, "tok1")],
                             edges = [(s2, N.Ch #"a")]}
          val nfa2 = N.cons {start = s3,
                             finals = [(s4, "tok2")],
                             edges = [(s4, N.Ch #"b")]}
          val nfa3 = N.setFinals (N.add (N.merge (nfa1, nfa2, s1), s2, s3, N.Eps),
                                  [(s4, "tok2")])
       in
          concat [single ("start", vtx, {actual = N.start nfa1, expected = s1}),
                  single ("finals", alist, {actual = N.finals nfa1,
                                            expected = [(s2, "tok1")]}),
                  single ("merge1", vtx, {actual = N.start nfa3, expected = s1}),
                  single ("merge2", alist, {actual = N.finals nfa3,
                                            expected = [(s4, "tok2")]})
          ]
       end
end
end

structure ListNFATests = NFATests(NFA)
