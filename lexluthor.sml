structure LexLuthor =
struct

(* S is a set of states (ints), T is a set of S (set of sets of states) *)
structure S : ORD_SET = IntListSet
structure T : ORD_SET =
   ListSetFn(
       struct
          type ord_key = S.set
          val compare = S.compare
       end)

(* M is a map with keys of type set of states *)
structure M : ORD_MAP =
   ListMapFn(
       struct
          type ord_key = S.set
          val compare = S.compare
       end)

fun fromList l = S.addList(S.empty,l)

fun setToString s =
   let
      val items = S.listItems s
      val commas = Utils.interleave (map Int.toString items) ","
   in
      "{" ^ String.concat(commas) ^ "}"
   end

fun mapToString m =
   let
      val items = M.listItemsi m
      fun pairToS (s, i) = (setToString s) ^ ":" ^ (Int.toString i)
   in
      "{" ^ (String.concat(Utils.interleave (map pairToS items) ",")) ^ "}"
   end

(* bare minimal regular expressions:
   literal, concatenation, alternation, repetition (Kleene closure) *)

datatype 'a regex =
   Symbol of 'a
 | Concat of 'a regex * 'a regex
 | Altern of 'a regex * 'a regex
 | Repeat of 'a regex

type state = int

(* using an explicit datatype that wraps records rather than a type
   shorthand for a record which cuts down on tricky flex record
   issues, while being about as verbose *)

datatype 'a NFAinput = NFAinput of 'a | Epsilon

datatype 'a NFAedge = NFAedge of {beginState : state,
                                  endState   : state,
                                  label      : 'a NFAinput}

datatype 'a NFA = NFA of {startState : state,
                          edges      : 'a NFAedge list,
                          stopStates : S.set}

(* a little mutable state to implement unique ids makes it easier to
   merge NFAs, don't need to worry about preventing state collisions *)
local
   val id = ref 0
in
   fun nextId() =
      let
         val id' = !id
      in
         (id := !id + 1
          ; id')
      end
end

fun sym ch =
   let
      val start = nextId()
      val stop = nextId()
   in
      NFA {startState = start,
           stopStates = S.singleton stop,
           edges      = [NFAedge {beginState = start,
                                  label      = NFAinput ch,
                                  endState   = stop}]}
   end

fun concat (NFA {startState = startStateA,
                 stopStates = stopStatesA,
                 edges = edgesA},
            NFA {startState = startStateB,
                 stopStates = stopStatesB,
                 edges = edgesB}) =
    let
       val between = map (fn s => NFAedge {beginState = s,
                                           endState   = startStateB,
                                           label      = Epsilon})
                         (S.listItems stopStatesA)
    in
       NFA {startState = startStateA,
            stopStates = stopStatesB,
            edges      = edgesA @ edgesB @ between}
    end

fun altern (NFA {startState = startStateA,
                 stopStates = stopStatesA,
                 edges = edgesA},
            NFA {startState = startStateB,
                 stopStates = stopStatesB,
                 edges = edgesB}) =
    let
       val start = nextId()
       val final = nextId()
       val makeEdgeToFinal = (fn s => NFAedge {beginState = s,
                                               endState   = final,
                                               label      = Epsilon})
       val bStopsToFinal = map makeEdgeToFinal (S.listItems stopStatesB)
       val aStopsToFinal = map makeEdgeToFinal (S.listItems stopStatesA)
    in
       NFA {startState = start,
            stopStates = S.singleton final,
            edges      = NFAedge {beginState = start,
                                  label      = Epsilon,
                                  endState   = startStateA} ::
                         NFAedge {beginState = start,
                                  label      = Epsilon,
                                  endState   = startStateB} ::
                         edgesA @ edgesB @ aStopsToFinal @ bStopsToFinal}
    end

fun repeat (NFA {startState, stopStates, edges}) =
   let
      val epsEdges = foldl (fn (s, acc) => 
                                   (NFAedge {beginState = s,
                                             endState   = startState,
                                             label      = Epsilon})
                                   :: (NFAedge {beginState = startState,
                                                endState   = s,
                                                label      = Epsilon})
                                   :: acc)
                           []
                           (S.listItems stopStates)
   in
      NFA {startState = startState,
           stopStates = stopStates,
           edges      = epsEdges @ edges}
   end

fun regexToNFA (Symbol ch) = sym ch
  | regexToNFA (Concat (a,b)) = concat(regexToNFA a, regexToNFA b)
  | regexToNFA (Altern (a,b)) = altern(regexToNFA a, regexToNFA b)
  | regexToNFA (Repeat a) = repeat(regexToNFA a)

(* corresponds to "edge" function defined by Appel *)
fun edge (NFA {edges,...}, state, label1) =
   let
      fun pred (NFAedge {beginState, endState, label=label2}) =
          label1 = label2 andalso beginState = state
      fun endState (NFAedge {endState,...}) = endState
   in
      fromList (map endState (List.filter pred edges))
   end

(* union of a set of sets *)
fun union sets = T.foldl S.union S.empty sets

(* computes epsilon closure of a set of states *)
fun closure (nfa, states) =
   let
      fun epsEdge s = edge(nfa, s, Epsilon)
      fun oneStep s = S.union(s,foldl S.union S.empty (map epsEdge (S.listItems s)))
      fun loop t t' =
         if S.equal(t, t')
            then t
         else loop t' (oneStep t')
   in
      loop states (oneStep states)
   end

(* follow an implicit DFA edge and return the new DFA state *)
fun dfaEdge (nfa, states, input) =
   let
      fun e s = edge(nfa, s, input)
   in
      closure(nfa, foldl S.union S.empty
                         (map e (S.listItems states)))
   end

(* DFAs as distinct type from NFAs, note that DFAInput does not incude epsiplon *)
datatype 'a DFAinput = DFAinput of 'a

datatype 'a DFAedge = DFAedge of {beginState : state,
                                  endState   : state,
                                  label      : 'a DFAinput}

datatype 'a DFA = DFA of {startState : state,
                          edges      : 'a DFAedge list,
                          stopStates : S.set}

(* return list of ints from low to high, inclusive *)
fun range (low, high) =
   let
      val idx = ref low
      val ret = ref []
   in
      (while (!idx < high + 1) do
             (ret := !idx :: !ret;
              idx := !idx + 1);
       rev (!ret))
   end

(* return list of chars from low to high, inclusive *)
fun charRange (low, high) =
   map Char.chr
       (range (Char.ord low, Char.ord high))

(* FIXME alphabet restricted to lowercase letters, but should be a param *)
val alphas = charRange (#"a", #"z")

(* true if the two sets share at least one element *)
fun anyShared (s, t) =
   S.exists (fn x => S.member(t, x)) s

local
   fun pop r =
      let
         val p = hd (!r)
      in
         (r := tl (!r)
          ; p)
      end

   fun push (r,p) = r := p :: !r

   fun notEmpty lr = not (null (!lr))
in
   (* pretty hairy imperative version *)
   fun nfaToDfa (nfa as NFA {startState=nfaStartState,
                             edges=nfaEdges,
                             stopStates=nfaStopStates}) =
       let
          val dfaStartState = closure(nfa, S.singleton nfaStartState)
          val dfaStatesMap = ref (M.insert(M.empty, dfaStartState, nextId()))
          fun getDfaState d = let val s = M.find(!dfaStatesMap, d)
                              in case s of
                                    NONE => let val newId = nextId()
                                            in (dfaStatesMap := M.insert(!dfaStatesMap, d, newId); newId)
                                            end
                                  | SOME i => i
                              end
          val dfaEdges = ref []
          val dfaStates = ref (T.singleton dfaStartState) (* set of sets NFA states, ie set of DFA states *)
          val unvisited = ref [dfaStartState]
          fun findStopStates () = fromList(map (fn t => Option.valOf(M.find(!dfaStatesMap, t)))
                                               (T.listItems(T.filter (fn d => anyShared(d,nfaStopStates)) (!dfaStates))))
       in
          (while (notEmpty unvisited)
                 do let val u = pop unvisited
                        val idx = ref 0
                        val len = length alphas
                    in while (!idx < len)
                             do (let val t = dfaEdge(nfa, u, NFAinput (List.nth(alphas, !idx)))
                                 in if not (S.isEmpty t)
                                       then (dfaEdges := DFAedge {beginState = getDfaState u,
                                                                  endState = getDfaState t,
                                                                  label = DFAinput (List.nth(alphas,!idx))} :: (!dfaEdges);
                                             if not (T.member(!dfaStates, t))
                                                then (dfaStates := T.add (!dfaStates, t);
                                                      push(unvisited, t))
                                             else ())
                                    else ()
                                 end; idx := !idx + 1)
                    end;
           DFA {startState = Option.valOf(M.find(!dfaStatesMap, dfaStartState)),
                edges = (!dfaEdges),
                stopStates = findStopStates ()})
       end
end

fun inputToString (NFAinput ch) = Char.toString ch
  | inputToString Epsilon = "eps"

fun printNFA (NFA {startState, edges, stopStates}) =
   (print ("start=" ^ (Int.toString startState) ^ "\n");
    print "edges=\n";
    List.app (fn NFAedge {beginState, label,endState} =>
                     print (" " ^ (Int.toString beginState) ^ "-" ^ (inputToString label) ^ "-" ^ (Int.toString endState) ^ "\n"))
             edges;
    print "stops={ ";
    S.app (fn s => print (Int.toString s ^ " ")) stopStates;
    print "}\n")

fun printDFA (DFA {startState, edges, stopStates}) =
   (print ("start=" ^ (Int.toString startState) ^ "\n");
    print "edges=\n";
    List.app (fn DFAedge {beginState, label=(DFAinput ch), endState} =>
                     print (" " ^ (Int.toString beginState) ^ "-" ^ (Char.toString ch) ^ "-" ^ (Int.toString endState) ^ "\n"))
             edges;
    print "stops={ ";
    S.app (fn s => print (Int.toString s ^ " ")) stopStates;
    print "}\n")

fun dfaTransition (DFA {edges,...}, state, input) =
   let
      fun compare (DFAedge {beginState,label,...}) =
         state = beginState andalso input = label
   in
      case (List.find compare edges) of
         NONE => NONE
       | SOME (DFAedge {endState,...}) => SOME endState
   end

fun match' (dfa as DFA {startState, edges, stopStates}, input) =
   let
      fun isFinalState state =
         S.member(stopStates, state)

      fun succeed position =
         SOME (List.take(input, position), List.drop(input, position))

      fun loop currentS lastFinalS pos posAtLastFinalS =
         let
            fun finish () =
               if isFinalState currentS
                  then succeed pos
               else
                   if lastFinalS >= 0
                      then succeed posAtLastFinalS
                   else NONE
         in
            if pos > (length input - 1) (* at end of input *)
               then finish ()
            else
                let val ch = List.nth (input, pos)
                    val nextState = dfaTransition(dfa, currentS, ch)
                    val pos' = pos + 1
                in
                   case nextState of
                      NONE => finish ()
                    | SOME s => if isFinalState s
                                   then loop s s pos' pos'
                                else loop s lastFinalS pos' posAtLastFinalS
                end
         end
   in
      loop startState ~1 0 ~1 (* yuck how can we make this more typesafe? *)
   end

fun match (re, inputString) =
   let
      fun unwrap (DFAinput x) = x
      fun collapse list = String.implode (map unwrap list)
      val nfa = regexToNFA re
      val dfa = nfaToDfa nfa
   in
      case match' (dfa, map DFAinput (String.explode inputString)) of
         NONE => NONE
       | SOME (acc, rest) => SOME (collapse acc, collapse rest)
   end

end
