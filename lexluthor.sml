
(* bare minimal regular expressions:
   literal, concatenation, alternation, repetition (Kleene closure) *)

structure BasicRegExpSyntax =
   struct

      structure R = RegExpSyntax

      datatype syntax =
         Symbol of char
       | Concat of syntax * syntax
       | Altern of syntax * syntax
       | Repeat of syntax
       | Epsilon

      val getc: string -> (char, int) StringCvt.reader =
         fn s => fn i =>
            if i < String.size s
               then SOME(String.sub(s, i), i+1)
            else NONE

      val parse: string -> R.syntax option =
         fn s =>
            case (AwkSyntax.scan (getc s)) 0 of
               NONE => NONE
             | SOME (re, _) => SOME re

      fun unsafeParse s = Option.valOf(parse s)

      (*
      not supported:

        | Interval of (syntax * int * int option)
        | NonmatchSet of CharSet.set
        | Begin
        | End
      *)
      local
         fun reduce f l = foldl f (hd l) (tl l)
      in
         fun desugar (R.Char ch) = Symbol ch
           | desugar (R.Star syn) = Repeat (desugar syn)
           | desugar (R.Alt syns) = reduce Altern (rev (map desugar syns))
           | desugar (R.Concat syns) = reduce Concat (rev (map desugar syns))
           | desugar (R.MatchSet charSet) =
             reduce Altern (rev (map Symbol
                                     (R.CharSet.listItems charSet)))
           | desugar (R.Plus syn) = let val b = (desugar syn)
                                    in Altern (b, Repeat b)
                                    end
           | desugar (R.Option syn) = let val b = (desugar syn)
                                      in Altern (b, Epsilon)
                                      end
           | desugar (R.Group syn) = desugar syn
      end

      fun unsafeDesugar s = desugar(unsafeParse s)

   end

signature LEXER_SPEC =
   sig
      eqtype token
      val tokens: (BasicRegExpSyntax.syntax * token) list
   end

functor LexLuthorFn(LexerSpec: LEXER_SPEC) =
struct

structure RE = BasicRegExpSyntax

(* S is a set of states (ints), T is a set of S (set of sets of states) *)
structure S : ORD_SET = IntListSet
structure T : ORD_SET =
   ListSetFn(
       struct
          type ord_key = S.set
          val compare = S.compare
       end)

structure M = ExtOrdMapFn(ListMapFn(
       struct
          type ord_key = S.set
          val compare = S.compare
       end))

structure IntListMap = ExtOrdMapFn(IntListMap)

fun fromList l = S.addList(S.empty,l)

fun setToString s =
   let
      val items = S.listItems s
      val commas = ExtList.interleave (map Int.toString items) ","
   in
      "{" ^ String.concat(commas) ^ "}"
   end

fun mapToString m =
   let
      val items = M.listItemsi m
      fun pairToS (s, i) = (setToString s) ^ ":" ^ (Int.toString i)
   in
      "{" ^ (String.concat(ExtList.interleave (map pairToS items) ",")) ^ "}"
   end

type state = int

(* using an explicit datatype that wraps records rather than a type
   shorthand for a record which cuts down on tricky flex record
   issues, while being about as verbose *)

datatype 'a NFAinput = NFAinput of 'a | Epsilon

datatype 'a NFAedge = NFAedge of {beginState : state,
                                  endState   : state,
                                  label      : 'a NFAinput}

(* stop states represented as a map from state (int) to token *)
datatype 'a NFA = NFA of {startState : state,
                          edges      : 'a NFAedge list,
                          stopStates : LexerSpec.token option IntListMap.map}

(* a little mutable state to implement unique ids makes it easier to
   merge NFAs, don't need to worry about preventing state collisions *)
local
   val id = ref 0
in
   fun nextId () =
      let
         val id' = !id
      in
         (id := !id + 1
          ; id')
      end
end

fun epsilon tok =
   let
      val start = nextId ()
      val stop = nextId ();
   in
      NFA {startState = start,
           stopStates = IntListMap.singleton (stop, tok),
           edges      = [NFAedge {beginState = start,
                                  label      = Epsilon,
                                  endState   = stop}]}
   end

fun sym (tok, ch) =
   let
      val start = nextId ()
      val stop = nextId ()
   in
      NFA {startState = start,
           stopStates = IntListMap.singleton (stop, tok),
           edges      = [NFAedge {beginState = start,
                                  label      = NFAinput ch,
                                  endState   = stop}]}
   end

fun concat (tok,
            NFA {startState = startStateA,
                 stopStates = stopStatesA,
                 edges = edgesA},
            NFA {startState = startStateB,
                 stopStates = stopStatesB,
                 edges = edgesB}) =
    let
       val between = map (fn s => NFAedge {beginState = s,
                                           endState   = startStateB,
                                           label      = Epsilon})
                         (IntListMap.keys stopStatesA)
    in
       NFA {startState = startStateA,
            stopStates = stopStatesB,
            edges      = edgesA @ edgesB @ between}
    end

fun altern (tok,
            NFA {startState = startStateA,
                 stopStates = stopStatesA,
                 edges = edgesA},
            NFA {startState = startStateB,
                 stopStates = stopStatesB,
                 edges = edgesB}) =
    let
       val start = nextId ()
    in
       NFA {startState = start,
            stopStates = IntListMap.unionWith Pair.first (stopStatesA, stopStatesB),
            edges      = NFAedge {beginState = start,
                                  label      = Epsilon,
                                  endState   = startStateA} ::
                         NFAedge {beginState = start,
                                  label      = Epsilon,
                                  endState   = startStateB} ::
                         edgesA @ edgesB}
    end

fun repeat (tok, NFA {startState, stopStates, edges}) =
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
                           (IntListMap.keys stopStates)
   in
      NFA {startState = startState,
           stopStates = stopStates,
           edges      = epsEdges @ edges}
   end

fun regexToNFA (tok, RE.Symbol ch) = sym (tok, ch)
  | regexToNFA (tok, RE.Epsilon) = epsilon tok
  | regexToNFA (tok, RE.Concat (a, b)) = concat(tok,
                                             regexToNFA (tok, a),
                                             regexToNFA (tok, b))
  | regexToNFA (tok, RE.Altern (a, b)) = altern(tok,
                                             regexToNFA (tok, a),
                                             regexToNFA (tok, b))
  | regexToNFA (tok, RE.Repeat a) = repeat(tok, regexToNFA (tok, a))

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
   in
      HigherOrder.fixedPoint (states, oneStep, S.equal)
   end

(* follow an implicit DFA edge and return the new DFA state *)
fun dfaEdge (nfa, states, input) =
   let
      fun e s = edge(nfa, s, input)
   in
      closure(nfa, foldl S.union S.empty
                         (map e (S.listItems states)))
   end

(* DFAs as distinct type from NFAs, note that DFAInput does not incude epsilon *)
datatype 'a DFAinput = DFAinput of 'a

datatype 'a DFAedge = DFAedge of {beginState : state,
                                  endState   : state,
                                  label      : 'a DFAinput}

datatype 'a DFA = DFA of {startState : state,
                          edges      : 'a DFAedge list,
                          stopStates : LexerSpec.token option IntListMap.map}

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

(* FIXME alphabet restricted to lowercase letters and digits, but should be a param *)
val alphas = charRange (#"a", #"z") @ charRange (#"0", #"9") @ [#" ", #"(", #")"]

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
          val dfaStatesMap = ref (M.insert(M.empty, dfaStartState, nextId ()))
          fun getDfaState d = let val s = M.find(!dfaStatesMap, d)
                              in case s of
                                    NONE => let val newId = nextId ()
                                            in (dfaStatesMap := M.insert(!dfaStatesMap, d, newId); newId)
                                            end
                                  | SOME i => i
                              end
          val dfaEdges = ref []
          val dfaStates = ref (T.singleton dfaStartState) (* set of sets NFA states, ie set of DFA states *)
          val unvisited = ref [dfaStartState]

          fun findStopStates dfaStatesMap nfaStopStates dfaStates =
             T.foldl (fn (dfaState, stopsMap) =>
                        let
                           val toks =
                              S.foldl (fn (nfaState, acc) =>
                                         case IntListMap.find(nfaStopStates, nfaState) of
                                            NONE => acc
                                          | SOME tok => tok :: acc)
                                      []
                                      dfaState
                        in
                           if not (null toks)
                              then (if not (ExtList.allEq toks)
                                       then print "warning! multiple distinct tokens for DFA final state"
                                    else ()
                                    ; IntListMap.insert(stopsMap, M.unsafeFind dfaStatesMap dfaState, hd toks))
                           else stopsMap
                        end)
                     IntListMap.empty
                     dfaStates
       in
          (while (notEmpty unvisited) do
                 let
                    val u = pop unvisited
                 in
                    List.app (fn a =>
                                     let
                                        val t = dfaEdge(nfa, u, NFAinput a)
                                     in
                                        if not (S.isEmpty t)
                                           then (dfaEdges := DFAedge {beginState = getDfaState u,
                                                                      endState = getDfaState t,
                                                                      label = DFAinput a} :: (!dfaEdges);
                                                 if not (T.member(!dfaStates, t))
                                                    then (dfaStates := T.add (!dfaStates, t);
                                                          push(unvisited, t))
                                                 else ())
                                        else ()
                                     end)
                             alphas
                    end;
           DFA {startState = Option.valOf(M.find(!dfaStatesMap, dfaStartState)),
                edges = (!dfaEdges),
                stopStates = findStopStates (!dfaStatesMap) nfaStopStates (!dfaStates) })
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
    S.app (fn s => print (Int.toString s ^ " ")) (fromList(IntListMap.keys stopStates));
    print "}\n")

fun printDFA (DFA {startState, edges, stopStates}) =
   (print ("start=" ^ (Int.toString startState) ^ "\n");
    print "edges=\n";
    List.app (fn DFAedge {beginState, label=(DFAinput ch), endState} =>
                     print (" " ^ (Int.toString beginState) ^ "-" ^ (Char.toString ch) ^ "-" ^ (Int.toString endState) ^ "\n"))
             edges;
    print "stops={ ";
    S.app (fn s => print (Int.toString s ^ " ")) (fromList(IntListMap.keys stopStates));
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
         case IntListMap.find(stopStates, state) of
            NONE => false
          | SOME _ => true

      fun succeed finalState position =
         let
            val SOME tok = IntListMap.find(stopStates, finalState)
         in
            SOME (tok, List.take(input, position), List.drop(input, position))
         end

      fun loop currentS lastFinalS pos posAtLastFinalS =
         let
            fun finish () =
               if isFinalState currentS
                  then succeed currentS pos
               else
                   if lastFinalS >= 0
                      then succeed lastFinalS posAtLastFinalS
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

fun unwrap (DFAinput x) = x
fun collapse list = String.implode (map unwrap list)

fun match (re, inputString) =
   let
      val nfa = regexToNFA (NONE, re)
      val dfa = nfaToDfa nfa
   in
      case match' (dfa, map DFAinput (String.explode inputString)) of
         NONE => NONE
       | SOME (tok, acc, rest) => SOME (collapse acc, collapse rest)
   end

fun makeNfa (regex, token) =
   regexToNFA(SOME token, regex)

fun combineNFAs (nfa1, nfa2) =
   altern(NONE, nfa1, nfa2)

fun sizeNFA (NFA {edges=edges,...}) =
    let
       val states = foldl (fn (NFAedge {beginState=b,endState=e,...},acc) =>
                              S.add(S.add(acc, b), e))
                          S.empty
                          edges
    in
       (S.numItems states, length edges)
    end

fun sizeDFA (DFA {edges=edges,...}) =
    let
       val states = foldl (fn (DFAedge {beginState=b,endState=e,...},acc) =>
                              S.add(S.add(acc, b), e))
                          S.empty
                          edges
    in
       (S.numItems states, length edges)
    end

datatype lexer = Lexer of char DFA

fun mkLexer () =
    let
       (* FIXME what if tokens is [] ? *)
       val nfas = map makeNfa LexerSpec.tokens
       val combinedNFA = foldl combineNFAs (hd nfas) (tl nfas)
       val (statesNFA,edgesNFA) = sizeNFA combinedNFA
       val _ = print ("NFA=" ^ Int.toString statesNFA ^ " states,"
                      ^ Int.toString edgesNFA ^ " edges,")
       val dfa = nfaToDfa combinedNFA
       val (statesDFA,edgesDFA) = sizeDFA dfa
       val _ = print ("DFA=" ^ Int.toString statesDFA ^ " states,"
                      ^ Int.toString edgesDFA ^ " edges\n")
    in
       Lexer dfa
    end

fun lex (Lexer dfa, s) =
   let
      fun loop acc inputs =
         case match' (dfa, inputs) of
            NONE => rev acc
          | SOME (_, [], _) => rev acc
          | SOME (tok, match, rest) =>
                (Option.valOf tok, collapse match) :: loop acc rest
   in
      (loop [] (map DFAinput (String.explode s)))
   end

end
