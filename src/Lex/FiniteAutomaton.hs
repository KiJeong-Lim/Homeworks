module Lex.FiniteAutomaton where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Function
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lex.FiniteAutomatonAux

calcUnitedNfa :: Set.Set Char -> [Regex] -> NFA -- Thompson's construction (See https://en.wikipedia.org/wiki/Thompson%27s_construction)
calcUnitedNfa alphabets = runIdentity . go where
    mkNewQ :: StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity ParserState
    -- Get a new state of nfa:
    -- > q <- mkNewQ
    mkNewQ = do
        (q_next, delta) <- get
        let q_next' = succ q_next
        q_next' `seq` put (q_next', delta)
        return q_next
    addTransition :: ((ParserState, Maybe MyChar), ParserState) -> StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity ()
    addTransition (key, q) = do
        (q_next, delta) <- get
        put (q_next, Map.alter (Just . maybe (Set.singleton q) (Set.insert q)) key delta)
    epsilon :: ParserState -> ParserState -> StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity ()
    -- To add an epsilon-transition from `qi` to `qf`:
    epsilon qi qf = addTransition ((qi, Nothing), qf)
    char :: ParserState -> Char -> ParserState -> StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity ()
    -- To add a (normal) transition from `qi` to `qf` via `ch`:
    char qi ch qf = addTransition ((qi, Just ch), qf)
    construct :: Regex -> StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity (ParserState, ParserState)
    -- Construct transitions for a regex `re`:
    -- > (qi, qf) <- construct re
    -- where `qi` is the initial state, and
    --       `qf` is the final state.
    construct (ReUnion re1 re2) = do
        qi <- mkNewQ
        (qi1, qf1) <- construct re1
        (qi2, qf2) <- construct re2
        qf <- mkNewQ
        epsilon qi qi1
        epsilon qi qi2
        epsilon qf1 qf
        epsilon qf2 qf
        return (qi, qf)
    construct (ReConcat re1 re2) = do
        qi <- mkNewQ
        (qi1, qf1) <- construct re1
        (qi2, qf2) <- construct re2
        qf <- mkNewQ
        epsilon qi qi1
        epsilon qf1 qi2
        epsilon qf2 qf
        return (qi, qf)
    construct (ReStar re1) = do
        qi <- mkNewQ
        (qi1, qf1) <- construct re1
        qf <- mkNewQ
        epsilon qf1 qi1
        epsilon qi qi1
        epsilon qf1 qf
        epsilon qi qf
        return (qi, qf)
    construct (ReChar ch) = do
        qi <- mkNewQ
        qf <- mkNewQ
        char qi ch qf
        return (qi, qf)
    construct (ReEmpty) = do
        qi <- mkNewQ
        qf <- mkNewQ
        return (qi, qf)
    construct (ReNil) = do
        qi <- mkNewQ
        qf <- mkNewQ
        epsilon qi qf
        return (qi, qf)
    go :: [Regex] -> Identity NFA
    go regexes = do
        let q0 = 0
            n = length regexes
        (branches, (q_next, delta)) <- flip runStateT (succ q0, Map.empty) $ sequence
            [ do
                qf <- mkNewQ
                (qi1, qf1) <- construct re
                epsilon q0 qi1
                epsilon qf1 qf
                return (qf, label)
            | (label, re) <- zip [1, 2 .. n] regexes
            ]
        let qfs = Map.fromList branches
            states = Set.fromAscList [0 .. q_next - 1]
        return (NFA { nfa_states = states, nfa_alphabets = alphabets, nfa_q0 = q0, nfa_qfs = qfs, nfa_delta = delta })

mkDfaFromNfa :: NFA -> DFA
mkDfaFromNfa (NFA { nfa_states = states, nfa_alphabets = alphabets, nfa_q0 = q0, nfa_qfs = qfs, nfa_delta = delta }) = runIdentity result where
    cl :: Set.Set ParserState -> Set.Set ParserState -- calculates an epsilon-closure.
    cl qs = if qs == qs' then qs' else cl qs' where
        qs' :: Set.Set ParserState
        qs' = List.foldl' Set.union qs [ (q, Nothing) `Map.lookup` delta & maybe Set.empty id | q <- Set.toList qs ]
    nexts :: Set.Set ParserState -> MyChar -> Set.Set ParserState
    nexts qs ch = Set.unions [ (q, Just ch) `Map.lookup` delta & maybe Set.empty id | q <- Set.toList qs ]
    draw :: Map.Map (Set.Set ParserState) ParserState -> [((Set.Set ParserState, ParserState), MyChar)] -> StateT (Map.Map (ParserState, MyChar) ParserState) Identity (Map.Map (Set.Set ParserState) ParserState)
    draw mapping [] = return mapping
    draw mapping (((qs, q), ch) : items) = do
        let ps = cl (nexts qs ch)
        delta' <- get
        case ps `Map.lookup` mapping of
            Nothing -> do
                let p = Map.size mapping
                put (Map.insert (q, ch) p delta')
                draw (Map.insert ps p mapping) items
            Just p -> do
                put (Map.insert (q, ch) p delta')
                draw mapping items
    iter :: Map.Map (Set.Set ParserState) ParserState -> StateT (Map.Map (ParserState, MyChar) ParserState) Identity (Map.Map ParserState ExitNumber, Map.Map (Set.Set ParserState) ParserState)
    iter mapping = do
        mapping' <- draw mapping ((,) <$> Map.toList mapping <*> Set.toList alphabets)
        if mapping == mapping'
            then return (Map.fromList [ (p, label) | (q, label) <- Map.toList qfs, (qs, p) <- Map.toList mapping', q `Set.member` qs ], mapping')
            else iter mapping'
    result :: Identity DFA
    result = do
        let q0' = 0
        ((qfs', mapping'), delta') <- runStateT (iter (Map.singleton (cl (Set.singleton q0)) q0')) Map.empty
        let states' = (Map.keysSet delta' & Set.map fst) `Set.union` (Map.elems delta' & Set.fromList)
        return (DFA { dfa_states = states', dfa_alphabets = alphabets, dfa_q0 = q0', dfa_qfs = qfs', dfa_delta = delta' })

minimizeDFA :: DFA -> DFA
minimizeDFA (DFA { dfa_states = allStates, dfa_alphabets = alphabets, dfa_q0 = q0, dfa_qfs = qfs, dfa_delta = delta }) = result where
    initialKlasses :: [Set.Set ParserState]
    initialKlasses = ((allStates `Set.difference` Map.keysSet qfs) : Map.elems (Map.toList qfs & foldr loop1 Map.empty)) & filter (not . Set.null) where
        loop1 :: (ParserState, ExitNumber) -> Map.Map ExitNumber (Set.Set ParserState) -> Map.Map ExitNumber (Set.Set ParserState)
        loop1 (qf, label) = Map.alter (Just . maybe (Set.singleton qf) (Set.insert qf)) label
    finalKlasses :: [Set.Set ParserState]
    finalKlasses = splitKlasses initialKlasses initialKlasses where
        splitKlasses :: [Set.Set ParserState] -> [Set.Set ParserState] -> [Set.Set ParserState]
        splitKlasses result stack
            | null stack = result
            | otherwise = uncurry splitKlasses (Set.foldr (uncurry . loop2 (head stack)) (result, tail stack) alphabets)
        loop2 :: Set.Set ParserState -> Char -> [Set.Set ParserState] -> [Set.Set ParserState] -> ([Set.Set ParserState], [Set.Set ParserState])
        loop2 top ch = foldr (>=>) return . map loop3 where
            focused :: Set.Set ParserState
            focused = Set.filter (\q -> maybe False (\p -> p `Set.member` top) ((q, ch) `Map.lookup` delta)) allStates
            loop3 :: Set.Set ParserState -> [Set.Set ParserState] -> ([Set.Set ParserState], [Set.Set ParserState])
            loop3 klass stack
                | Set.null myintersection = ([klass], stack)
                | Set.null mydifference = ([klass], stack)
                | otherwise = ([myintersection, mydifference], stack')
                where
                    myintersection :: Set.Set ParserState
                    myintersection = klass `Set.intersection` focused
                    mydifference :: Set.Set ParserState
                    mydifference = klass `Set.difference` focused
                    stack' :: [Set.Set ParserState]
                    stack' = case klass `List.elemIndex` stack of
                        Nothing -> if Set.size myintersection <= Set.size mydifference then myintersection : stack else mydifference : stack
                        Just idx -> take idx stack ++ [myintersection, mydifference] ++ drop (idx + 1) stack
    convert :: ParserState -> ParserState
    convert q = head [ q' | (q', qs) <- zip [0, 1 .. ] finalKlasses, q `Set.member` qs ]
    result :: DFA
    result = DFA
        { dfa_states = Set.fromAscList [0 .. length finalKlasses - 1]
        , dfa_alphabets = alphabets
        , dfa_q0 = convert q0
        , dfa_qfs = Map.fromList [ (convert qf, label) | (qf, label) <- Map.toList qfs ]
        , dfa_delta = Map.fromList [ ((convert q, ch), convert p) | ((q, ch), p) <- Map.toList delta ]
        }

removeDeadStates :: DFA -> DFA -- returns a partial DFA.
removeDeadStates (DFA { dfa_states = states, dfa_alphabets = alphabets, dfa_q0 = q0, dfa_qfs = qfs, dfa_delta = delta }) = result where
    edges :: Map.Map ParserState (Set.Set ParserState)
    edges = [ (p, q) | ((q, ch), p) <- Map.toList delta ] & foldr loop1 Map.empty where
        loop1 :: (ParserState, ParserState) -> Map.Map ParserState (Set.Set ParserState) -> Map.Map ParserState (Set.Set ParserState)
        loop1 (p, q) = Map.alter (Just . maybe (Set.singleton q) (Set.insert q)) p
    winners :: Set.Set ParserState
    winners = Set.toList (Map.keysSet qfs) & loop2 Set.empty where
        loop2 :: Set.Set ParserState -> [ParserState] -> Set.Set ParserState
        loop2 ps [] = ps
        loop2 ps (q : qs) = if q `Set.member` ps then qs & loop2 ps else ((q `Map.lookup` edges & maybe [] Set.toAscList) ++ qs) & loop2 (Set.insert q ps)
    result :: DFA
    result = DFA
        { dfa_states = winners
        , dfa_alphabets = alphabets
        , dfa_q0 = q0
        , dfa_qfs = qfs
        , dfa_delta = Map.fromAscList [ ((q, ch), p) | ((q, ch), p) <- Map.toAscList delta, q `Set.member` winners, p `Set.member` winners ]
        }
