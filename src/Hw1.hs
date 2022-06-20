module Hw1 where

import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HwAux

calcUnitedNFA :: [Regex] -> NFA -- Thompson's construction (See https://en.wikipedia.org/wiki/Thompson%27s_construction)
calcUnitedNFA = runIdentity . go where
    mkNewQ :: StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity ParserState
    -- [1] To get a new state of nfa:
    --     > q <- mkNewQ
    mkNewQ = do
        (q_next, delta) <- get
        put (q_next + 1, delta)
        return q_next
    addTransition :: ((ParserState, Maybe MyChar), ParserState) -> StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity ()
    -- [1] To add an epsilon-transition from `qi` to `qf`:
    --     > addTransition ((qi, Nothing), qf)
    -- [2] To add a (normal) transition from `qi` to `qf` via `ch`:
    --     > addTransition ((qi, Just ch), qf)
    addTransition (key, q) = do
        (q_next, delta) <- get
        put (q_next, Map.alter (Just . maybe (Set.singleton q) (Set.insert q)) key delta)
    dispatch :: Regex -> StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity (ParserState, ParserState)
    -- [1] To construct transitions for a regex `re`:
    --     > (qi, qf) <- dispatch re
    --     where `qi` is the initial state, and
    --           `qf` is the final state.
    dispatch (ReUnion re1 re2) = undefined {- impl this -}
    dispatch (ReConcat re1 re2) = undefined {- impl this -}
    dispatch (ReStar re1) = undefined {- impl this -}
    dispatch (ReChar ch) = undefined {- impl this -}
    dispatch (ReEmpty) = undefined {- impl this -}
    dispatch (ReNil) = undefined {- impl this -}
    go :: [Regex] -> Identity NFA
    go regexes = do
        let q0 = 0
            n = length regexes
        (branches, (q_next, delta)) <- flip runStateT (1, Map.empty) $ sequence
            [ do
                qf <- mkNewQ
                (qi1, qf1) <- dispatch re
                addTransition ((q0, Nothing), qi1)
                addTransition ((qf1, Nothing), qf)
                return (qf, label)
            | (label, re) <- zip [1, 2 .. n] regexes
            ]
        return $ NFA
            { nfa_Q0 = q0
            , nfa_QF = Map.fromList branches
            , nfa_delta = delta
            }

calcDFA :: NFA -> DFA
calcDFA (NFA q0 qfs delta) = runIdentity result where
    cl :: Set.Set ParserState -> Set.Set ParserState
    -- calculate epsilon-closure.
    cl qs = if qs == qs' then qs' else cl qs' where
        qs' :: Set.Set ParserState
        qs' = foldr Set.union qs
            [ case Map.lookup (q, Nothing) delta of
                Nothing -> Set.empty
                Just ps -> ps
            | q <- Set.toList qs
            ]
    nexts :: Set.Set ParserState -> MyChar -> Set.Set ParserState
    nexts qs ch = Set.unions
        [ case Map.lookup (q, Just ch) delta of
            Nothing -> Set.empty
            Just ps -> ps
        | q <- Set.toList qs
        ]
    draw :: Map.Map (Set.Set ParserState) ParserState -> [((Set.Set ParserState, ParserState), MyChar)] -> StateT (Map.Map (ParserState, MyChar) ParserState) Identity (Map.Map (Set.Set ParserState) ParserState)
    draw mapping [] = return mapping
    draw mapping (((qs, q'), ch) : items) = do
        let ps = cl (nexts qs ch)
        delta' <- get
        case Map.lookup ps mapping of
            Nothing -> do
                let p' = Map.size mapping
                put (Map.insert (q', ch) p' delta')
                draw (Map.insert ps p' mapping) items
            Just p' -> do
                put (Map.insert (q', ch) p' delta')
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
        return $ DFA
            { dfa_Q0 = q0'
            , dfa_QF = qfs'
            , dfa_delta = delta'
            }
