module Hw1 where

import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HwAux

calcUnitedNfa :: [Regex] -> NFA -- Thompson's construction (See https://en.wikipedia.org/wiki/Thompson%27s_construction)
calcUnitedNfa = runIdentity . go where
    mkNewQ :: StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity ParserState
    -- Get a new state of nfa:
    -- > q <- mkNewQ
    mkNewQ = do
        (q_next, delta) <- get
        put (q_next + 1, delta)
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
        (branches, (q_next, delta)) <- flip runStateT (1, Map.empty) $ sequence
            [ do
                qf <- mkNewQ
                (qi1, qf1) <- construct re
                epsilon q0 qi1
                epsilon qf1 qf
                return (qf, label)
            | (label, re) <- zip [1, 2 .. n] regexes
            ]
        return NFA
            { nfa_q0 = q0
            , nfa_qF = Map.fromList branches
            , nfa_delta = delta
            }

mkDfaFromNfa :: NFA -> DFA
mkDfaFromNfa (NFA q0 qfs delta) = runIdentity result where
    cl :: Set.Set ParserState -> Set.Set ParserState
    -- calculate epsilon-closure.
    cl qs = if qs == qs' then qs' else cl qs' where
        qs' :: Set.Set ParserState
        qs' = foldr Set.union qs [ maybe Set.empty id $ Map.lookup (q, Nothing) delta | q <- Set.toList qs ]
    nexts :: Set.Set ParserState -> MyChar -> Set.Set ParserState
    nexts qs ch = Set.unions [ maybe Set.empty id $ Map.lookup (q, Just ch) delta | q <- Set.toList qs ]
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
        return DFA
            { dfa_q0 = q0'
            , dfa_qF = qfs'
            , dfa_delta = delta'
            }
