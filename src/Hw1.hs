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
    mkNewQ = do
        (q_next, delta) <- get
        put (q_next + 1, delta)
        return q_next
    -- [1] To get a new state of nfa:
    --     > q <- mkNewQ
    addTransition :: ((ParserState, Maybe MyChar), ParserState) -> StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity ()
    addTransition (key, q) = do
        (q_next, delta) <- get
        put (q_next, Map.alter (Just . maybe (Set.singleton q) (Set.insert q)) key delta)
    -- [1] To add an epsilon-transition from `qi` to `qf`:
    --     > addTransition ((qi, Nothing), qf)
    -- [2] To add a (normal) transition from `qi` to `qf` via `ch`:
    --     > addTransition ((qi, Just ch), qf)
    dispatch :: Regex -> StateT (ParserState, Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)) Identity (ParserState, ParserState)
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
        (branches, (q_next, delta)) <- flip runStateT (n + 1, Map.empty) $ sequence
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
