module Lex.FiniteAutomatonAux where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type MyChar = Char

type ParserState = Int

type ExitNumber = Int

type NFA = GNFA ParserState

type DFA = GDFA ParserState

data Regex
    = ReUnion Regex Regex  -- `ReUnions re1 re2` = `re1` ∪ `re2`
    | ReConcat Regex Regex -- `ReConcat re1 re2` = `re1` `re2`
    | ReStar Regex         -- `ReStar re1`       = `re1`*
    | ReChar MyChar        -- `ReChar ch`        = 'ch'
    | ReEmpty              -- `ReEmpty`          = ∅
    | ReNil                -- `ReNil`            = ""
    deriving (Show)

data GNFA state
    = NFA
        { nfa_states :: Set.Set state -- states
        , nfa_alphabets :: Set.Set Char -- alphabets
        , nfa_q0 :: state -- initial state
        , nfa_qfs :: Map.Map state ExitNumber -- final states
        , nfa_delta :: Map.Map (state, Maybe MyChar) (Set.Set state) -- transitions
        }
    deriving (Show)

data GDFA state
    = DFA
        { dfa_states :: !(Set.Set state) -- states
        , dfa_alphabets :: !(Set.Set Char) -- alphabets
        , dfa_q0 :: !(state) -- initial state
        , dfa_qfs :: !(Map.Map state ExitNumber) -- final states
        , dfa_delta :: !(Map.Map (state, MyChar) state) -- transitions
        }
    deriving (Show)

mkGraph :: (Ord a, Ord b) => [(a, b)] -> Map.Map a (Set.Set b)
mkGraph = List.foldl' (\acc -> uncurry $ \a -> \b -> Map.alter (Just . maybe (Set.singleton b) (Set.insert b)) a acc) Map.empty
