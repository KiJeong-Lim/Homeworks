module DfaAux where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type MyChar = Char

type ParserState = Int

type ExitNumber = Int

data Regex
    = ReUnion Regex Regex  -- `ReUnions re1 re2` = `re1` ∪ `re2`
    | ReConcat Regex Regex -- `ReConcat re1 re2` = `re1` `re2`
    | ReStar Regex         -- `ReStar re1`       = `re1`*
    | ReChar MyChar        -- `ReChar ch`        = 'ch'
    | ReEmpty              -- `ReEmpty`          = ∅
    | ReNil                -- `ReNil`            = ""
    deriving (Show)

data NFA
    = NFA
        { nfa_states :: Set.Set ParserState -- states
        , nfa_alphabets :: Set.Set Char -- alphabets
        , nfa_q0 :: ParserState -- initial state
        , nfa_qfs :: Map.Map ParserState ExitNumber -- final states
        , nfa_delta :: Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState) -- transitions
        }
    deriving (Show)

data DFA
    = DFA
        { dfa_states :: !(Set.Set ParserState)
        , dfa_alphabets :: !(Set.Set Char) -- alphabets
        , dfa_q0 :: !(ParserState) -- initial state
        , dfa_qfs :: !(Map.Map ParserState ExitNumber) -- final states
        , dfa_delta :: !(Map.Map (ParserState, MyChar) ParserState) -- transitions
        }
    deriving (Show)

collectTransititions :: (Ord a, Ord b) => [(a, b)] -> Map.Map a (Set.Set b)
collectTransititions = List.foldl' (\acc -> uncurry $ \a -> \b -> Map.alter (Just . maybe (Set.singleton b) (Set.insert b)) a acc) Map.empty
