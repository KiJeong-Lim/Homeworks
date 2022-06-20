module HwAux where

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
        { nfa_q0 :: ParserState -- the initial state
        , nfa_qF :: Map.Map ParserState ExitNumber -- final states
        , nfa_delta :: Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState) -- transitions
        }
    deriving (Show)

data DFA
    = DFA
        { dfa_q0 :: !(ParserState) -- the initial state
        , dfa_qF :: !(Map.Map ParserState ExitNumber) -- final states
        , dfa_delta :: !(Map.Map (ParserState, MyChar) ParserState) -- transitions
        }
    deriving (Show)

alphabets :: Set.Set Char
alphabets = Set.fromList (['a' .. 'z'] ++ ['A' .. 'Z'] ++ " `~0123456789!@#$%^&*()-=_+[]\\{}|;\':\"\n,./<>?")
