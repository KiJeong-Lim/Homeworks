module HwAux where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type MyChar = Char

type ParserState = Int

type ExitNumber = Int

data Regex
    = ReUnion Regex Regex
    | ReConcat Regex Regex
    | ReStar Regex
    | ReChar MyChar
    | ReEmpty
    | ReNil
    deriving (Show)

data NFA
    = NFA
        { nfa_Q0 :: ParserState
        , nfa_QF :: Map.Map ParserState ExitNumber
        , nfa_delta :: Map.Map (ParserState, Maybe MyChar) (Set.Set ParserState)
        }
    deriving (Show)
