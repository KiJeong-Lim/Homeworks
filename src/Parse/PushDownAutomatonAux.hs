module Parse.PushDownAutomatonAux where

import Lex.FiniteAutomatonAux (MyChar, ParserState, ExitNumber)

type Nonterminal = MyChar

type Terminal = Int

type ProductionRule = (Terminal, [Symbol])

type ParserStackSymbol = Int

type NPDA = GNPDA ParserState ParserStackSymbol

type PDA_config = (ParserStackSymbol, [MyChar], [ParserStackSymbol])

data Symbol
    = NS Nonterminal
    | TS Terminal
    deriving (Eq, Ord, Show)

data CFG
    = CFG
        { cfg_V_N :: Set.Set Nonterminal
        , cfg_V_T :: Set.Set Terminal
        , cfg_P :: [ProductionRule]
        , cfg_S :: Nonterminal
        }
    deriving (Show)

data GNPDA state stack_symbol
    = NPDA
        { npda_Q :: Set.Set state
        , npda_Sigma :: Set.Set MyChar
        , npda_Gamma :: Set.Set stack_symbol
        , npda_delta :: Map.Map (state, Maybe MyChar, stack_symbol) (Set.Set (state, [stack_symbol]))
        , npda_q0 :: state
        , npda_Z0 :: stack_symbol
        , npda_F :: Set.Set state
        }
    deriving (Show)
