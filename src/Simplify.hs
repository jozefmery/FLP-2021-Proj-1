{-
  File:     Simplify.hs
  Author:   Jozef Méry - xmeryj00@vutbr.cz
  Project:  FLP-2021-xmeryj00-simplify-bkg
  Date:     20.2.2021
  Description:  Library for simplifying text-represented context-free
                grammar based on formal language theory algorithms.
-}

--- extensions ---
{-# LANGUAGE NamedFieldPuns #-}
--- extensions ---

module Simplify 
  ( loadGrammar
  , Grammar(..)
  ) where

--- imports ---
import Data.Set 
  ( Set
  , fromList
  , toList
  , union
  )

import Data.List ( intersperse )

import Result 
  ( Result(..)
  , (<:) 
  )

--- imports ---

-- Tuple alias representing a context-free grammar rule N -> (N u T)*,
-- where an empty string (Epsilon) is represented by the '#' character.
type Rule = (Char, String)

-- Grammar data structure based on the formal grammar
-- definition G = (N, T, S, P).
data Grammar = Grammar{ ns  :: Set Char
                      , ts  :: Set Char
                      , s   :: Char
                      , rs  :: Set Rule
                      }

-- Define Grammar conversion to string. Same format is used as the input.
instance Show Grammar where
  show Grammar{ ns, ts, s, rs } = 
    addCommas ( toList ns ) ++ "\n" ++
    addCommas ( toList ts ) ++ "\n" ++
    [s]                     ++ "\n" ++
    concatMap (\(n, nts) -> n:"->" ++ nts ++ "\n") (toList rs) 

-- Adds commas between each character in a string e.g.: "ABC" -> "A,B,C".
-- Used for stringifying non-/terminals in a grammar.
addCommas :: String -> String
addCommas = intersperse ','

-- Filters a given character from a string.
filterChar :: Char -> String -> String
filterChar c s = [x | x <- s, x /= c]

-- Filters commas from a string.
-- Used for transforming input non-/terminals to internal representation.
filterComma :: String -> String
filterComma = filterChar ','

-- Checks if every character of a string is an element of another string.
-- Returns original string on success, or a customizable error message otherwise.
-- Used for checking input non-/terminals.
checkSymbols :: String -> (Char -> String) -> String -> Result String
checkSymbols _ _ "" = Ok ""
checkSymbols sym err (c:cc) | c `elem` sym = checkSymbols sym err cc >> Ok (c:cc)
                            | otherwise = Err $ err c

-- Invokes comma filtering on the input non-/terminals list (string),
-- and a custom checker. On success, returns a set of symbols (chars).
parseSymbols :: (String -> Result String) -> String -> Result (Set Char)
parseSymbols checker sym = checker (filterComma sym) >>= \n -> Ok $ fromList n

-- Simple helper for error message formatting.
formatError :: String -> String
formatError e = "Error in input grammar: " ++ e

-- Custom error for non-terminal symbols supplied to checkSymbols.
invalidNonTerminalError :: Char -> String 
invalidNonTerminalError c = formatError "invalid non-terminal: " ++ [c]

-- Partially invoked checkSymbols with capital letters as the valid set
-- and a custom error function for non-terminals. The last missing parameter
-- is the list of non-terminals to check.
checkNonTerminals :: String -> Result String
checkNonTerminals = checkSymbols ['A'..'Z'] invalidNonTerminalError 

-- Partially invoked parseSymbols with checkNonTerminals being the checker,
-- and the missing parameter being the list of non-terminals to parse.
parseNonTerminals :: String -> Result (Set Char)
parseNonTerminals = parseSymbols checkNonTerminals 

-- Parses non-terminals in a raw Grammar-like tuple structure.
-- If parsing is successful, returns a modified tuple. The rest of the tuple
-- elements are unchanged. This approach in all set* functions allows a parsing
-- pipeline with elegant error handling allowed by the Result monad. Tuples are
-- used for input and output to allow varying types between stages, representing a
-- semi-constructed Grammar with potential errors ahead. If successful, setRules returns
-- the fully constructed Grammar.
setNonTerminals :: (String, String, String, [String]) -> Result (Set Char, String, String, [String])
setNonTerminals (ns, ts, s, rs) = parseNonTerminals ns >>= \parsed -> Ok (parsed, ts, s, rs)

-- Custom error for terminal symbols supplied to checkSymbols.
invalidTerminalError :: Char -> String 
invalidTerminalError c = formatError "invalid terminal: " ++ [c]

-- Partially invoked checkSymbols with lower-case letters as the valid set
-- and a custom error function for terminals. The last missing parameter
-- is the list of terminals to check.
checkTerminals :: String -> Result String
checkTerminals = checkSymbols ['a'..'z'] invalidTerminalError 

-- Partially invoked parseSymbols with checkTerminals being the checker,
-- and the missing parameter being the list of non-terminals to parse.
parseTerminals :: String -> Result (Set Char)
parseTerminals = parseSymbols checkTerminals 

-- Parses non-terminals in a raw Grammar-like tuple structure.
-- See setNonTerminals for more details.
setTerminals :: (Set Char, String, String, [String]) -> Result (Set Char, Set Char, String, [String])
setTerminals (ns, ts, s, rs) = parseTerminals ts >>= \parsed -> Ok (ns, parsed, s, rs)

-- Parses the starting symbol in a raw Grammar-like tuple structure.
-- The starting symbol needs to be in the set of non-terminals.
-- See setNonTerminals for more details.
setStart :: (Set Char, Set Char, String, [String]) -> Result (Set Char, Set Char, Char, [String])
setStart (ns, ts, s, rs)  | length s == 1 && head s `elem` ns = Ok(ns, ts, head s, rs)
                          | otherwise = Err $ formatError "invalid starting symbol: " ++ s

-- Returns a list of characters, which are not valid (not inside the valid set).
-- Used for finding rules with invalid right sides.
invalidRuleSymbols :: Set Char -> String -> String
invalidRuleSymbols valid s = [c | c <- s, c `notElem` valid]

-- Checks the left side of a grammar rule.
checkRuleLeft :: (Set Char, Set Char, String) -> Result (Set Char, Set Char, String)
checkRuleLeft all@(ns, _, r@(left:_))
  | left `notElem` ns = Err $ formatError "invalid left side: " ++ [left] ++ " in rule: " ++ r
  | otherwise         = Ok all

-- Checks the right side of a grammar rule.
checkRuleRight :: (Set Char, Set Char, String) -> Result (Set Char, Set Char, String)
checkRuleRight all@(ns, ts, r@(_:_:_:right)) 
  | null right          = Err $ formatError "empty right side in rule: " ++ r
  | right == "#"        = Ok all -- special case, empty string
  | not $ null invalid  = Err $ formatError "invalid symbols: " ++ show invalid ++ " in rule: " ++ r
  | otherwise           = Ok all
  where -- avoid horrendous duplication
    invalid = invalidRuleSymbols (ns `union` ts) right

-- Parses a single rule in a raw Grammar-like tuple structure (no initial symbol).
parseRule :: (Set Char, Set Char, String) -> Result Rule
parseRule all@(_, _, left:'-':'>':right) = checkRuleLeft all >>= checkRuleRight >> Ok (left, right)
parseRule (_, _, r) = Err $ formatError "invalid rule: " ++ r 

-- Parses all rules in a raw Grammar-like tuple structure (no initial symbol).
parseRules :: (Set Char, Set Char, Char, [String]) -> Result [Rule]
parseRules (_, _, _, []) = Ok []
parseRules (ns, ts, s, r:rs) = parseRule (ns, ts, r) <: parseRules (ns, ts, s, rs)

-- Invokes rule parser and constructs complete Grammar if successful.
setRules :: (Set Char, Set Char, Char, [String]) -> Result Grammar
setRules g@(ns, ts, s, _) = parseRules g >>= \parsed -> Ok $ Grammar ns ts s $ fromList parsed

-- Invokes each stage of the grammar parsing pipeline.
parseGrammar :: [String] -> Result Grammar
parseGrammar []           = Err "Empty input"
parseGrammar [_]          = Err "Grammar missing alphabet"
parseGrammar [_, _]       = Err "Grammar missing initial symbol"
parseGrammar (ns:ts:s:rs) = setNonTerminals (ns, ts, s, rs) >>= setTerminals >>= setStart >>= setRules

-- Reads a whole file if given some input,
-- or stdin if none.
readInput :: Maybe FilePath -> IO String
readInput (Just input) = readFile input
readInput Nothing = getContents

-- Filters empty strings from a list.
filterEmptyStrings :: [String] -> [String]
filterEmptyStrings ss = [s | s <- ss, s /= ""]

-- Loads grammar from file or stdin.
-- Empty lines are ignored.
loadGrammar :: Maybe FilePath -> IO(Result Grammar)
loadGrammar input = do
  content <- readInput input
  return $ parseGrammar $ filterEmptyStrings $ lines content