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
{-# LANGUAGE TupleSections #-}
--- extensions ---

module Simplify 
  ( loadGrammar
  , Grammar(..)
  , simplifyGrammar1
  , simplifyGrammar2
  ) where

--- imports ---
import qualified Data.Set as Set
  ( Set
  , fromList
  , toList
  , union
  , empty
  , isSubsetOf
  , filter
  , map
  , member
  , insert
  , singleton
  , intersection
  , unions
  , notMember
  )

import Data.List ( intersperse )

import Result 
  ( Result(..)
  , (<:) 
  )

import Data.Functor ( (<&>) )
--- imports ---

-- Tuple alias representing a context-free grammar rule N -> (N u T)*,
-- where an empty string (Epsilon) is represented by the '#' character.
type Rule = (Char, String)

ruleToStr :: Rule -> String
ruleToStr (l, r) = l:"->" ++ r

-- Grammar data structure based on the formal grammar
-- definition G = (N, T, S, P).
data Grammar = Grammar{ ns  :: Set.Set Char
                      , ts  :: Set.Set Char
                      , s   :: Char
                      , rs  :: Set.Set Rule
                      }

-- Define Grammar conversion to string. Same format is used as the input.
instance Show Grammar where
  show Grammar{ ns, ts, s, rs } = unlines $ [addCommas ( Set.toList ns ), addCommas ( Set.toList ts ), [s]] 
                                            ++ map ruleToStr ( Set.toList rs ) 

-- Adds commas between each character in a string e.g.: "ABC" -> "A,B,C".
-- Used for stringifying non-/terminals in a grammar.
addCommas :: String -> String
addCommas = intersperse ','

-- Filters commas from a string.
-- Used for transforming input non-/terminals to internal representation.
filterComma :: String -> String
filterComma = filter (/= ',')

-- Returns a list of characters, which are not valid (not inside the valid set).
-- Used for finding rules with invalid right sides and invalid non-/terminals.
invalidSymbols :: Set.Set Char -> String -> String
invalidSymbols valid = filter ( `Set.notMember` valid )

-- Checks if every character of a string is an element of a character set.
-- Returns original string on success, or a customizable error message otherwise.
-- Used for checking input non-/terminals.
checkSymbols :: Set.Set Char -> (String -> String) -> String -> Result String
checkSymbols valid err s  | null invalid = Ok s
                          | otherwise = Err $ err $ addCommas invalid
                          where
                            invalid = invalidSymbols valid s

-- Invokes comma filtering on the input non-/terminals list (string),
-- and a custom checker. On success, returns a set of symbols (chars).
parseSymbols :: (String -> Result String) -> String -> Result (Set.Set Char)
parseSymbols checker sym = checker ( filterComma sym ) <&> Set.fromList 

-- Simple helper for error message formatting.
formatError :: String -> String
formatError = ( "Error in input grammar: " ++ )

-- Custom error for non-terminal symbols supplied to checkSymbols.
invalidNonTerminalError :: String -> String 
invalidNonTerminalError = ( formatError "invalid non-terminals: " ++ )

-- Partially invoked checkSymbols with capital letters as the valid set
-- and a custom error function for non-terminals. The last missing parameter
-- is the list of non-terminals to check.
checkNonTerminals :: String -> Result String
checkNonTerminals = checkSymbols ( Set.fromList ['A'..'Z'] ) invalidNonTerminalError

-- Partially invoked parseSymbols with checkNonTerminals being the checker,
-- and the missing parameter being the list of non-terminals to parse.
parseNonTerminals :: String -> Result (Set.Set Char)
parseNonTerminals = parseSymbols checkNonTerminals 

-- Parses non-terminals in a raw Grammar-like tuple structure.
-- If parsing is successful, returns a modified tuple. The rest of the tuple
-- elements are unchanged. This approach in all set* functions allows a parsing
-- pipeline with elegant error handling allowed by the Result monad. Tuples are
-- used for input and output to allow varying types between stages, representing a
-- semi-constructed Grammar with potential errors ahead. If successful, setRules returns
-- the fully constructed Grammar.
setNonTerminals :: (String, String, String, [String]) -> Result (Set.Set Char, String, String, [String])
setNonTerminals (ns, ts, s, rs) = parseNonTerminals ns <&> (, ts, s, rs)

-- Custom error for terminal symbols supplied to checkSymbols.
invalidTerminalError :: String -> String 
invalidTerminalError = ( formatError "invalid terminals: " ++ )

-- Partially invoked checkSymbols with lower-case letters as the valid set
-- and a custom error function for terminals. The last missing parameter
-- is the list of terminals to check.
checkTerminals :: String -> Result String
checkTerminals = checkSymbols ( Set.fromList ['a'..'z'] ) invalidTerminalError

-- Partially invoked parseSymbols with checkTerminals being the checker,
-- and the missing parameter being the list of non-terminals to parse.
parseTerminals :: String -> Result (Set.Set Char)
parseTerminals = parseSymbols checkTerminals 

-- Parses non-terminals in a raw Grammar-like tuple structure.
-- See setNonTerminals for more details.
setTerminals :: (Set.Set Char, String, String, [String]) -> Result (Set.Set Char, Set.Set Char, String, [String])
setTerminals (ns, ts, s, rs) = parseTerminals ts <&> (ns, , s, rs)

-- Checks if a grammar starting symbol is a single character,
-- and if is inside the non-terminal set.
checkStart :: Set.Set Char -> String -> Result Char
checkStart ns s@[c] 
  | c `Set.member` ns = Ok c
  | otherwise = Err $ formatError "starting symbol " ++ s ++ " not element of non-terminals"

checkStart _ s = Err $ formatError "invalid starting symbol: " ++ s

-- Parses the starting symbol in a raw Grammar-like tuple structure.
-- The starting symbol needs to be in the set of non-terminals.
-- See setNonTerminals for more details.
setStart :: (Set.Set Char, Set.Set Char, String, [String]) -> Result (Set.Set Char, Set.Set Char, Char, [String])
setStart (ns, ts, s, rs) = checkStart ns s <&> (ns, ts, , rs)

-- Checks the left side of a grammar rule.
checkRuleLeft :: (Set.Set Char, Set.Set Char, String) -> Result (Set.Set Char, Set.Set Char, String)
checkRuleLeft a@(ns, _, r@(left:_)) 
  | left `notElem` ns = Err $ formatError "invalid left side: " ++ [left] ++ " in rule: " ++ r
  | otherwise         = Ok a

checkRuleLeft _ = error "Found an invalid rule. This is likely a parsing error." 

-- Checks the right side of a grammar rule.
-- Assumes valid rule format. Validity checked by parseRule.
checkRuleRight :: (Set.Set Char, Set.Set Char, String) -> Result (Set.Set Char, Set.Set Char, String)
checkRuleRight a@(ns, ts, r@(_:_:_:right)) 
  | null right          = Err $ formatError "empty right side in rule: " ++ r
  | right == "#"        = Ok a -- special case, empty string
  | not $ null invalid  = Err $ formatError "invalid symbols: " ++ show invalid ++ " in rule: " ++ r
  | otherwise           = Ok a
  where -- avoid horrendous duplication
    invalid = invalidSymbols ( ns `Set.union` ts ) right

checkRuleRight _ = error "Invalid call"

-- Parses a single rule in a raw Grammar-like tuple structure (no initial symbol).
parseRule :: (Set.Set Char, Set.Set Char, String) -> Result Rule
parseRule a@(_, _, left:'-':'>':right) = checkRuleLeft a >>= checkRuleRight >> Ok (left, right)
parseRule (_, _, r) = Err $ formatError "invalid rule: " ++ r 

-- Parses all rules in a raw Grammar-like tuple structure (no initial symbol).
parseRules :: (Set.Set Char, Set.Set Char, Char, [String]) -> Result [Rule]
parseRules (_, _, _, []) = Ok []
parseRules (ns, ts, s, r:rs) = parseRule (ns, ts, r) <: parseRules (ns, ts, s, rs)

-- Invokes rule parser and constructs complete Grammar if successful.
setRules :: (Set.Set Char, Set.Set Char, Char, [String]) -> Result Grammar
setRules g@(ns, ts, s, _) = parseRules g <&> \parsed -> Grammar ns ts s $ Set.fromList parsed

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
filterEmptyStrings = filter (/= "")

-- Loads grammar from file or stdin.
-- Empty lines are ignored.
loadGrammar :: Maybe FilePath -> IO(Result Grammar)
loadGrammar input = readInput input <&> ( parseGrammar . filterEmptyStrings . lines )

-- Checks if the right side of a rule is a member in
-- an iteration of a char set.
rightSideInIter :: Set.Set Char -> Rule -> Bool
rightSideInIter _ (_, "#") = True -- special epsilon case, valid regardless of alphabet
rightSideInIter set (_, r) = Set.fromList r `Set.isSubsetOf` set

-- Returns a set of non-terminals which generate finite strings.
-- Based on algorithm 4.1 from TIN.
generatingNonTerminals' :: Set.Set Char -> Grammar -> Set.Set Char 
generatingNonTerminals' prev g@Grammar{ ts, rs }  
  | prev == current = current
  | otherwise       = generatingNonTerminals' current g
  where
    current = Set.map fst $ Set.filter (rightSideInIter $ prev `Set.union` ts) rs

-- Wrapper around generatingNonTerminals', which supplies
-- the initial empty set.
generatingNonTerminals :: Grammar -> Grammar
generatingNonTerminals g@Grammar{ s } = g{ ns = Set.insert s $ generatingNonTerminals' Set.empty g }

-- Checks if a given rule is valid within a given grammar.
isRuleValid :: Grammar -> Rule -> Bool
isRuleValid Grammar{ ns, ts } rule@(l, _) = l `Set.member` ns && rightSideInIter ( ns `Set.union` ts ) rule

-- Filters invalid rules from a grammar.
filterGrammarRules :: Grammar -> Grammar
filterGrammarRules g@Grammar{ rs } = g{ rs = Set.filter ( isRuleValid g ) rs }

-- Invokes the appropriate functions to complete
-- the first step of the grammar simplification process.
-- Uses grammar wrapped in results to improve integration
-- in main.
simplifyGrammar1 :: Result Grammar -> Result Grammar
simplifyGrammar1 g = g <&> filterGrammarRules . generatingNonTerminals 

-- Returns a set of non-/terminals, which are accessible through 
-- an arbitrary length sequence of rule applications starting from the
-- starting symbol. Based on algorithm 4.2 from TIN.
availableSymbols' :: Set.Set Char -> Grammar -> Set.Set Char
availableSymbols' prev g@Grammar{ rs } 
  | prev == current = current
  | otherwise       = availableSymbols' current g
  where
    current = prev `Set.union` Set.unions ( Set.map ( Set.fromList . snd ) $ Set.filter (( `Set.member` prev ) . fst ) rs )

-- Invokes availableSymbols' and filter unavailable symbols from grammar.
availableSymbols :: Grammar -> Grammar
availableSymbols g@Grammar{ ns, ts, s } = g{ ns = ns `Set.intersection` v, ts = ts `Set.intersection` v }
  where
    v = availableSymbols' ( Set.singleton s ) g

-- Invokes the appropriate functions to complete
-- the second step of the grammar simplification process.
-- Uses grammar wrapped in results to improve integration
-- in main.
simplifyGrammar2 :: Result Grammar -> Result Grammar

-- handle special case
-- required by assignment
-- this is redundant, second part handles this case correctly
simplifyGrammar2 (Ok g@Grammar{ s, rs }) | rs == Set.empty = Ok g { ns = Set.singleton s, ts = Set.empty }

simplifyGrammar2 g = g <&> filterGrammarRules . availableSymbols