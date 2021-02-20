{-
  File:     Simplify.hs
  Author:   Jozef Méry - xmeryj00@vutbr.cz
  Project:  FLP-2021-xmeryj00-simplify-bkg
  Date:     20.2.2021
  Description: TODO  
-}

module Simplify ( loadGrammar ) where

data Grammar = Grammar{ n :: String
                      , t :: String
                      , s :: Char
                      , p :: [String]
                      }

readInput :: Maybe FilePath -> IO String
readInput (Just input) = readFile input
readInput Nothing = getContents

-- loadGrammar :: Maybe FilePath -> Grammar
loadGrammar :: Maybe FilePath -> IO ()
loadGrammar input = do
  content <- readInput input
  print $ lines content