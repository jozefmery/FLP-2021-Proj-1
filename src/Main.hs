{-
  File:     Main.hs
  Author:   Jozef Méry - xmeryj00@vutbr.cz
  Project:  FLP-2021-xmeryj00-simplify-bkg
  Date:     9.2.2021
  Description: Program entry point. Loads command-line arguments, executes requested
               actions, prints results, and handles errors.  
-}

--- extensions ---
{-# LANGUAGE NamedFieldPuns #-}
--- extensions ---

module Main where

--- imports ---
import Args
  ( Options(..)
  , fullUsage 
  , options
  )

import System.Exit
  ( exitSuccess
  , exitFailure
  )

import Result( Result(..) )

import Simplify -- TODO

--- imports ---

printError :: String -> IO()
printError err = putStrLn $ "Error: " ++ err

run :: Result Options -> IO()
run (Err err) = do
  printError err
  exitFailure

run (Ok Options{ help = True }) = do
  putStrLn fullUsage
  exitSuccess

run (Ok Options
          { internal  = True
          , step1     = False
          , step2     = False
          , input     
          }) = do
  loadGrammar input -- TODO
  exitSuccess

run (Ok Options 
          { internal  = False
          , step1     = True
          , step2     = False
          }) = do
  print "step1" -- TODO
  exitSuccess

run (Ok Options
          { internal  = False
          , step1     = False
          , step2     = True
          }) = do
  print "step2" -- TODO
  exitSuccess

-- handle invalid flag combination (multiple flags or none)
run (Ok Options{}) = do
  printError "Invalid flag combination (use exactly one)"
  putStrLn fullUsage
  exitFailure

main :: IO()
-- get options from command-line arguments and run the program
main = options >>= run