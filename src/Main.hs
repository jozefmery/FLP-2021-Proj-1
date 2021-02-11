{-
    File:     Main.hs
    Author:   Jozef Méry - xmeryj00@vutbr.cz
    Project:  FLP-2021-xmeryj00-simplify-bkg
    Date:     9.2.2021
    Description: TODO  
-}

module Main where

--- imports ---

-- GetOpt code adopted from official docs
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Console-GetOpt.html
import System.Console.GetOpt
import System.Environment

import Src.Args

--- imports ---

run :: Options -> IO()
run Options{ help = True } = putStrLn $ usageInfo usage optionsTransformer

run Options { internal = True
            , step1 = False
            , step2 = False
            } = 
              print "internal" 

run Options { internal = False
            , step1 = True
            , step2 = False
            } = 
              print "step1" 

run Options { internal = False
            , step1 = False
            , step2 = True
            } = 
              print "step2"

run Options{} = do
  putStrLn "Invalid parameters"
  putStrLn usage

main :: IO ()
-- main = 
main = do
  opts <- options
  print opts
  run opts