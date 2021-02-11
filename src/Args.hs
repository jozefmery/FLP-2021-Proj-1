{-
    File:     Args.hs
    Author:   Jozef Méry - xmeryj00@vutbr.cz
    Project:  FLP-2021-xmeryj00-simplify-bkg
    Date:     9.2.2021
    Description: TODO  
-}

module Src.Args where

--- imports ---

-- GetOpt code adopted from official docs
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Console-GetOpt.html
import System.Console.GetOpt
import System.Environment( getArgs )

--- imports ---

-- TODO
data Options = Options 
  { help :: Bool
  , internal :: Bool
  , step1 :: Bool
  , step2 :: Bool 
  , input :: Maybe FilePath
  } deriving Show

-- TODO
defaultOptions :: Options
defaultOptions = Options
  { help = False
  , internal = False
  , step1 = False
  , step2 = False
  , input = Nothing 
  }

-- TODO
usage :: String
usage = "Usage: simplify-bkg {-h | -i | -1 | -2} [<input_file>]"

-- TODO
optionsTransformer :: [OptDescr (Options -> Options)]
optionsTransformer = 
  [ Option ['h'] ["help"] (NoArg (\ opts -> opts { help = True })) "h"
  , Option ['i'] [] (NoArg (\ opts -> opts { internal = True })) "TODO i"
  , Option ['1'] [] (NoArg (\ opts -> opts { step1 = True })) "1"
  , Option ['2'] [] (NoArg (\ opts -> opts { step2 = True })) "2"
  ]

options' :: [String] -> IO (Options, [String])
options' argv =
  case getOpt Permute optionsTransformer argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo usage optionsTransformer))
    
options :: IO Options
options = do
  argv <- getArgs
  (flags, args) <- options' argv
  return $ setInputPath flags args

setInputPath :: Options -> [String] -> Options
setInputPath opts [] = opts 
setInputPath opts (first:_) = opts{ input = Just first }