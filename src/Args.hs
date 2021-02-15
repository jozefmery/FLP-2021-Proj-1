{-
    File:     Args.hs
    Author:   Jozef Méry - xmeryj00@vutbr.cz
    Project:  FLP-2021-xmeryj00-simplify-bkg
    Date:     9.2.2021
    Description: Command-line argument parsing into program options.  
-}

module Args
  ( Options(..)
  , usage
  , optionsTransformer
  , options
  ) where

--- imports ---

-- GetOpt code adopted from official docs
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Console-GetOpt.html
import System.Console.GetOpt
  ( getOpt
  , usageInfo
  , ArgDescr(NoArg)
  , ArgOrder(Permute)
  , OptDescr(..)
  )
import System.Environment( getArgs )

--- imports ---

-- Program options structure representing command-line
-- argument options. 
data Options = Options 
  { help      :: Bool
  , internal  :: Bool
  , step1     :: Bool
  , step2     :: Bool 
  , input     :: Maybe FilePath
  } deriving Show

-- Define default options.
-- Default options are invalid (missing a stage).
defaultOptions :: Options
defaultOptions = Options
  { help      = False
  , internal  = False
  , step1     = False
  , step2     = False
  , input     = Nothing 
  }

-- Program usage string.
usage :: String
usage = "Usage: simplify-bkg {-h | -i | -1 | -2} [<input_file>]"

-- GetOpt Options transformer definitions.
optionsTransformer :: [OptDescr (Options -> Options)]
optionsTransformer = 
  [ Option ['h'] ["help"] (NoArg (\ opts -> opts { help = True }))      "Print this help message."
  , Option ['i'] []       (NoArg (\ opts -> opts { internal = True }))  "Print the internal representation of the parsed input grammar."
  , Option ['1'] []       (NoArg (\ opts -> opts { step1 = True }))     "Print the grammar after the first step. "
  , Option ['2'] []       (NoArg (\ opts -> opts { step2 = True }))     "Print the grammar after the second step."
  ]

-- Runs getOpt and fills in the input field of the Options structure if any
-- using the addInputPath function. If multiple are provided,
-- the first is chosen.
options :: IO Options
options = do
  argv <- getArgs
  (opts, args) <- transformOptions $ getOpt Permute optionsTransformer argv
  return $ addInputPath opts args

-- Applies transforms and checks for errors.
transformOptions :: ([Options -> Options], [String], [String]) -> IO (Options, [String])
transformOptions (o, n, []) = do return (foldl (flip id) defaultOptions o, n)
transformOptions (_, _, errs) = ioError (userError (concat errs ++ usageInfo usage optionsTransformer))

-- Adds input path if any.
addInputPath :: Options -> [String] -> Options
addInputPath opts [] = opts
addInputPath opts (first:_) = opts{ input = Just first }