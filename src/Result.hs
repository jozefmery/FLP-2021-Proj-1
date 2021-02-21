{-
  File:     Result.hs
  Author:   Jozef Méry - xmeryj00@vutbr.cz
  Project:  FLP-2021-xmeryj00-simplify-bkg
  Date:     20.2.2021
  Description: Simple Maybe-like monad with an error message instead of Nothing.
-}

module Result ( Result(..)
              , (>>!) 
              , (<:)
              ) where

--- imports ---
import Control.Monad( liftM, ap )
--- imports ---

-- Maybe-like Monad with an error message string instead of Nothing.
-- Used throughout the application for error handling in a Rust-lang
-- inspired style.
data Result ok = Ok ok | Err String
  deriving (Eq, Ord, Read, Show)

-- Functor and Applicative required by Monad instance.
instance Functor Result where
  fmap = liftM

instance Applicative Result where
  pure  = return
  (<*>) = ap

-- Make Result "monadic".
-- Implement common operations. 
instance Monad Result where
  (Ok ok) >>= f     = f ok
  Err e   >>= _     = Err e
  res >> f          = res >>= const f
  return            = Ok

-- >>= style extractor except for the error message.
(>>!) :: Result a -> (String -> Result a) -> Result a
Err e >>! f = f e
Ok a >>! _ = Ok a

-- Operator for pushing a Result value into a Result list.
(<:) :: Result a -> Result [a] -> Result [a]

-- earlier error takes precedence
Err e <: _ = Err e 
_ <: Err e = Err e

Ok a <: Ok as = Ok (a:as)