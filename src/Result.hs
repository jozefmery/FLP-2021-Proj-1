{-
  File:     Result.hs
  Author:   Jozef Méry - xmeryj00@vutbr.cz
  Project:  FLP-2021-xmeryj00-simplify-bkg
  Date:     20.2.2021
  Description: Simple Maybe-like monad with an error message attached to "Nothing".
-}

module Result( Result(..) ) where

--- imports

import Control.Monad( liftM, ap )

--- imports

-- Maybe-like Monad with an error message string instead of nothing.
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