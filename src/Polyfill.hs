{-
  File:     Polyfill.hs
  Author:   Jozef Méry - xmeryj00@vutbr.cz
  Project:  FLP-2021-xmeryj00-simplify-bkg
  Date:     3.3.2021
  Description: Contains polyfills for older compiler versions as the ghc used during development was
               version 8.6.5  

-}

module Polyfill ( (<&>) ) where

(<&>) :: Functor f => f a -> (a -> b) -> f b 
(<&>) = flip fmap