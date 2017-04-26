{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Main where

import           ConCat.AltCat
import           ConCat.Syntactic (render)

import           Functions
import           Prelude hiding ((.), id, curry, uncurry, const, show)

import qualified Prelude as P


program :: Int -> Int -> String
program x y = show (x + y)

main :: IO ()
main = do
   print $ render (ccc program)
--  "curry (Show . addC)"

   print $ ccc program 10 20
-- "30"
