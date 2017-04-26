{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Main where

import           ConCat.AltCat
import           ConCat.Syntactic (render)

import           Functions
import           Prelude hiding ((.), id, curry, uncurry, const)

import qualified Prelude as P


program :: Int -> Int -> Int
program x y = foo (x + y)

main :: IO ()
main =  print $ render (ccc program)
-- "curry (Foo . addC)"
