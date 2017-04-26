{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Main where

import           ConCat.AltCat
import           ConCat.Syntactic (render)
import qualified ConCat.Syntactic as Syn


import           Prelude hiding ((.), id, curry, uncurry, const)
import qualified Prelude as P

class FooCat k  where foo_ :: Int `k` Int

instance FooCat (->)  where foo_ = (+3)
instance FooCat Syn.Syn  where foo_ = Syn.app0 "Foo"

foo :: FooCat k => (Int `k` Int)
foo = foo_
{-# INLINE [0] foo #-}
{-# RULES "reveal op0" reveal foo = foo_ #-}

program :: Int -> Int -> Int
program x y = foo (x + y)


main :: IO ()
main =  print $ render (ccc program)
-- Should be:
-- "curry (Foo . addC)"
--
-- But inlining of 'foo' occurs too early
-- "curry (addC . (id &&& const 3) . addC)"
