{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Main where

import           ConCat.AltCat
import           ConCat.Syntactic (render)
import qualified ConCat.Syntactic as Syn


import           Prelude hiding ((.), id, curry, uncurry, const)
import qualified Prelude as P

class FooCat k  where fooC :: Int `k` Int

instance FooCat (->)  where fooC = (+3)
instance FooCat Syn.Syn  where fooC = Syn.app0 "Foo"

foo :: FooCat k => (Int `k` Int)
foo = fooC
{-# INLINE [0] foo #-}
{-# RULES "reveal op0" reveal foo = fooC #-}

program :: Int -> Int -> Int
program x y = foo (x + y)


main :: IO ()
main =  print $ render (ccc program)
-- Should be:
-- "curry (Foo . addC)"
--
-- But inlining of 'foo' occurs too early
-- "curry (addC . (id &&& const 3) . addC)"
