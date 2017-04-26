{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Functions where

import qualified ConCat.Syntactic as Syn
import           ConCat.AltCat

import Prelude hiding (uncurry, curry)
import qualified Prelude as P

class FooCat k  where fooC :: Int `k` Int

instance FooCat (->)  where fooC = (+3)
instance FooCat Syn.Syn  where fooC = Syn.app0 "Foo"

foo :: FooCat k => (Int `k` Int)
foo = fooC
{-# INLINE [0] foo #-}
{-# RULES "reveal op0" reveal foo = fooC #-}
