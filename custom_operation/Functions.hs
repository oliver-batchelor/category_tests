{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Functions where

import qualified ConCat.Syntactic as Syn
import           ConCat.AltCat

import Prelude hiding (uncurry, curry)
import qualified Prelude as P

class FooCat k  where foo_ :: Int `k` Int

instance FooCat (->)  where foo_ = (+3)
instance FooCat Syn.Syn  where foo_ = Syn.app0 "Foo"

foo :: FooCat k => (Int `k` Int)
foo = foo_
{-# INLINE [0] foo #-}
{-# RULES "reveal op0" reveal foo = foo_ #-}
