{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Functions where

import qualified ConCat.Syntactic as Syn
import           ConCat.AltCat

import Prelude hiding (uncurry, curry, show)
import qualified Prelude as P

class ShowCat k  where showC :: Show a => a `k` String

instance ShowCat (->)  where showC = P.show
instance ShowCat Syn.Syn  where showC = Syn.app0 "Show"

show :: (ShowCat k, Show a) => (a `k` String)
show = showC
{-# INLINE [0] show #-}

{-# RULES "reveal op0" reveal show = showC #-}
