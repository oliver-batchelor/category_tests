{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Functions where

import qualified ConCat.Syntactic as Syn
import           ConCat.AltCat

import Prelude hiding (uncurry, curry)
import qualified Prelude as P

class FooCat k  where bar_ :: Prod k Int Int `k` Int

instance FooCat (->)  where bar_ = uncurry (+)
instance FooCat Syn.Syn  where bar_ = Syn.app0 "Bar"


bar :: FooCat k => (Prod k Int Int `k` Int)
bar = bar_
{-# INLINE [0] bar #-}
{-# RULES "reveal op0" reveal bar = bar_ #-}


-- this results in:
-- ccc residuals: [ccc @ Syn @ Int @ (Int -> Int) baz]
-- but if this function is in main it works fine

-- baz :: Int -> Int -> Int
-- baz = curry bar
-- {-# NOINLINE  baz #-}

-- This is fine, the 'curry' ends up in the category returned from ccc
baz :: (ClosedCat k, FooCat k, Ok k Int) => Int `k` (Exp k Int Int)
baz = curry bar
{-# NOINLINE baz #-}
