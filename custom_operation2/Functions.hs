{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Functions where

import qualified ConCat.Syntactic as Syn
import           ConCat.AltCat

import Prelude hiding (uncurry, curry)
import qualified Prelude as P

class FooCat k  where barC :: Prod k Int Int `k` Int

instance FooCat (->)  where barC = uncurry (+)
instance FooCat Syn.Syn  where barC = Syn.app0 "Bar"


bar :: FooCat k => (Prod k Int Int `k` Int)
bar = barC
{-# INLINE [0] bar #-}
{-# RULES "reveal op0" reveal bar = barC #-}


-- this results in:
-- ccc residuals: [ccc @ Syn @ Int @ (Int -> Int) baz]
-- but if this function is in main it works fine

-- baz :: Int -> Int -> Int
-- baz = curry bar
-- {-# NOINLINE  baz #-}

-- This is fine, the 'curry' ends up in the category returned from ccc
-- e.g. rendered as: 'uncurry (curry "Bar")'
baz :: (ClosedCat k, FooCat k, Ok k Int) => Int `k` (Exp k Int Int)
baz = curry bar
{-# NOINLINE baz #-}
