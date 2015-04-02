{-# LANGUAGE UnicodeSyntax #-}

module Data.Vector.BubbleSort where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Loops
import Data.STRef
import qualified Data.Vector.Unboxed as Vec
import Data.Vector.Unboxed.Mutable as MVec
import Test.QuickCheck

sortM ∷ Unbox a ⇒ Ord a ⇒ MVector s a → ST s ()
sortM vector = do
  swapped ← newSTRef True
  whileM_ (readSTRef swapped) $ do
    writeSTRef swapped False
    forM_ [0 .. (MVec.length vector-2)] $ \i → do
      v0 ← MVec.read vector (i+0)
      v1 ← MVec.read vector (i+1)
      when (v0 > v1) $ do
        MVec.swap vector i (i+1)
        writeSTRef swapped True

sort ∷ (Ord a, Unbox a) ⇒ Vec.Vector a → Vec.Vector a
sort v = runST $ do
  mv ← Vec.thaw v
  sortM mv
  Vec.freeze mv

isSorted ∷ Ord a ⇒ [a] → Bool
isSorted [] = True
isSorted [x] = True
isSorted (a:b:cs) = a <= b && isSorted(b:cs)

sortWorks ∷ [Int] → Bool
sortWorks = isSorted . Vec.toList . sort . Vec.fromList

test = do
  quickCheck sortWorks
  quickCheckWith stdArgs { maxSuccess = 5000 } sortWorks
