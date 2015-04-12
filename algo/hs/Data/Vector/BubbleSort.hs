{-# LANGUAGE UnicodeSyntax, TypeFamilies #-}

module Data.Vector.BubbleSort where

import Prelude hiding (length,read)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Loops
import Data.STRef
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as V
import           Data.Vector.Generic.Mutable as VM
import Test.QuickCheck

ref = newSTRef
deref = readSTRef
($=) var val = writeSTRef var val

sortM ∷ (MVector v a, Ord a) => v s a → ST s ()
sortM vector = do
  swapped ← ref True
  whileM_ (deref swapped) $ do
    swapped $= False
    forM_ [0 .. (length vector-2)] $ \i → do
      v0 ← read vector (i+0)
      v1 ← read vector (i+1)
      when (v0 > v1) $ do
        swap vector i (i+1)
        swapped $= True

sort ∷ (V.Vector v a, V.Vector w a, Ord a, V.Mutable v~V.Mutable w) => w a → v a
sort v = runST $ do
  mv ← V.thaw v
  sortM mv
  V.freeze mv

isSorted ∷ Ord a ⇒ [a] → Bool
isSorted [] = True
isSorted [x] = True
isSorted (a:b:cs) = a <= b && isSorted(b:cs)

sortWorks ∷ [Int] → Bool
sortWorks = isSorted . VB.toList . sort . VB.fromList

test = do
  quickCheck sortWorks
  quickCheckWith stdArgs { maxSuccess = 5000 } sortWorks
