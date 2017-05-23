{-# LANGUAGE UnicodeSyntax, TypeFamilies, ScopedTypeVariables #-}

module Data.Vector.InsertionSort where

import Prelude hiding (length,read)
import Control.Monad
import Data.Functor
import Control.Monad.ST
import Control.Monad.Loops
import Data.STRef
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as V
import           Data.Vector.Generic ((!))
import           Data.Vector.Generic.Mutable as VM

import Test.QuickCheck

ref = newSTRef
deref = readSTRef
($=) var val = writeSTRef var val

sortM ∷ (MVector v a, Ord a) => v s a → ST s ()
sortM vec = do
    forM_ [1 .. (length vec - 1)] $ \j → do
        key ← read vec j
        iV ← ref (j - 1)
        let done = do i ← deref iV
                      if (i < 0) then return True
                                 else (<= key) <$> read vec i
        whileM_ (not <$> done) $ do i ← deref iV
                                    read vec i >>= write vec (i+1)
                                    iV $= (i-1)
        deref iV >>= \v → write vec (v+1) key

sort ∷ (V.Vector v a, V.Vector w a, Ord a, V.Mutable v~V.Mutable w) => w a → v a
sort v = runST $ do
  mv ← V.thaw v
  sortM mv
  V.freeze mv

isSorted ∷ [Int] → Bool
isSorted [] = True
isSorted [x] = True
isSorted (a:b:cs) = a <= b && isSorted(b:cs)

sortWorks ∷ [Int] → Bool
sortWorks = isSorted . VB.toList . sort . VB.fromList

test = do
  quickCheck sortWorks
  quickCheckWith stdArgs { maxSuccess = 5000 } sortWorks
