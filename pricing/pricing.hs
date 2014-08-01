{-# LANGUAGE UnicodeSyntax, OverloadedLists, TypeFamilies, NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification, BangPatterns #-}

-- TODO Make an object that maps to [PricePt], but it stored as Vec Double
--   pricePart should return one of these.
--   The implicit unit counts associated with the result of pricePt suck.

import Prelude.Unicode
import Control.Monad.ST
import Data.Functor ((<$>))
-- import Data.Vector ((!))
import Data.Vector.Unboxed ((!), Unbox)
import Debug.Trace
import GHC.Exts
import Graphics.Gnuplot.Simple
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import qualified Data.Vector.Unboxed as UVec

type Vec a = Vec.Vector a
type MVec a = MVec.MVector a
type UVec a = UVec.Vector a

-- instance IsList (Vec.Vector a) where
  -- type Item (Vec a) = a
  -- fromList = Vec.fromList
  -- toList = Vec.toList


type PricePt = (Int, Double)
data Params = Params {xmax∷Int, resolution∷Int} deriving Show
type BOM = Vec (Int, Vec PricePt)


-- TODO Test this.
-- TODO For ‘interpolate n [0..n]’ we output ‘[1..n]’ make this clear.
interpolate ∷ Unbox a ⇒ Int → UVec a → UVec a
interpolate n v = UVec.generate n (\i → v!floor(inc*f i))
  where inc = f (UVec.length v) / f n
        f = fromIntegral ∷ (Int → Double)

scale ∷ Num a ⇒ a → Vec a → Vec a
scale x v = Vec.map (x*) v

priceAt ∷ UVec Int → UVec Double → (UVec Double → Double)
priceAt units prices memo = minimize $ UVec.zipWith f units prices
	where f u c = priceOfferAt u c memo
	      minimize = UVec.foldl' min (1/0)

priceOfferAt ∷ Int → Double → UVec Double → Double
priceOfferAt units price memo =
	let buying = UVec.length memo in
	let remaining = buying-units in
	if buying≡0 then 0 else
  if remaining≤0 then 0 else
  memo!remaining

pricePart ∷ Params → UVec Int → UVec Double → UVec Double
pricePart p units prices = UVec.constructN (xmax p) $ priceAt units prices

unitPrices ∷ UVec Double → UVec Double
unitPrices v = UVec.generate (UVec.length v) e
	where e ∷ Int → Double
	      e 0 = 0
	      e i = (v!i) / (fromIntegral i)

boom ∷ Vec PricePt → (UVec Int, UVec Double)
boom pts = (units, prices)
	where units = UVec.fromList $ map fst $ Vec.toList pts
	      prices = UVec.fromList $ map snd $ Vec.toList pts

main = do let e@(ps,(q,offers)) = slowExample 3
              (units,prices) = boom offers
              p = pricePart ps units prices
          print $ UVec.length p


-- Testing
testPlot p = plotList [YRange(0,1.1),PNG("test.png")] l
  where l = drop 1 $ UVec.toList $ unitPrices p

example = pricePart params units price
  where params = Params{xmax=200, resolution=200}
        (units,price) = boom [(1,1), (50,25)]

genOffers ∷ Int → Int → Int → Vec PricePt
genOffers r n i = Vec.generate n e
  where f = fromIntegral
        e j = (1+i+j*(r+n), 1+f(i+j*(r+n))/f j) ∷ PricePt

(???) a b = Vec.unsafeIndex a b

slowExample ∷ Int → (Params, (Int,Vec PricePt))
slowExample r = ( Params{xmax=1000000, resolution=100}
                , (Vec.generate 1 $ \i → (i `mod` 3, genOffers r 400 i))???0
                )
