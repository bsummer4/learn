{-# LANGUAGE UnicodeSyntax, OverloadedLists, TypeFamilies, NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}

-- TODO Make an object that maps to [PricePt], but it stored as Vec Double
--   pricePart should return one of these.
--   The implicit unit counts associated with the result of pricePt suck.

import Control.Monad.ST
import Data.Functor ((<$>))
import Data.List (foldl')
import Data.Vector ((!))
import Debug.Trace
import GHC.Exts
import Graphics.Gnuplot.Simple
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import qualified Data.Vector.Unboxed as UVec

type Vec a = Vec.Vector a
type MVec a = MVec.MVector a
type UVec a = UVec.Vector a

instance IsList (Vec.Vector a) where
  type Item (Vec a) = a
  fromList = Vec.fromList
  toList = Vec.toList


type PricePt = (Int, Double)
data Params = Params {xmax∷Int, resolution∷Int} deriving Show
type BOM = Vec (Int, Vec PricePt)


(?=) (v,i) x = MVec.write v i x
(?) v i = MVec.read v i

scale ∷ Num a ⇒ a → Vec a → Vec a
scale x v = (x*) <$> v

minimumV ∷ Ord a ⇒ a → Vec a → a
minimumV = Vec.foldl' min

forE ∷ Int → (Int → ST s ()) → ST s ()
forE n m = loop 0
  where loop i | i>=n = return ()
        loop i        = m i >> loop (i+1)

mapE ∷ (a → ST s b) → Vec a → ST s (Vec b)
mapE m v = do
  buf ← MVec.new $ Vec.length v
  forE (Vec.length v) $ \i → do
    elt ← m (v!i)
    (buf,i) ?= elt
  Vec.freeze buf

-- TODO Test.
interpolate ∷ Int → Vec a → Vec a
interpolate n v = Vec.generate n (\i → v!floor(inc*f i))
  where inc = f (Vec.length v) / f n
        f = fromIntegral ∷ (Int → Double)

memoGenerate ∷ Int → ((Int→ST s a) → Int → ST s a) → ST s (Vec a)
memoGenerate n f = do
  result ← MVec.new n
  forE n $ \i → do v ← f (result?) i
                   (result,i) ?= v
  Vec.freeze result

priceAt ∷ Vec PricePt → (Int → ST s Double) → Int → ST s Double
priceAt pts memo n = do
  prices ← mapE (\p → priceOfferAt p memo n) pts
  return $ minimumV (1/0) prices

tr a b = traceShow (a,b) b

priceOfferAt ∷ PricePt → (Int → ST s Double) → Int → ST s Double
priceOfferAt (units,cost) memo 0 = return 0
priceOfferAt (units,cost) memo buying = do
  let stillNeed = (buying-units)
  costOfRemainingParts ← if stillNeed<=0 then return 0 else memo stillNeed
  return $ cost + costOfRemainingParts

pricePart ∷ Params → Vec PricePt → Vec Double
pricePart p prices = runST $ memoGenerate (xmax p) $ priceAt prices

example = pricePart params offers
  where params = Params{xmax=200, resolution=200}
        offers = [(1,1), (50,25)]

unitPrices ∷ Vec Double → Vec Double
unitPrices v = Vec.generate (Vec.length v) e
	where e 0 = 0
	      e i = (v!i)/(fromIntegral i)

main = do let e@(ps,(q,offers)) = slowExample 3
              p = pricePart ps offers
          print $ Vec.length p
--        plotList [PNG("test.png")] (drop 1 $ Vec.toList $ unitPrices p)
--        plotList [YRange(0,1.1),PNG("test.png")] (drop 1 $ Vec.toList $ unitPrices p)

genOffers ∷ Int → Int → Int → Vec PricePt
genOffers r n i = Vec.generate n e
  where f = fromIntegral
        e j = (1+i+j*(r+n), 1+f(i+j*(r+n))/f j) ∷ PricePt

slowExample ∷ Int → (Params, (Int,Vec PricePt))
slowExample r = ( Params{xmax=100000, resolution=100000}
                , (Vec.generate 1 $ \i → (i `mod` 3, genOffers r 400 i))!0
                )
