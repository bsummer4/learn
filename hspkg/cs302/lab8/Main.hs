{-# LANGUAGE RecordWildCards, OverloadedStrings, NamedFieldPuns #-}

module Main where

import           Control.Applicative
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Word                         (Word)
import           Debug.Trace
import           Text.Read
import           Text.Printf


-- Types/Values translated directly from the specification: --------------------

data Direction    = North | South | East | West
                  deriving (Show)

data Orientation  = Street | Avenue
                  deriving (Show)

type Street       = (Orientation,Word)
data Intersection = Intersection { street∷Word, avenue∷Word } deriving (Eq,Ord,Show)
type Location     = (Double, Double) -- Vector2D
newtype Speed     = MPH Double deriving (Show)
newtype Distance  = Miles Double deriving (Show)
newtype Duration  = Seconds Double deriving (Show)
data LightSetup   = LightSetup { aveGreen ∷ Duration, streetGreen ∷ Duration } deriving (Show)
type CitySetup    = [(Intersection, Location, LightSetup)]

speedLimit ∷ Speed
speedLimit = MPH 30

offset ∷ Intersection → Direction → Intersection
offset isect@Intersection{..} North = isect {street = street+1}
offset isect@Intersection{..} South = isect {street = street-1}
offset isect@Intersection{..} East  = isect {avenue = avenue-1}
offset isect@Intersection{..} West  = isect {avenue = avenue+1}

isMulOf ∷ Integral a ⇒ a → a → Bool
isMulOf num divisor = num `div` divisor == 0

streetDirections ∷ Word → [Direction]
streetDirections n | n `isMulOf` 5 = [East,West]
streetDirections n | n `isMulOf` 2 = [West]
streetDirections n                 = [East]

avenueDirections ∷ Word → Word → [Direction]
avenueDirections max n | n == max      = [North,South]
avenueDirections _   n | n `isMulOf` 5 = [North,South]
avenueDirections _   n | n `isMulOf` 2 = [North]
avenueDirections _   n                 = [South]

inputLine ∷ Text → Maybe (Intersection, Location, LightSetup)
inputLine line = case T.words line of
  [stT,aveT,xT,yT,stDurT,aveDurT] → result <$> r stT <*> r aveT  <*> r xT <*> r yT <*> r stDurT <*> r aveDurT
                                      where r :: Read a => Text -> Maybe a
                                            r = readMaybe . T.unpack
                                            result street avenue x y stSecs aveSecs =
                                              ( Intersection{..}
                                              , (x,y)
                                              , LightSetup{streetGreen=Seconds stSecs, aveGreen=Seconds aveSecs}
                                              )
  _                               → trace (printf "Invalid line: '%s'\n" (T.unpack line))
                                      Nothing

inputCity ∷ Text → CitySetup
inputCity = catMaybes . fmap inputLine . T.lines


-- Encode an Intersection as an Int --------------------------------------------

isectId ∷ Word → Intersection → Int
isectId maxAvenue Intersection{..} = fromIntegral $ street*(maxAvenue+1) + avenue

fromIntersectionId ∷ Word → Int → Intersection
fromIntersectionId maxAvenue i = Intersection{..}
  where street = fromIntegral $ (i `div` (fromIntegral maxAvenue+1))
        avenue = fromIntegral $ (i `mod` (fromIntegral maxAvenue+1))

prop_packableIntersection ∷ Word → Int → Bool
prop_packableIntersection maxAve i =
  i == (fromIntegral $ isectId maxAve $ fromIntersectionId maxAve $ fromIntegral i)


-- Computing the Graph ---------------------------------------------------------

timeToTravel ∷ Distance → Duration
timeToTravel (Miles dist) = Seconds $ 60 * 60 * (dist / mph)
  where MPH mph = speedLimit

distance ∷ Location → Location → Distance
distance (x1,y1) (x2,y2) = Miles $ sqrt $ abs(x1-x2)^2 + abs(y1-y2)^2

directions ∷ Word → Intersection → [Direction]
directions maxAve Intersection{street,avenue} =
  streetDirections street ++ avenueDirections maxAve avenue

cityGraph ∷ CitySetup → Gr (Intersection,LightSetup) Duration
cityGraph setup = mkGraph nodes edges
  where maxAve = maximum $ (0:) $ flip map setup $ \(isect,_,_) → avenue isect
        toNode = isectId maxAve
        tbl    = Map.fromList [(i,l) | (i,l,_) ← setup]
        nodes  = [(toNode i, (i,light)) | (i,_,light) ← setup]

        edges ∷ [(Int, Int, Duration)]
        edges = do (start,_,_)     ← setup
                   (dest,duration) ← edgesFrom start
                   return (toNode start, toNode dest, duration)

        edgesFrom ∷ Intersection → [(Intersection,Duration)]
        edgesFrom x = do y         ← offset x <$> directions maxAve x
                         Just locX ← [Map.lookup x tbl]
                         Just locY ← [Map.lookup y tbl]
                         let dist  = distance locX locY
                         return (y, timeToTravel(distance locX locY))


-- Entry Point -----------------------------------------------------------------

-- Caululate the fastet path from the light [0,0] to the light with
-- the largest street and avenue.
--
-- Assumptions:
--
-- - We travel each segment of road at exactly 30 MPH.
-- - We never wait for light [0,0].

-- If the light is green for G seconds in your direction and R seconds in the
-- other direction, then your average waiting time is
-- [0 * G/(R+G) + (R)/2 * R/(R+G) = R^2/(2(R+G))] seconds.
data LightAssumptions = WorstCase | BestCase | OnAvg

data Action = ReadGraph
            | BestPath LightAssumptions

data ActionOutput = FastestTime
                  | PrintAllNodesAndPath
                  | PrintJGraph

findRoute ∷ LightAssumptions → CitySetup → [Intersection]
findRoute = undefined


-- Examples --------------------------------------------------------------------

-- spec ∷ [Text] → CitySetup
spec = cityGraph . inputCity . T.unlines


-- Since light [0,0]'s coordinates are (1.3,1) and light [0,1]'s coordinates are
-- (1,1), the distance between the two lights is 0.3 miles. Note that since 0 is a
-- multiple of five, Street zero and Avenue zero are two-way. Moreover, since
-- Avenue one is the highest avenue, it is two-way.

ex1 = spec [ "0 0  1.3 1  9 12"
           , "0 1  1.0 1  3 4"
           , "0 1  1.0 1  3 4"
           ]


-- Now, Streets 1 and 2 are one-way, as is Avenue 1. The distances between
-- intersections are straightforward, with the exception of [2,0] and [2,1]. That
-- segment has a distance of 0.5, since sqrt(0.3^2+0.4^2) equals five.
--
-- Note also that the intersections are specified in a more random order here.

ex2 = spec [ "1 0  1.3 1.4 9  12"
           , "1 1  1   1.4 11  7"
           , "2 0  1.3 1.8 9  12"
           , "1 2  0.7 1.4 3   3"
           , "0 1  1   1   3   3"
           , "2 1  1   2.2 15  7"
           , "2 2  0.7 2.2 6  15"
           , "0 2  0.7 1   3   3"
           , "0 0  1.3 1   9  12"
           ]

ex3 = spec [ "0 0  0.5 0.1    9  12"
           , "0 1  0.4 0.1    1 110"
           , "0 2  0.3 0.1  110   1"
           , "0 3  0.2 0.1  110   1"
           , "0 4  0.1 0.1  110   1"
           , "1 0  0.5 0.2    1 110"
           , "1 1  0.4 0.2  110   1"
           , "1 2  0.3 0.2  110   1"
           , "1 3  0.2 0.2    1 110"
           , "1 4  0.1 0.2    1 110"
           , "2 0  0.5 0.3    1 110"
           , "2 1  0.4 0.3  110   1"
           , "2 2  0.3 0.3  110   1"
           , "2 3  0.2 0.3  110   1"
           , "2 4  0.1 0.3    1 110"
           ]
