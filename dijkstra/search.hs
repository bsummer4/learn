{-# LANGUAGE DeriveAnyClass, DeriveGeneric, RecordWildCards, UnicodeSyntax #-}

import qualified Data.ByteString.Lazy as BSL
import           Data.Csv
import           Data.Either
import           Data.Hashable
import           Data.HashMap.Strict  (HashMap, (!))
import qualified Data.HashMap.Strict  as M
import           Data.List            as L
import           Data.PSQueue         (Binding ((:->)), PSQ, key, prio)
import qualified Data.PSQueue         as PSQ
import qualified Data.Vector          as V
import           GHC.Generics         (Generic)
import           Text.Printf

data Idx = Idx { x ∷ !Int
               , y ∷ !Int
               }
  deriving (Eq,Ord,Generic,Hashable)

instance Show Idx where
  show (Idx x y) = printf "%dx%d" x y

data St = St { weights  ∷ HashMap Idx Double
             , distance ∷ HashMap Idx Double
             , prev     ∷ HashMap Idx Idx
             , queue    ∷ PSQ Idx Double
             }

instance Show St where
  show (St{..}) = unlines [ printf "weights: %s"    (show weights)
                          , printf "\ndistance: %s" (show distance)
                          , printf "\nprev: %s"     (show prev)
                          , printf "\nqueue: %s"    (show queue)
                          ]

fromRight (Right x) = x

parseMap ∷ BSL.ByteString → V.Vector (V.Vector Int)
parseMap = fromRight . decode NoHeader

loadMap ∷ IO (V.Vector (V.Vector Int))
loadMap = parseMap <$> BSL.readFile "maze.txt"

vectorToMap ∷ V.Vector a → HashMap Int a
vectorToMap = V.ifoldr' M.insert M.empty

mergeKeys ∷ (Eq k1,Eq k2,Eq k3)
          ⇒ (Hashable k1, Hashable k2, Hashable k3)
          ⇒ (k1 → k2 → k3) → HashMap k1 (HashMap k2 v) → HashMap k3 v
mergeKeys mkKey = M.foldlWithKey' f M.empty
  where f acc k1 = M.foldlWithKey' g acc
          where g subAcc k2 v = M.insert (mkKey k1 k2) v subAcc

csvWeights ∷ V.Vector (V.Vector Int) → HashMap Idx Double
csvWeights = fmap fromIntegral . mergeKeys Idx . vectorToMap . fmap vectorToMap

shrink ∷ Int → V.Vector (V.Vector Int) → V.Vector (V.Vector Int)
shrink sz = fmap (V.take sz) . V.take sz

start  = Idx 0 0
target = Idx 200 200

initialState ∷ V.Vector (V.Vector Int) → St
initialState vecs = St {..}
  where weights  = csvWeights vecs
        distance = M.insert start 0 $ const (1/0) <$> weights
        prev     = M.empty
        queue    = PSQ.fromList $ (\v → v :-> (distance!v)) <$> M.keys weights

main = do
  initial ← initialState <$> loadMap
  let (Just(route,cost)) = shortestPath $ fullDijkstra initial
  putStr $ printf "cost: %f\nroute: %s" cost (show route)

offsets ∷ (Idx → Bool) → Idx → [Idx]
offsets valid Idx{..} = filter valid [ Idx (x+1) y
                                     , Idx (x-1) y
                                     , Idx  x    (y+1)
                                     , Idx  x    (y-1)
                                     ]

step u st@St{..} v =
  let alt = (distance!u) + weights!v in
    if alt >= (distance!v) then st else
      St { weights  = weights
         , distance = M.insert    v          alt distance
         , prev     = M.insert    v          u   prev
         , queue    = PSQ.adjust (const alt) v   queue
         }

dijkstra ∷ St → St
dijkstra St{..} | PSQ.null queue = St{..}
dijkstra St{..}                  = result
    where Just((u:->_), queue') = PSQ.minView queue
          neighbors = offsets (flip M.member weights) u
          result = L.foldl' (step u) (St{..}{queue=queue'}) neighbors

fullDijkstra st =
  if PSQ.null (queue st) then st else
    fullDijkstra (dijkstra st)

shortestPath ∷ St → Maybe ([Idx],Double)
shortestPath St{..} = go target ([target],weights!target)
  where go cur (rpath,paid) =
            do next ← M.lookup cur prev
               if next == start
                 then return(reverse rpath,paid)
                 else go next (next:rpath, paid+(weights!next))
