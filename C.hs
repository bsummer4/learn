{-# LANGUAGE UnicodeSyntax #-}

module C(Stuff, empty, get, set) where
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

type Stuff k v = (v, Map k v)

empty ∷ v → Stuff k v
empty x = (x, Map.empty)

get ∷ Ord k ⇒ k → Stuff k v → v
get k (d,m) = fromMaybe d $ Map.lookup k m

set ∷ Ord k ⇒ k → v → Stuff k v → Stuff k v
set k v (d,m) = (d,Map.insert k v m)
