{-# LANGUAGE UnicodeSyntax, RankNTypes #-}

module D(Stuff, empty, get, set, lens) where
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Control.Lens as L
import qualified Control.Lens.At as L

data Stuff k v = Stuff v (Map k v)

empty ∷ v → Stuff k v
empty x = Stuff x Map.empty

get ∷ Ord k ⇒ k → Stuff k v → v
get k (Stuff d m) = fromMaybe d $ L.view (L.at k) m

set ∷ Ord k ⇒ k → v → Stuff k v → Stuff k v
set k v (Stuff d m) = Stuff d $ L.set (L.at k) (Just v) m

lens ∷ Ord k ⇒ k → L.Lens' (Stuff k v) v
lens k = L.lens (get k) (\c v → set k v c)
