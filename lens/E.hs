{-# LANGUAGE UnicodeSyntax, RankNTypes #-}

module E(Stuff, empty, lens) where
import Prelude.Unicode
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Control.Lens as L
import Control.Lens ((^.), (.~), (&))

data Stuff k v = Stuff v (Map k v)
	deriving Show

empty ∷ v → Stuff k v
empty x = Stuff x Map.empty

get ∷ Ord k ⇒ k → Stuff k v → v
get k (Stuff d m) = fromMaybe d $ L.view (L.at k) m

set ∷ Ord k ⇒ k → v → Stuff k v → Stuff k v
set k v (Stuff d m) = Stuff d $ L.set (L.at k) (Just v) m

lens ∷ Ord k ⇒ k → L.Lens' (Stuff k v) v
lens k = L.lens (get k) (\c v → set k v c)

stuff ∷ Stuff Int (Stuff Int Int)
stuff = empty (empty 0)

ex ∷ [Int]
ex =
	[ L.view (lens 5) $ L.view (lens 4) $ L.set (lens 4∘lens 5) 13 stuff
	, (stuff&(lens 4∘lens 5).~13) ^. lens 4 ^. lens 5
	]
