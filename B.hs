{-# LANGUAGE UnicodeSyntax #-}

module B(cons,car,cdr,setCar,setCdr) where
import qualified Control.Lens as L
import qualified Control.Lens.Tuple as L

cons ∷ a → b → (a,b)
cons a b = (a,b)

car ∷ (a,b) → a
car x = L.view L._1 x

cdr ∷ (a,b) → b
cdr x = L.view L._2 x

setCar ∷ a → (a,b) → (a,b)
setCar a c = L.set L._1 a c

setCdr ∷ b → (a,b) → (a,b)
setCdr b c = L.set L._2 b c
