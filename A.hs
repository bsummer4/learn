{-# LANGUAGE UnicodeSyntax #-}

module A(cons,car,cdr,setCar,setCdr) where

cons ∷ a → b → (a,b)
cons a b = (a,b)

car ∷ (a,b) → a
car (a,b) = a

cdr ∷ (a,b) → b
cdr (a,b) = b

setCar ∷ a → (a,b) → (a,b)
setCar a (_,b) = (a,b)

setCdr ∷ b → (a,b) → (a,b)
setCdr b (a,_) = (a,b)
