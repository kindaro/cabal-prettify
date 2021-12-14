module Prelude.Fancy where

import Prelude.Unicode

bind ∷ Monad monad ⇒ (input → monad output) → monad input → monad output
bind = (=<<)

type left × right = (left, right)
infixr 7 ×

(▵) ∷ (input → left) → (input → right) → input → (left, right)
function ▵ gunction = \ input → (function input, gunction input)
infixr 7 ▵

type left + right = Either left right
infixr 6 +

(▿) ∷ (left → output) → (right → output) → left + right → output
function ▿ gunction = either function gunction
infixr 6 ▿

class Commutative bifunctor where
  commute ∷ bifunctor α (bifunctor β γ) → bifunctor β (bifunctor α γ)

instance Commutative (, ) where
  commute ∷ α × β × γ → β × α × γ
  commute (α, (β, γ)) = (β, (α, γ))

instance Commutative Either where
  commute ∷ α + β + γ → β + α + γ
  commute (Left value) = (Right ∘ Left) value
  commute (Right (Left value)) = Left value
  commute (Right (Right value)) = Right (Right value)

distribute ∷ Bool × α → α + α
distribute (False, value) = Left value
distribute (True, value) = Right value
