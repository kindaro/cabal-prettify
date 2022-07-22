module Prelude.Fancy where

import Prelude.Unicode
import Control.Applicative
import Control.Monad.Writer
import qualified Data.ByteString

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

constant ∷ α → β → α
constant = const

whence ∷ Alternative alternative ⇒ Bool → α → alternative α
whence True = pure
whence False = constant empty

for ∷ Functor functor ⇒ functor α → (α → β) → functor β
for = flip fmap

say ∷ (Applicative applicative, MonadWriter (applicative α) monad) ⇒ α → monad ( )
say = tell ∘ pure

type ByteArray = Data.ByteString.ByteString
