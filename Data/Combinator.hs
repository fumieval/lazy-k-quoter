module Data.Combinator where

import Data.ByteString

infixl 9 :$

data Combinator = S | K | I | B | C | W | Foreign (Combinator -> Combinator) | Expr :$ Expr

class Combinatory a where
    fromCombinator :: Combinator -> a
    toCombinator :: a -> Combinator

instance Combinatory Int where

instance Combinatory ByteString where

instance (Combinatory a, Combinatory b) => Combinatory (a -> b) where
    fromCombinator e = \x -> e :$ fromCombinator x
    toCombinator f = Foreign (toCombinator . f . fromCombinator)
