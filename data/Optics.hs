{-# LANGUAGE RankNTypes #-}
module Data.Optics where

import Control.Monad.State
import Control.Invertible.Monoidal
import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice

-----------------------
--- Concrete Optics ---
-----------------------

data Lens a b s t = Lens { view :: s -> a, update :: (b, s) -> t }

pi1 :: Lens a b (a, c) (b, c)
pi1 = Lens _view _update
  where
    _view (x, _) = x
    _update (x', (_, y)) = (x', y)

pi2 :: Lens a b (c, a) (c, b)
pi2 = Lens _view _update
  where
    _view (_, y) = y
    _update (y', (x, _)) = (x, y')

sign :: Lens Bool Bool Integer Integer
sign = Lens _view _update
  where
    _view x = x >= 0
    _update (b, x) = if b then abs x else negate (abs x)


-- Prisms
-- Given a compound data structure (Sum Type) of type S, one of whose variants
-- is A, one can access that variant via a function `match :: S -> S + A` that
-- 'downcasts' to an A if possible and yields the original S if not.
-- Conversely, one may update the data stricture vai a function `build :: A -> s`
-- that 'upcasts' a new A to the compound type S.
data Prism a b s t = Prism { match :: s -> Either t a, build :: b -> t}

the :: Prism a b (Maybe a) (Maybe b)
the = Prism _match _build
  where
    _match (Just x) = Right x
    _match Nothing  = Left Nothing
    _build x = Just x

whole :: Prism Integer Integer Double Double
whole = Prism _match _build
  where
    _match x
      | f == 0 = Right n
      | otherwise = Left x
      where (n, f) = properFraction x
    _build = fromIntegral

-- Concrete Lenses dont compose
pi11 :: Lens a b ((a, c), d) ((b, c), d)
pi11 = Lens _view _update
  where
    Lens v u = pi1
    _view = v . v
    _update (x', xyz) = u (xy', xyz)
      where
        xy = v xyz
        xy' = u (x', xy)

-- Adapters:

data Adapter a b s t = Adapter {from :: s -> a, to :: b -> t}

flatten :: Adapter (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
flatten = Adapter _from _to
  where
    _from ((x, y), z) = (x, y, z)
    _to (x, y, z) = ((x, y), z)


-- Traverals:

inc :: Bool -> State Integer Bool
inc b = state (\n -> (b, n + 1))

data Tree a = Empty | Node (Tree a) a (Tree a)

inorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
inorder _ Empty = pure Empty
inorder m (Node t x u) = ((pure Node <*> inorder m t) <*> m x) <*> inorder m u

countOdd :: Integer -> State Integer Bool
countOdd n = if even n then pure False else inc True

countOddTree :: Tree Integer -> State Integer (Tree Bool)
countOddTree = inorder countOdd
