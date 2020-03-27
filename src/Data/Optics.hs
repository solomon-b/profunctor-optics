{-# LANGUAGE ScopedTypeVariables #-}
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

-- Lens into an integer and extract the sign as a bool
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
    _build = Just

whole :: Prism Integer Integer Double Double
whole = Prism _match _build
  where
    _match x
      | f == 0 = Right n
      | otherwise = Left x
      where (n, f) = properFraction x
    _build = fromIntegral

-- Concrete Lenses dont compose
-- TYPE ERROR: view (pi1 . pi1) ((1, 2), 3)

-- A lens into a nested tuple
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
-- When the component being viewed through a lens is actually the whole of the
-- structure, then the lens is essentially a pair of functions of types S → A
-- and B → T

data Adapter a b s t = Adapter {from :: s -> a, to :: b -> t}

-- Adapters can be used as plumbing combinators.
-- Suppose we wanted an optic like pi11 which accesses the A parameter
-- but we had a different but isomorphic tuple (a, b, c)
-- For this we can simply pi11 with the following adapter:
flatten :: Adapter (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
flatten = Adapter _from _to
  where
    _from ((x, y), z) = (x, y, z)
    _to (x, y, z) = ((x, y), z)

pi11' :: Lens a a' (a, b, c) (a', b, c)
pi11' = Lens _view _update
  where
    Lens v u = pi11
    Adapter _from _to = flatten
    _view :: (a, b, c) -> a
    _view = v . _to
    _update :: (a', (a, b, c)) -> (a', b, c)
    _update (a', abc) =
      let abc' = _to abc
      in _from $ u (a', abc')


-- Traverals:
-- A traversable datatype is a container datatype (such as lists, or trees), in
-- which the data structures a finite number of elements, and an ordering on the
-- positions of those elements. Given such a traversable data structure, one can
-- traverse it, visiting each of the elements in turn, in the given order.


inc :: Bool -> State Integer Bool
inc b = state (\n -> (b, n + 1))

data Tree a = Empty | Node (Tree a) a (Tree a)

inorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
inorder _ Empty = pure Empty
inorder m (Node t x u) = ((Node <$> inorder m t) <*> m x) <*> inorder m u

countOdd :: Integer -> State Integer Bool
countOdd n = if even n then pure False else inc True

countOddTree :: Tree Integer -> State Integer (Tree Bool)
countOddTree = inorder countOdd


-- Traversal can be seen as a generalisation of lenses and of prisms, providing
-- access not just to a single component within a whole structure but onto an
-- entire sequence of such components. Indeed, the type (A → F B) → (S → F T) of
-- witnesses to traversability of the container type S is almost equivalent to a
-- pair of functions contents :: S → An and fill :: S × Bn → T, for some n being
-- the number of elements in the container.

-- The idea is that contents yields the sequence of elements in the container,
-- in the order specified by the traversal, and fill takes an old container and
-- a new sequence of elements and updates the old container by replacing each of
-- the elements with a new one.

-- a factorization into two functions contents and fill is not quite right,
-- because the appropriate value of the exponent n depends on the particular
-- container in S, and must match for applications of contents and fill:
-- one can in general only refill a container with precisely the same number of
-- elements as it originally contained.
-- However, the dependence can be captured by tupling together the two functions
-- and using a common existentially quantified length: the traversable type S is
-- equivalent to ∃n . An × (Bn → T).

data FunList a b t = Done t | More a (FunList a b (b -> t))

-- The isomorphism between FunList A B T and T + (A × (FunList A B (B → T))) is
-- witnessed by the following two functions:
out :: FunList a b t -> Either t (a, FunList a b (b -> t))
out (Done t) = Left t
out (More x l) = Right (x, l)

inn :: Either t (a, FunList a b (b -> t)) -> FunList a b t
inn (Left t) = Done t
inn (Right (x, l)) = More x l

-- Now, a traversal function of type (A → F B) → (S → F T) for each applicative
-- functor F yields an isomorphism S ≃ FunList A B T. In order to construct the
-- transformation from S to FunList A B T using such a traversal function, we
-- require FunList A B to be an applicative functor:

instance Functor (FunList a b) where
  fmap f (Done t) = Done (f t)
  fmap f (More x l) = More x (fmap (f .) l)

instance Applicative (FunList a b) where
  pure = Done
  (<*>) (Done f) l' = fmap f l'
  (<*>) (More x l) l' = More x (fmap flip l <*> l')

-- We also require an operation of type A → FunList A B B on elements, which we
-- call single as it parcels up an element as a singleton FunList:
single :: a -> FunList a b b
single x = More x (Done id)

-- We can use single as the body of a traversal, instantiating the applicative
-- functor F to FunList A B. This traversal will construct a singleton FunList
-- for each element of a container, then concatenate the singletons into one
-- long FunList. In particular, this gives t single::S → FunList A B T as one
-- half of the isomorphism S ≃ FunList A B T. Conversely, we can retrieve the
-- traversable container from the FunList:

fuse :: FunList b b t -> t
fuse (Done t) = t
fuse (More x l) = fuse l x

newtype Traversal a b s t = Traversal { extract :: s -> FunList a b t }

-- As another example, inorder single::Tree a → FunList a b (Tree b) extracts
-- the in-order sequence of elements from a tree, and moreover provides a
-- mechanism to refill the tree with a new sequence of elements. This type
-- matches the payload of a concrete traversal; so we can define concrete
-- in-order traversal of a tree by:
inorderC :: Traversal a b (Tree a) (Tree b)
inorderC = Traversal (inorder single)

-------------------
--- Profunctors ---
-------------------
