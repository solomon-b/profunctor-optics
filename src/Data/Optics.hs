{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Data.Optics where

import Control.Monad.State
import Data.Coerce

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

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

-- Laws:
-- dimap id id = id
-- dimap (f' . f) (g . g') = dimap f g . dimap f' g'

instance Profunctor (->) where
  dimap :: (a' -> a) -> (b -> b') -> (a -> b) -> (a' -> b')
  dimap f g h = g . h . f

newtype UpStar f a b = UpStar { unUpStar :: a -> f b }

instance Functor f => Profunctor (UpStar f) where
  dimap :: Functor f => (a' -> a) -> (b -> b') -> UpStar f a b -> UpStar f a' b'
  dimap f g (UpStar h) = UpStar $ fmap g . h . f

-- we say that a profunctor is cartesian if, informally, it can pass around some
-- additional context in the form of a pair. This is represented by an
-- additional method first that lifts a transformer of type P A B to one of type
-- P (A×C) (B×C) for any type C, passing through an additional contextual value
-- of type C:
class Profunctor p => Cartesian p where
  first :: p a b -> p (a, c) (b, c)
  second :: p a b -> p (c, a) (c, b)

-- Laws:
-- dimap runit runit' h = first h
-- dimap assoc assoc' (first (first h)) = first h
-- and symmetrical laws for second
-- where
--   runit  :: (a, ()) -> a
--   runit' :: a -> (a, ())
--   assoc  :: (a, (b, c)) -> ((a, b), c)
--   assoc' :: ((a, b), c) -> (a, (b, c))

-- one might call such profunctors cartesianly strong, because first acts as a
-- categorical ‘strength’ with respect to cartesian product; we abbreviate this
-- more precise term to simply ‘cartesian’. The function arrow is obviously
-- cartesian

-- we say that a profunctor is cartesian if, informally, it can pass around some
-- additional context in the form of a pair.
instance Cartesian (->) where
  first :: (a -> b) -> (a, c) -> (b, c)
  first h = cross h id
  second :: (a -> b) -> (c, a) -> (c, b)
  second h = cross id h

-- Control.Arrow.***
cross :: (b -> c) -> (b' -> c') -> ((b, b') -> (c, c'))
cross f g (x, y) = (f x, g y)

instance Functor f => Cartesian (UpStar f) where
  first :: Functor f => UpStar f a b -> UpStar f (a, c) (b, c)
  first (UpStar unUpStar) = UpStar $ rstrength . cross unUpStar id
  second :: Functor f => UpStar f a b -> UpStar f (c, a) (c, b)
  second (UpStar unUpStar) = UpStar $ lstrength . cross id unUpStar

rstrength :: Functor f => (f a, b) -> f (a, b)
rstrength (fx, y) = fmap (, y) fx

lstrength :: Functor f => (a, f b) -> f (a, b)
lstrength (x, fy) = fmap (x ,) fy

-- profunctors that can be lifted to act on sum types: 
class Profunctor p => CoCartesian p where
  left :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)

-- Laws:
-- dimap rzero rzero' h = left h
-- dimap coassoc' coassoc (left (left h)) = left h
-- and symmetrically for right
-- where
-- rzero :: Either a Void -> a
-- rzero' :: a -> Either a Void
-- coassoc :: Either a (Either b c) -> Either (Either a b) c
-- coassoc' :: Either (Either a b) c -> Either a (Either b c)

instance CoCartesian (->) where
  left :: (a -> b) -> (Either a c -> Either b c)
  left h = plus h id
  right :: (a -> b) -> (Either c a -> Either c b)
  right h = plus id h


plus :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
plus f g (Left x) = Left (f x)
plus f g (Right y) = Right (g y)

instance Applicative f => CoCartesian (UpStar f) where
  left :: Functor f => UpStar f a b -> UpStar f (Either a c) (Either b c)
  left (UpStar unUpStar) = UpStar $ either (fmap Left . unUpStar) (pure . Right)
  right :: Functor f => UpStar f a b -> UpStar f (Either c a) (Either c b)
  right (UpStar unUpStar) = UpStar $ either (pure . Left) (fmap Right . unUpStar)

-- The third refinement is a class of profunctors that support a form of
-- parallel com- position (in the sense of ‘independent’ rather than
-- ‘concurrent’):
class Profunctor p => Monoidal p where
  par :: p a b -> p c d -> p (a, c) (b, d)
  empty :: p () ()

-- Laws:
-- dimap assoc assoc' (par (par h j) k) = par h (par j k)
-- dimap runit runit' h = par h empty
-- dimap lunit lunit' h = par empty h
-- where
-- lunit :: ((), a) -> a
-- lunit' :: a -> ((), a)

instance Monoidal (->) where
  par :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))
  par = cross
  empty :: () -> ()
  empty = id

instance Applicative f => Monoidal (UpStar f) where
  par :: Applicative f => UpStar f a b -> UpStar f c d -> UpStar f (a, c) (b, d)
  par h k = UpStar (pair (unUpStar h) (unUpStar k))
  empty :: Applicative f => UpStar f () ()
  empty = UpStar pure

pair :: Applicative f => (a -> f b) -> (c -> f d) -> (a, c) -> f (b, d)
pair h k (x, y) = (,) <$> h x <*> k y


--------------------------------------
--- Optics In Terms of Profunctors ---
--------------------------------------

-- we represent data accessors as mappings between transformers:
type Optic p a b s t = p a b -> p s t

-- Informally, when S is a composite type with some component of type A, and T
-- similarly a composite type in which that component has type B, and P is some
-- notion of transformer, then we can think of a data accessor of type Optic P A
-- B S T as lifting a component transformer of type P A B to a whole-structure
-- transformer of type P S T.

type AdapterP a b s t = forall p. Profunctor p => Optic p a b s t

adapterC2P :: Adapter a b s t -> AdapterP a b s t
adapterC2P (Adapter o i) = dimap o i

instance Profunctor (Adapter a b) where
  dimap :: (s' -> s) -> (t -> t') -> Adapter a b s t -> Adapter a b s' t'
  dimap f g (Adapter o i) = Adapter (o . f) (g . i)

-- S' -f-> S -o-> A
-- B  -i>  T -g-> T'

-- Now, we construct the trivial concrete adapter Adapter id id of type Adapter
-- A B A B, and use the profunctor adapter to lift that to the desired concrete
-- adapter:
adapterP2C :: AdapterP a b s t -> Adapter a b s t
adapterP2C l = l (Adapter id id)

type LensP a b s t = forall p. Cartesian p => Optic p a b s t

instance Profunctor (Lens a b) where
  dimap :: (s' -> s) -> (t -> t') -> Lens a b s t -> Lens a b s' t'
  dimap f g (Lens v u) = Lens (v . f) (g . u . cross id f)

instance Cartesian (Lens a b) where
  first :: Lens a b s t -> Lens a b (s, c) (t, c)
  first (Lens v u) = Lens (v . fst) (fork (u . cross id fst) (snd . snd))
  second :: Lens  a b s t -> Lens a b (c, s) (c, t)
  second (Lens v u) = Lens (v . snd) (fork (fst . snd) (u . cross id snd))

-- Note: This should be definable in terms of Cartesian and Category
-- Control.Arrow.&&&
fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g x = (f x, g x)

lensC2P :: Lens a b s t -> LensP a b s t
lensC2P (Lens v u) = dimap (fork v id) u . first

lensP2C :: LensP a b s t -> Lens a b s t
lensP2C l = l (Lens id fst)

type PrismP a b s t = forall p. CoCartesian p => Optic p a b s t

instance Profunctor (Prism a b) where
  dimap :: (s' -> s) -> (t -> t') -> Prism a b s t -> Prism a b s' t'
  dimap f g (Prism a b) = Prism (plus g id . (a . f)) (g . b)

instance CoCartesian (Prism a b) where
  left :: Prism a b s t -> Prism a b (Either s c) (Either t c)
  left (Prism a b) =
    Prism (either (plus Left id . a) (Left . Right)) (Left . b)
  right :: Prism a b s t -> Prism a b (Either c s) (Either c t)
  right (Prism a b) =
    Prism (either (Left . Left) (plus Right id . a)) (Right . b)

prismC2P :: Prism a b s t -> PrismP a b s t
prismC2P (Prism m b) = dimap m (either id b) . right

prismP2C :: PrismP a b s t -> Prism a b s t
prismP2C l = l $ Prism Right id

--data Traversal a b s t = Traversal { extract :: s  -> FunList a b t}

-- The key step in the profunctor representation of traversals is to identify a
-- function traverse that lifts a transformation k :: P A B from As to Bs to act
-- on each of the elements of a FunList in order:
traverse' :: (CoCartesian p, Monoidal p) => p a b -> p (FunList a c t) (FunList b c t)
traverse' k = dimap out inn . right . par k $ traverse' k

--par :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))

type TraversalP a b s t =
  forall p. (Cartesian p, CoCartesian p, Monoidal p) => Optic p a b s t

traversalC2P :: Traversal a b s t -> TraversalP a b s t
traversalC2P (Traversal h) = dimap h fuse . traverse'

type Traversal' a b s t = UpStar (FunList a b) s t

-- Sitting between Arrow (Strong Category) and Profunctor we have:
class Profunctor p => Mux p where
  -- this is ***/cross
  mux :: p a b -> p c d -> p (a, c) (b, d)

instance Mux (->) where
  mux :: (a -> a') -> (b -> b') -> ((a,b) -> (a', b'))
  mux f g (a, b) = (f a, g b)

instance Applicative p => Mux (UpStar p) where
  mux :: UpStar p a b -> UpStar p c d -> UpStar p (a, c) (b, d)
  mux (UpStar pab) (UpStar pcd) = UpStar $ \(a, c) -> (,) <$> pab a <*> pcd c

class Profunctor p => Demux p where
  demux :: p a b -> p c d -> p (Either a c) (Either b d)

instance Demux (->) where
  demux :: (a -> b) -> (c -> d) -> Either a c -> Either b d
  demux f g = either (Left . f) (Right . g)

instance Functor p => Demux (UpStar p) where
  demux :: UpStar p a b -> UpStar p c d -> UpStar p (Either a c) (Either b d)
  demux (UpStar f) (UpStar g) = UpStar $ either (fmap Left . f) (fmap Right . g)

instance Profunctor (Traversal a b) where
  dimap :: (s' -> s) -> (t -> t') -> Traversal a b s t -> Traversal a b s' t'
  dimap f g (Traversal h) = Traversal $ fmap g . h . f

instance Cartesian  (Traversal a b) where
  first :: Traversal a b s t -> Traversal a b  (s, c) (t, c)
  first (Traversal h) = Traversal $ \(s, c) -> (, c) <$> h s
  --first (Traversal h) = Traversal $ unUpStar $ mux (UpStar h) (UpStar pure)
  second :: Traversal a b s t -> Traversal a b (c, s) (c, t)
  second (Traversal h) = Traversal $ \(c, s) -> (c,) <$> h s

instance CoCartesian (Traversal a b) where
  left :: Traversal a b s t -> Traversal a b (Either s c) (Either t c)
  left (Traversal h) = Traversal $ either (fmap Left . h) (pure . Right)
  --left (Traversal h) = Traversal $ unUpStar $ demux (UpStar h) (UpStar pure)
  right :: Traversal a b s t -> Traversal a b (Either c s) (Either c t)
  right (Traversal h) = undefined

-- Asad says i probably wont need Monoidal with Mux
instance Monoidal (Traversal a b) where
  par :: Traversal a b s t -> Traversal a b s' t' -> Traversal a b (s, s') (t, t')
  par (Traversal f) (Traversal g) = Traversal $ unUpStar $ mux (UpStar f) (UpStar g)
  empty :: Traversal a b () ()
  empty = Traversal $ unUpStar $ UpStar pure

traversalP2C :: TraversalP a b s t -> Traversal a b s t
traversalP2C l = l $ Traversal $ \a -> More a $ Done id

-- View --

newtype Forget r a b = Forget { unForget :: a -> r }

instance Profunctor (Forget r) where
  dimap :: (a' -> a) -> (b -> b') -> Forget r a b -> Forget r a' b'
  dimap f g (Forget h) = Forget $ h . f

instance Cartesian (Forget r) where
  first :: Forget r a b -> Forget r (a, c) (b, c)
  first (Forget h) = Forget $ \(a, c) -> h a
  second :: Forget r a b -> Forget r (c, a) (c, b)
  second (Forget h) = Forget $ \(c, a) -> h a

traverseOf :: TraversalP a b s t -> (forall f. Applicative f => (a -> f b) -> s -> f t)
traverseOf p = unUpStar . p . UpStar

-- Composing Profunctor Optics --

  --first :: p a b -> p (a, c) (b, c)
  --second :: p a b -> p (c, a) (c, b)
pi1' :: LensP a b (a, c) (b, c)
pi1' = dimap fst undefined

viewP :: forall a b s t. (forall p. Cartesian p => p a b -> p s t) -> s -> a
viewP l = unForget $ l (Forget id)

--pi1' :: LensP a b (a, c) (b, c)
--pi1' = lensC2P pi1

