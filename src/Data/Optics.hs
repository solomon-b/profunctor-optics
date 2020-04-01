{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Data.Optics where

import Data.Monoid
import Data.Profunctor

class Profunctor p => Mux p where
  mux :: p a b -> p c d -> p (a, c) (b, d)

class Profunctor p => Demux p where
  demux :: p a b -> p c d -> p (Either a c) (Either b d)

class Profunctor p => Monoidal p where
  par :: p a b -> p c d -> p (a, c) (b, d)
  empty :: p () ()

instance Mux (->) where
  mux :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
  mux f g (a, c) = (f a, g c)

instance Demux (->) where
  demux :: (a -> b) -> (c -> d) -> Either a c -> Either b d
  demux f g = \case
    Left a  -> Left (f a)
    Right c -> Right (g c)

instance Monoidal (->) where
  par :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
  par = mux

  empty :: () -> ()
  empty = id

instance Applicative f => Mux (Star f) where
  mux :: Star f a b ->  Star f c d -> Star f (a, c) (b, d)
  mux (Star f) (Star g)= Star $ \(a, c) -> (,) <$> f a <*> g c

instance Functor f => Demux (Star f) where
  demux :: Star f a b -> Star f c d -> Star f (Either a c) (Either b d)
  demux (Star f) (Star g) = Star $ \case
    Left a -> Left <$> f a
    Right c -> Right <$> g c

instance Applicative f => Monoidal (Star f) where
  par :: Star f a b -> Star f c d -> Star f (a, c) (b, d)
  par = mux

  empty :: Star f () ()
  empty = Star $ \() -> pure ()

--------------
--- Optics ---
--------------

type Optic p s t a b = p a b -> p s t

---------------
--- Adapter ---
---------------

type Adapter s t a b = forall p. Profunctor p => Optic p s t a b

from :: Adapter s t a b -> s -> a
from adapt = undefined

to :: Adapter s t a b -> b -> t
to adapt = undefined

--------------
--- Setter ---
--------------

type Setter s t a b = Optic (->) s t a b

over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
over = undefined

set :: forall s t a b. Setter s t a b -> b -> s -> t
set = undefined

------------
--- Lens ---
------------

type Lens s t a b = forall p. Strong p => Optic p s t a b

lens :: forall s t a b. (s -> a) -> (b -> s -> t) -> Lens s t a b
lens v s = undefined

view :: forall s t a b. Lens s t a b -> s ->  a
view l = undefined

-------------
--- Prism ---
-------------

type Prism s t a b = forall p. Choice p => Optic p s t a b

prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism = undefined

------------
--- Fold ---
------------

type Fold r s t a b = Optic (Forget r) s t a b

preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
preview = undefined

foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
foldMapOf = undefined

-----------------
--- Traversal ---
-----------------

type Traversal s t a b =
  forall p. (Strong p, Choice p, Monoidal p) => Optic p s t a b

traverseOf :: Traversal s t a b -> (forall f. Applicative f => (a -> f b) -> s -> f t)
traverseOf p = runStar . p . Star
