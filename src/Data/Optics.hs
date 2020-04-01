{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Data.Optics where

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

type Optic p a b s t = p a b -> p s t

---------------
--- Adapter ---
---------------

type Adapter a b s t = forall p. Profunctor p => Optic p a b s t

------------
--- Lens ---
------------

type Lens a b s t = forall p. Strong p => Optic p a b s t

-------------
--- Prism ---
-------------

type Prism a b s t = forall p. Choice p => Optic p a b s t

-----------------
--- Traversal ---
-----------------

type Traversal a b s t =
  forall p. (Strong p, Choice p, Monoidal p) => Optic p a b s t
