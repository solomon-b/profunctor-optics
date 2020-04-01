{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Data.Optics where

import Data.Monoid
import Data.Profunctor

-------------------
--- Typeclasses ---
-------------------

class Profunctor p => Mux p where
  mux :: p a b -> p c d -> p (a, c) (b, d)

class Profunctor p => Demux p where
  demux :: p a b -> p c d -> p (Either a c) (Either b d)

class Profunctor p => Monoidal p where
  par :: p a b -> p c d -> p (a, c) (b, d)
  empty :: p () ()

-------------------
--- Profunctors ---
-------------------

-- data (->) (a :: *) (b :: *) = a -> b

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

-- newtype Star f d c = Star { runStar :: d -> f c }

instance Applicative f => Mux (Star f) where
  mux :: Star f a b -> Star f c d -> Star f (a, c) (b, d)
  mux (Star f) (Star g) = Star $ \(a, c) -> (,) <$> f a <*> g c

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

newtype Tagged a b = Tagged { unTagged :: b }

instance Profunctor Tagged where
  dimap :: (a' -> a) -> (b -> b') -> Tagged a b -> Tagged a' b'
  dimap f g (Tagged b) = Tagged (g b)

instance Choice Tagged where
  left' :: Tagged a b -> Tagged (Either a c) (Either b c)
  left' (Tagged b) = Tagged $ Left b
  right' :: Tagged a b -> Tagged (Either c a) (Either c b)
  right' (Tagged b) = Tagged $ Right b

instance Mux Tagged where
  mux :: Tagged a b -> Tagged c d -> Tagged (a, c) (b, d)
  mux (Tagged b) (Tagged d) = Tagged (b, d)

instance Monoidal Tagged where
  par :: Tagged a b -> Tagged c d -> Tagged (a, c) (b, d)
  par = mux

  empty :: Tagged () ()
  empty = Tagged ()

-- newtype Forget r a b = Forget { runForget :: a -> r }

--------------
--- Optics ---
--------------

type Optic p s t a b = p a b -> p s t

---------------
--- Adapter ---
---------------

type Adapter s t a b = forall p. Profunctor p => Optic p s t a b

adapter :: forall s t a b. (s -> a) -> (b -> t) -> Adapter s t a b
adapter = dimap

from :: Adapter s t a b -> s -> a
from adapt = runForget . adapt $ Forget id

to :: Adapter s t a b -> b -> t
to adapt = unTagged . adapt . Tagged

flatten :: Adapter ((a, b), c) ((a', b'), c') (a, b, c) (a', b', c')
flatten = adapter from_ to_
  where
    from_ ((x, y), z) = (x, y, z)
    to_ (x, y, z) = ((x, y), z)

--------------
--- Setter ---
--------------

type Setter s t a b = Optic (->) s t a b

over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
over = id

set :: forall s t a b. Setter s t a b -> b -> s -> t
set setter b = over setter (const b)

--------------
--- Getter ---
--------------

type AGetter s t a b = Fold a s t a b

view :: forall s t a b. AGetter s t a b -> s ->  a
view l = runForget $ l (Forget id)

------------
--- Lens ---
------------

type Lens s t a b = forall p. Strong p => Optic p s t a b

lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' $ \s -> (get s, set s)

lens' :: forall s t a b. (s -> (a, b -> t)) -> Lens s t a b
lens' to l = dimap to (\(b, f) -> f b) (first' l)

_1 :: forall a b c. Lens (a, c) (b, c) a b
_1 = lens fst (\(_, c) b -> (b, c))

_2 :: forall a b c. Lens (c, a) (c, b) a b
_2 = lens snd (\(c, _) b -> (c, b))

-------------
--- Prism ---
-------------

type Prism s t a b = forall p. Choice p => Optic p s t a b
prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism to from pab = dimap from (either id id) $ right' $ rmap to pab

_Nothing :: forall a b. forall p. Choice p => p () () -> p (Maybe a) (Maybe b)
_Nothing = prism (const Nothing) (maybe (Right ()) (const (Left Nothing)))

_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = left'

_Right :: forall a b c. Prism (Either c a) (Either c b) a b
_Right = right'

_Cons :: forall a b. Prism [a] [b] (a, [a]) (b, [b])
_Cons = prism (uncurry (:)) $ \case
  x:xs -> Right (x, xs)
  [] -> Left []

_Nil :: forall a b. Prism [a] [b] () b
_Nil = prism pure $ \case
  x:xs -> Left []
  [] -> Right ()

------------
--- Fold ---
------------

type Fold r s t a b = Optic (Forget r) s t a b

preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
preview f s = getFirst . (\g -> g s) . runForget $ f (Forget $ \a -> First $ Just a)

foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
foldMapOf = undefined

-----------------
--- Traversal ---
-----------------

type Traversal s t a b =
  forall p. (Strong p, Choice p, Monoidal p) => Optic p s t a b

traverseOf :: Traversal s t a b -> (forall f. Applicative f => (a -> f b) -> s -> f t)
traverseOf p = runStar . p . Star
