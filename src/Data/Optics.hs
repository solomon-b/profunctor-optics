{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Optics where

import Data.Monoid
import Data.Profunctor
import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State


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

class (Strong p, Choice p) => Wander p where
  wander :: forall s t a b.
    (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t

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

instance Wander (->) where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> (a -> b) -> (s -> t)
  wander trav f = runIdentity $ (\x -> Identity (runIdentity . x)) $ trav (Identity . f)

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

instance Applicative f => Wander (Star f) where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t)
    -> Star f a b -> Star f s t
  wander trav (Star f) = Star $ trav f

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

instance Monoid r => Wander (Forget r) where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t)
    -> Forget r a b -> Forget r s t
  wander trav (Forget f) = (\g -> Forget (getConst . g)) $ trav $ Const . f

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

infixr 4 %~
(%~) = over

over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
over = id

infixr 4 .~
(.~) = set

set :: forall s t a b. Setter s t a b -> b -> s -> t
set setter b = over setter (const b)

infixr 4 +~
(+~) = addOver

addOver :: forall s t a. Num a => Setter s t a a -> a -> s -> t
addOver setter = over setter . (+)

infixr 4 -~
(-~) = subOver

subOver :: forall s t a. Num a => Setter s t a a -> a -> s -> t
subOver setter = over setter . (-)

infixr 4 *~
(*~) = mulOver

mulOver :: forall s t a. Num a => Setter s t a a -> a -> s -> t
mulOver setter = over setter . (*)

infixr 4 /~
(/~) = divOver

divOver :: forall s t a. Fractional a => Setter s t a a -> a -> s -> t
divOver setter = over setter . (/)

infix 4 .=
(.=) :: forall s a b m. MonadState s m => Setter s s a b -> b -> m ()
(.=) = assign

assign :: forall s a b m. MonadState s m => Setter s s a b -> b -> m ()
assign setter b = void . modify $ set setter b

infix 4 %=
(%=) :: forall s a b m. MonadState s m => Setter s s a b -> (a -> b) -> m ()
(%=) = modifying

modifying :: forall s a b m. MonadState s m => Setter s s a b -> (a -> b) -> m ()
modifying setter f = void . modify $ over setter f

--------------
--- Getter ---
--------------

type AGetter s t a b = Fold a s t a b

view :: forall s t a b. AGetter s t a b -> s ->  a
view l = runForget $ l (Forget id)

infixl 8 ^.
(^.) = viewOn

viewOn :: forall s t a b. s -> AGetter s t a b -> a
viewOn = flip view

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
preview f s = getFirst . ($ s) . runForget $ f (Forget $ \a -> First $ Just a)

infixl 8 ^?
(^?) = previewOn

previewOn :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
previewOn s fold = preview fold s

foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
foldMapOf fold f = runForget $ fold (Forget f)

foldOf :: forall s t a b. Fold a s t a b -> s -> a
foldOf fold = foldMapOf fold id

foldrOf :: forall s t a b r. Fold (Endo r) s t a b -> (a -> r -> r) -> r -> s -> r
foldrOf fold f r = flip appEndo r . foldMapOf fold (Endo . f)

folded :: forall g a b t r. Monoid r => Foldable g => Fold r (g a) b a t
folded  = Forget . foldMap . runForget

-----------------
--- Traversal ---
-----------------

type Traversal s t a b = forall p. Wander p => Optic p s t a b
--type Traversal s t a b =
--  forall p. (Strong p, Choice p, Monoidal p) => Optic p s t a b

traversed :: forall t a b. Traversable t => Traversal (t a) (t b) a b
traversed = wander traverse

traverseOf :: Traversal s t a b -> (forall f. Applicative f => (a -> f b) -> s -> f t)
traverseOf p = runStar . p . Star
