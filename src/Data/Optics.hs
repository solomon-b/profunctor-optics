{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Optics where

import Data.Monoid
import Data.Profunctor
import Data.Bifoldable
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

class Mux p => Muxative p where
  terminal :: p () ()

infixr 3 &&&
(&&&) :: Mux p => p a b -> p a c -> p a (b, c)
(&&&) f g = dimap (\a -> (a, a)) id $ mux f g

infixr 3 ***
(***) :: Mux p => p a a' -> p b b' -> p (a, b) (a', b')
(***) = mux

passthru :: (Strong p, Muxative p) => p a a
passthru = dimap ((),) snd $ first' terminal

class (Strong p, Choice p, Muxative p) => Wander p where
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

instance Muxative (->) where
  terminal :: () -> ()
  terminal = id

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

instance Applicative f => Muxative (Star f) where
  terminal :: Star f () ()
  terminal = Star $ \() -> pure ()

instance Applicative f => Wander (Star f) where
  wander :: (forall g. Applicative g => (a -> g b) -> s -> g t)
    -> Star f a b -> Star f s t
  wander trav (Star f) = Star $ trav f

newtype Tagged a b = Tagged { unTagged :: b }

instance Profunctor Tagged where
  dimap :: (a' -> a) -> (b -> b') -> Tagged a b -> Tagged a' b'
  dimap _ g (Tagged b) = Tagged (g b)

instance Choice Tagged where
  left' :: Tagged a b -> Tagged (Either a c) (Either b c)
  left' (Tagged b) = Tagged $ Left b
  right' :: Tagged a b -> Tagged (Either c a) (Either c b)
  right' (Tagged b) = Tagged $ Right b

instance Mux Tagged where
  mux :: Tagged a b -> Tagged c d -> Tagged (a, c) (b, d)
  mux (Tagged b) (Tagged d) = Tagged (b, d)

instance Muxative Tagged where
  terminal :: Tagged () ()
  terminal = Tagged ()

-- newtype Forget r a b = Forget { runForget :: a -> r }

instance Monoid r => Mux (Forget r) where
  mux :: Forget r a b -> Forget r c d -> Forget r (a, c) (b, d)
  mux f g = Forget $ bifoldMap (runForget f) (runForget g)

instance Monoid r => Muxative (Forget r) where
  terminal :: Forget r () ()
  terminal = Forget $ const mempty

instance Monoid r => Wander (Forget r) where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t)
    -> Forget r a b -> Forget r s t
  wander trav (Forget f) = (\g -> Forget (getConst . g)) $ trav $ Const . f
  --wander trav (Forget f) = (\g -> Forget (getConst . g)) $ trav $ Const . f

--------------------
--- Optics Types ---
--------------------

type Optic p s t a b = p a b -> p s t
type Adapter s t a b = forall p. Profunctor p => Optic p s t a b
type Setter s t a b = Optic (->) s t a b
type AGetter s t a b = Fold a s t a b
type Getter s t a b = forall r. Fold r s t a b
type Lens s t a b = forall p. Strong p => Optic p s t a b
type Prism s t a b = forall p. Choice p => Optic p s t a b
type Fold r s t a b = Optic (Forget r) s t a b
type Traversal s t a b = forall p. Wander p => Optic p s t a b
--type Traversal s t a b =
--  forall p. (Strong p, Choice p, Muxative p) => Optic p s t a b

----------------
--- Adapters ---
----------------

adapter :: forall s t a b. (s -> a) -> (b -> t) -> Adapter s t a b
adapter = dimap

--withAdapter :: forall s t a b r. AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
--withAdapter 

from :: forall s t a b. Adapter s t a b -> s -> a
from adapt = runForget . adapt $ Forget id

toAdapter :: forall s t a b. Adapter s t a b -> b -> t
toAdapter adapt = unTagged . adapt . Tagged

flatten :: forall a a' b b' c c'.
  Adapter ((a, b), c) ((a', b'), c') (a, b, c) (a', b', c')
flatten = adapter from_ to_
  where
    from_ ((x, y), z) = (x, y, z)
    to_ (x, y, z) = ((x, y), z)

---------------
--- Setters ---
---------------

infixr 4 %~
(%~) :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
(%~) = over

over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
over = id

infixr 4 .~
(.~) :: forall s t a b. Setter s t a b -> b -> s -> t
(.~) = set

set :: forall s t a b. Setter s t a b -> b -> s -> t
set setter b = over setter (const b)

infixr 4 +~
(+~) :: forall s t a. Num a => Setter s t a a -> a -> s -> t
(+~) = addOver

addOver :: forall s t a. Num a => Setter s t a a -> a -> s -> t
addOver setter = over setter . (+)

infixr 4 -~
(-~) :: forall s t a. Num a => Setter s t a a -> a -> s -> t
(-~) = subOver

subOver :: forall s t a. Num a => Setter s t a a -> a -> s -> t
subOver setter = over setter . (-)

infixr 4 *~
(*~) :: forall s t a. Num a => Setter s t a a -> a -> s -> t
(*~) = mulOver

mulOver :: forall s t a. Num a => Setter s t a a -> a -> s -> t
mulOver setter = over setter . (*)

infixr 4 /~
(/~) :: forall s t a. Fractional a => Setter s t a a -> a -> s -> t
(/~) = divOver

divOver :: forall s t a. Fractional a => Setter s t a a -> a -> s -> t
divOver setter = over setter . (/)

infixr 4 ||~
(||~) :: forall s t. Setter s t Bool Bool -> Bool -> s -> t
(||~) = disjOver

disjOver :: forall s t. Setter s t Bool Bool -> Bool -> s -> t
disjOver setter = over setter . (||)

infixr 4 &&~
(&&~) :: forall s t. Setter s t Bool Bool -> Bool -> s -> t
(&&~) = disjOver

conjOver :: forall s t. Setter s t Bool Bool -> Bool -> s -> t
conjOver setter = over setter . (&&)

infixr 4  <>~
(<>~) :: forall s t a. Semigroup a => Setter s t a a -> a -> s -> t
(<>~) = appendOver

appendOver :: forall s t a. Semigroup a => Setter s t a a -> a -> s -> t
appendOver setter = over setter . (<>)

infixr 4 ?~
(?~) :: forall s t a b. Setter s t a (Maybe b) -> b -> s -> t
(?~) = setJust

setJust :: forall s t a b. Setter s t a (Maybe b) -> b -> s -> t
setJust setter = set setter . Just

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

infix 4 +=
(+=) :: forall s a m. MonadState s m => Num a => Setter s s a a -> a -> m ()
(+=) = addModifying

addModifying :: forall s a m. MonadState s m => Num a => Setter s s a a -> a -> m ()
addModifying setter = modifying setter . (+)

infix 4 -=
(-=) :: forall s a m. MonadState s m => Num a => Setter s s a a -> a -> m ()
(-=) = subModifying

subModifying :: forall s a m. MonadState s m => Num a => Setter s s a a -> a -> m ()
subModifying setter = modifying setter . (-)

infix 4 *=
(*=) :: forall s a m. MonadState s m => Num a => Setter s s a a -> a -> m ()
(*=) = mulModifying

mulModifying :: forall s a m. MonadState s m => Num a => Setter s s a a -> a -> m ()
mulModifying setter = modifying setter . (*)

infix 4 /=
(/=) :: forall s a m. MonadState s m => Fractional a => Setter s s a a -> a -> m ()
(/=) = divModifying

divModifying :: forall s a m. MonadState s m => Fractional a => Setter s s a a -> a -> m ()
divModifying setter = modifying setter . (/)

infix 4 ||=
(||=) :: forall s m. MonadState s m => Setter s s Bool Bool -> Bool -> m ()
(||=) = disjModifying

disjModifying :: forall s m. MonadState s m => Setter s s Bool Bool -> Bool -> m ()
disjModifying setter = modifying setter . (||)

infix 4 &&=
(&&=) :: forall s m. MonadState s m => Setter s s Bool Bool -> Bool -> m ()
(&&=) = disjModifying

conjModifying :: forall s m. MonadState s m => Setter s s Bool Bool -> Bool -> m ()
conjModifying setter = modifying setter . (&&)

infix 4 <>=
(<>=) :: forall s a m. MonadState s m => Semigroup a => Setter s s a a -> a -> m ()
(<>=) = appendModifying

appendModifying :: forall s a m. MonadState s m => Semigroup a => Setter s s a a -> a -> m ()
appendModifying setter = modifying setter . (<>)

infix 4 ?=
(?=) :: forall s a b m. MonadState s m => Setter s s a (Maybe b) -> b -> m ()
(?=) = assignJust

assignJust :: forall s a b m. MonadState s m => Setter s s a (Maybe b) -> b -> m ()
assignJust setter = assign setter . Just

---------------
--- Getters ---
---------------

view :: forall s t a b. AGetter s t a b -> s ->  a
view l = runForget $ l (Forget id)

infixl 8 ^.
(^.) :: forall s t a b. s -> AGetter s t a b -> a
(^.) = viewOn

viewOn :: forall s t a b. s -> AGetter s t a b -> a
viewOn = flip view

to :: forall s t a b. (s -> a) -> Getter s t a b
to f p = Forget (runForget p . f)

takeBoth :: forall s t a b c d. AGetter s t a b -> AGetter s t c d -> Getter s t (a, c) (b, d)
takeBoth p q = to $ view p &&& view q

use :: forall s t a b m. MonadState s m => Getter s t a b -> m a
use getter = gets $ view getter

--------------
--- Lenses ---
--------------

lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get' set' = lens' $ \s -> (get' s, set' s)

lens' :: forall s t a b. (s -> (a, b -> t)) -> Lens s t a b
lens' to' l = dimap to' (\(b, f) -> f b) (first' l)

_1 :: forall a b c. Lens (a, c) (b, c) a b
_1 = lens fst (\(_, c) b -> (b, c))

_2 :: forall a b c. Lens (c, a) (c, b) a b
_2 = lens snd (\(c, _) b -> (c, b))

--------------
--- Prisms ---
--------------

prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism to' from' pab = dimap from' (either id id) $ right' $ rmap to' pab

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
  _:_ -> Left []
  [] -> Right ()

-------------
--- Folds ---
-------------

preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
preview f s = getFirst . ($ s) . runForget $ f (Forget $ \a -> First $ Just a)

infixl 8 ^?
(^?) :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
(^?) = previewOn

previewOn :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
previewOn s fold = preview fold s

foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
foldMapOf fold f = runForget $ fold (Forget f)

foldOf :: forall s t a b. Fold a s t a b -> s -> a
foldOf fold = foldMapOf fold id

foldrOf :: forall s t a b r. Fold (Endo r) s t a b -> (a -> r -> r) -> r -> s -> r
foldrOf fold f r = flip appEndo r . foldMapOf fold (Endo . f)

foldlOf :: forall s t a b r.
  Fold (Dual (Endo r)) s t a b -> (r -> a -> r) -> r -> s -> r
foldlOf fold f r = flip appEndo r . getDual . foldMapOf fold (Dual . Endo . flip f)

folded :: forall g a b t r. Monoid r => Foldable g => Fold r (g a) b a t
folded  = Forget . foldMap . runForget

toListOf :: forall s t a b. Fold (Endo [a]) s t a b -> s -> [a]
toListOf fold = foldrOf fold (:) []

infixl 8 ^..
(^..) :: forall s t a b. s -> Fold (Endo [a]) s t a b -> [a]
(^..) = toListOfOn

toListOfOn :: forall s t a b. s -> Fold (Endo [a]) s t a b -> [a]
toListOfOn = flip toListOf

filtered :: forall p a. Choice p => (a -> Bool) -> Optic p a a a a
filtered pred' = dimap (\a -> if pred' a then Right a else Left a) (either id id) . right'

------------------
--- Traversals ---
------------------

traversed :: forall t a b. Traversable t => Traversal (t a) (t b) a b
traversed = wander traverse

traverseOf :: forall f s t a b. Optic (Star f) s t a b -> (a -> f b) -> s -> f t
traverseOf p = runStar . p . Star

sequenceOf :: forall f s t a. Applicative f => Optic (Star f) s t (f a) a -> s -> f t
sequenceOf p = traverseOf p id
