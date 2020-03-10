module Data.Optics where

import Control.Invertable.Monoidal
import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice

-----------------------
--- Concrete Optics ---
-----------------------

data Lens s t a b = { view :: s -> a, modify :: (s, b) -> t }
