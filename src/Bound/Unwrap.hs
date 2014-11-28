{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bound.Unwrap (Name, name, Counter, UnwrapT, Unwrap) where
import Bound
import Control.Applicative
import Control.Monad
import Control.Monad.Gen

data Name a = Name { fresh :: Int
                   , uname :: a }
            deriving (Eq, Ord)

name :: a -> Name a
name = Name 0

-- Keeping this opaque, but I don't want *another*
-- monad for counting dammit. I built one and that was enough.
newtype Counter = Counter {getCounter :: Int}

type UnwrapT = GenT Counter
type Unwrap = Gen Counter
type MonadUnwrap m = MonadGen Counter m
