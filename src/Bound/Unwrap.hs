{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bound.Unwrap (Name,
                     name,
                     Counter,
                     UnwrapT,
                     Unwrap,
                     runUnwrapT,
                     runUnwrap,
                     unwrap) where
import Bound
import Control.Applicative
import Control.Monad.Gen

data Name a = Name { fresh :: !Int
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

runUnwrapT :: Monad m => GenT Counter m a -> m a
runUnwrapT = runGenTWith (successor $ Counter . succ . getCounter) (Counter 0)

runUnwrap :: Gen Counter a -> a
runUnwrap = runGenWith (successor $ Counter . succ . getCounter) (Counter 0)

freshify :: (MonadUnwrap m, Functor m) => Name a -> m (Name a)
freshify nm = (\i -> nm{fresh = i}) <$> fmap getCounter gen

unwrap :: (Monad f, Functor m, MonadUnwrap m)
          => a -> Scope () f (Name a) -> m (Name a, f (Name a))
unwrap a s = (\n -> (n, instantiate1 (return n) s)) <$> freshify (name a)
