{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bound.Unwrap (Fresh,
                     name,
                     Counter,
                     UnwrapT,
                     Unwrap,
                     runUnwrapT,
                     runUnwrap,
                     unwrap) where
import Bound
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Gen

data Fresh a = Fresh { fresh :: !Int
                   , uname :: a }
            deriving (Eq, Ord)

name :: a -> Fresh a
name = Fresh 0

-- Keeping this opaque, but I don't want *another*
-- monad for counting dammit. I built one and that was enough.
newtype Counter = Counter {getCounter :: Int}

type UnwrapT = GenT Counter
type Unwrap = Gen Counter
type MonadUnwrap m = MonadGen Counter m

runUnwrapT :: Monad m => GenT Counter m a -> m a
runUnwrapT = runGenTWith (successor $ Counter . succ . getCounter) (Counter 0)

runUnwrap :: Gen Counter a -> a
runUnwrap = runIdentity . runUnwrapT

freshify :: (MonadUnwrap m, Functor m) => Fresh a -> m (Fresh a)
freshify nm = (\i -> nm{fresh = i}) <$> fmap getCounter gen

unwrap :: (Monad f, Functor m, MonadUnwrap m)
          => a -> Scope () f (Fresh a) -> m (Fresh a, f (Fresh a))
unwrap a s = (\n -> (n, instantiate1 (return n) s)) <$> freshify (name a)
