{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bound.Unwrap ( Fresh
                    , name
                    , freshify
                    , Counter
                    , UnwrapT
                    , Unwrap
                    , runUnwrapT
                    , runUnwrap
                    , unwrap1
                    , unwrapAll1) where
import Bound
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Gen

data Fresh a = Fresh { fresh :: !Int
                     , uname :: a }
             deriving (Eq, Ord)

instance Show a => Show (Fresh a) where
  show (Fresh i a) = show i ++ '.' : show a

-- | Create a name. This name isn't unique at all at this point. Once
-- you have a name you can pass it to freshify to render it unique
-- within the current monadic context.
name :: a -> Fresh a
name = Fresh 0

-- | @erase@ drops the information in a 'Fresh' that makes it globally
-- unique and gives you back the user supplied name. For obvious
-- reasons, @erase@ isn't injective. It is the case that
--
-- @
--    erase . name  = id
--    name . erase /= id
-- @
erase :: Fresh a -> a
erase = uname

-- Keeping this opaque, but I don't want *another*
-- monad for counting dammit. I built one and that was enough.
newtype Counter = Counter {getCounter :: Int}

-- | A specialized version of 'GenT' used for unwrapping things.
type UnwrapT = GenT Counter
type Unwrap = Gen Counter

-- | A specialized constraint for monads who know how to unwrap
-- things.
type MonadUnwrap m = MonadGen Counter m

runUnwrapT :: Monad m => UnwrapT m a -> m a
runUnwrapT = runGenTWith (successor $ Counter . succ . getCounter)
                         (Counter 0)

runUnwrap :: Unwrap a -> a
runUnwrap = runIdentity . runUnwrapT

-- | Render a name unique within the scope of a monadic computation.
freshify :: (MonadUnwrap m, Functor m) => Fresh a -> m (Fresh a)
freshify nm = (\i -> nm{fresh = i}) <$> fmap getCounter gen

-- | Given a scope which binds one variable, unwrap it with a
-- variable. Note that @unwrap1@ will take care of @freshify@ing the
-- variable.
unwrap1 :: (Monad f, Functor m, MonadUnwrap m)
          => Fresh a
          -> Scope () f (Fresh a)
          -> m (Fresh a, f (Fresh a))
unwrap1 nm s = fmap head <$> unwrapAll1 nm [s]

-- | Given a list of scopes which bind one variable, unwrap them all
-- with the same variable. Note that @unwrapAll1@ will take care of
-- @freshify@ing the variable.
unwrapAll1 :: (Monad f, Functor m, MonadUnwrap m)
             => Fresh a
             -> [Scope () f (Fresh a)]
             -> m (Fresh a, [f (Fresh a)])
unwrapAll1 nm ss = do
  fnm <- freshify nm
  return $ (fnm, map (instantiate1 $ return fnm) ss)
