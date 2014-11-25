module Bound.Unwrap where
import Bound
import Control.Applicative
import Control.Monad.Gen

-- | Substitute a single bound variable for a free one.
unbindWith :: Monad f => a -> Scope () f a -> f a
unbindWith = instantiate1 . return

-- | Unbind a list of scopes together with the same free
-- variable.
unbindTogether :: (Monad f, Functor m, MonadGen a m)
                  => [Scope () f a] -> m [f a]
unbindTogether [] = return [] -- Avoid an unnecessary gen
unbindTogether scopes = (\a -> map (unbindWith a) scopes) <$> gen

-- | The core of the library. Use a 'MonadGen' instance to generate a
-- fresh free variable and instantiate a scope with it. This is useful
-- for working under a binder without fear of shadowing an existing
-- free variable.
unbind :: (Monad f, Functor m, MonadGen a m) => Scope () f a -> m (f a)
unbind scope = flip unbindWith scope <$> gen
