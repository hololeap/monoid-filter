{-|
Module      : Data.Monoid.Filter
Licesnse    : BSD-2
Stability   : experimental

Filters based on functions with the type @(a -> m (Maybe a))@.
-}

module Data.Monoid.Filter
    ( Filter
    , filter
    , modify
    , block
    , pass
    , runFilters
    )
    where

import Control.Arrow (Kleisli (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (fold)
import Data.Monoid.Endomorphism (Endomorphism (..))

import Prelude hiding (filter)

-- | A filter is a function @(a -> m (Maybe a))@. That is, it is an endomorphism
--   in the Kleisli category of @(MaybeT m)@.
type Filter m = Endomorphism (Kleisli (MaybeT m))

-- | Create a 'Filter' from a function
filter :: (a -> m (Maybe a)) -> Filter m a
filter = Endomorphism . Kleisli . (\f -> MaybeT . f)

-- | A 'Filter' which will modify @a@ and pass it along
modify :: Functor m => (a -> m a) -> Filter m a
modify = filter . (\f -> fmap Just . f)

-- | A 'Filter' which will consume @a@
block :: Functor m => (a -> m ()) -> Filter m a
block = filter . (\f -> fmap (const Nothing) . f)

-- | A 'Filter' which passes @a@ along unmodified
--
--   This is equivalent to 'mempty'
pass :: Applicative m => Filter m a
pass = filter (pure . Just)

-- | Pass a value through a chain of Filters
runFilters :: (Monad m, Foldable t) => t (Filter m a) -> a -> m (Maybe a)
runFilters = (\f -> runMaybeT . f) . runKleisli . getEndomorphism . fold
