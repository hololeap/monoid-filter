{-|
Module      : Data.Monoid.Filter.Pure
Licesnse    : BSD-2
Stability   : experimental

Filters based on functions with the type @(a -> Maybe a)@.
A pure version of 'Data.Monoid.Filter'
-}

module Data.Monoid.Filter.Pure
    ( Filter
    , filter
    , modify
    , block
    , pass
    , runFilters
    )
    where

import Data.Functor.Identity (Identity (..))

import Prelude hiding (filter)

import qualified Data.Monoid.Filter as Filter

-- | A pure version of 'Filter.Filter'.
--
--   A filter is a function @(a -> Maybe a)@. That is, it is an endomorphism
--   in the Kleisli category of @Maybe@.
type Filter = Filter.Filter Identity

-- | A pure version of 'Filter.filter'.
--
--   Create a 'Filter' from a function
filter :: (a -> Maybe a) -> Filter a
filter f = Filter.filter (Identity . f)

-- | A pure version of 'Filter.modify'.
--
--   A 'Filter' which will modify @a@ and pass it along
modify :: (a -> a) -> Filter a
modify f = filter (Just . f)

-- | A pure version of 'Filter.block'
-- 
--   A 'Filter' which will consume @a@
block :: Filter a
block = filter (const Nothing)

-- | A pure version of 'Filter.pass'
-- 
--   A 'Filter' which passes @a@ along unmodified
--
--   This is equivalent to 'mempty'
pass :: Filter a
pass = mempty

-- | A pure version of 'Filter.runFilters'
--
--   Pass a value through a chain of Filters
runFilters :: Foldable t => t (Filter a) -> a -> Maybe a
runFilters filts = runIdentity . Filter.runFilters filts
