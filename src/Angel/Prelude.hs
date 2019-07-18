-- | Compatibility Prelude to maximise GHC support.
-- Re-exports from base only.

{-# LANGUAGE NoImplicitPrelude #-}
module Angel.Prelude (
    module Control.Applicative
  , module Data.Functor
  , module Data.Monoid
  , module Prelude
  ) where

import           Control.Applicative (Applicative (..))
import           Data.Functor ((<$>))
import           Data.Monoid ((<>))
import           Prelude
