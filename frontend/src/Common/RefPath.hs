{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Common.RefPath
  ( IsRefPath (..)
  , mkRefPath
  ) where

import           Data.Text            (Text)

type PathSegment = Text

newtype RefPath = RefPath { unRefPath :: [ PathSegment ] }
 deriving (Monoid, Semigroup, Show, Eq, Ord)

class IsRefPath r where
  renderRef :: r -> RefPath

instance IsRefPath Text where
  renderRef = mkRefPath

mkRefPath :: PathSegment -> RefPath
mkRefPath = RefPath . pure
