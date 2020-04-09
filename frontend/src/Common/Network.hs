{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Common.Network
  ( NodeRef
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Text.URI as URI hiding (uriPath)

import Common.RefPath as MP

tshow :: Show a => a -> Text
tshow = T.pack . show

newtype NodeRef = NodeRef
  { unNodeRef :: URI.Authority
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NodeRef where
  parseJSON = undefined

instance ToJSON NodeRef where
  toJSON = undefined

instance IsRefPath NodeRef where
  renderRef = mkRefPath . renderNodeRef

renderNodeRef :: NodeRef -> Text
renderNodeRef (NodeRef (URI.Authority mUser h mp)) =
    maybe "" ((<> "@") . renderUser) mUser <> URI.unRText h <> maybe "" ((":" <>) . tshow) mp
  where
    renderUser (URI.UserInfo name mPw) = URI.unRText name <> maybe "" ((":" <>) . URI.unRText) mPw
