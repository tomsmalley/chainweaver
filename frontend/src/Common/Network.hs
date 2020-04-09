{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Network
  ( NodeRef
  ) where

import Data.Aeson
import Data.Text (Text)

import qualified Data.Text as T
import qualified Text.URI as URI hiding (uriPath)

tshow :: Show a => a -> Text
tshow = T.pack . show

type NodeRef = URI.Authority

instance FromJSON NodeRef where
  parseJSON = undefined

instance ToJSON NodeRef where
  toJSON = undefined

class IsRefPath r where
  renderRef :: r -> Text

instance IsRefPath Text where
  renderRef = id

instance IsRefPath NodeRef where
  renderRef = renderRef . renderNodeRef

renderNodeRef :: NodeRef -> Text
renderNodeRef (URI.Authority mUser h mp) =
    maybe "" ((<> "@") . renderUser) mUser <> URI.unRText h <> maybe "" ((":" <>) . tshow) mp
  where
    renderUser (URI.UserInfo name mPw) = URI.unRText name <> maybe "" ((":" <>) . URI.unRText) mPw
