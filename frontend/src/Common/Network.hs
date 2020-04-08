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
  ( NetworkName
  , NodeRef
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Lens
import Control.Monad
import Control.Monad.Except (MonadError, liftEither, runExceptT)
import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.URI.Lens

import qualified Data.Aeson.Types as A
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Text.Megaparsec as MP
import qualified Text.URI as URI hiding (uriPath)

import Common.RefPath as MP

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Name that uniquely describes a valid network.
newtype NetworkName = NetworkName
  { unNetworkName :: CI Text
  } deriving (Eq, Ord, Show)

instance FromJSON NetworkName where
  parseJSON = either (fail . T.unpack) pure . mkNetworkName <=< parseJSON
instance FromJSONKey NetworkName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack) pure . mkNetworkName
instance ToJSON NetworkName where
  toJSON = toJSON . CI.original . unNetworkName
instance ToJSONKey NetworkName where
  toJSONKey = A.toJSONKeyText (CI.original . unNetworkName)

-- | Construct a 'NetworkName', and banish mainnet - for now.
mkNetworkName :: Text -> Either Text NetworkName
mkNetworkName (T.strip -> t) = Right $ NetworkName $ CI.mk t

-- | Reference for a node in a network.
newtype NodeRef = NodeRef
  { unNodeRef :: URI.Authority
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NodeRef where
  parseJSON v = do
    t <- parseJSON v
    errVal <- runExceptT $ parseNodeRef t
    case errVal of
      Left err  -> fail $ T.unpack err
      Right val -> pure val

instance ToJSON NodeRef where
  toJSON = toJSON . renderNodeRef


instance IsRefPath NodeRef where
  renderRef = mkRefPath . renderNodeRef

  parseRef = do
    v <- MP.anySingle
    errVal <- runExceptT $ parseNodeRef v
    case errVal of
      Left err  -> fail $ T.unpack err
      Right val -> pure val

-- | Parse an authority from Text, failing with an error message if that is not possible.
parseNodeRef :: forall m. MonadError Text m => Text -> m NodeRef
parseNodeRef t = do
    let
      -- This is just to make the URI parser perform as we need it:
      normalizedT = ("https://" <>) . fromMaybe t $ T.stripPrefix "http://" t <|> T.stripPrefix "https://" t

    uri <- parseLifted URI.parser normalizedT
    liftEither . left (const "Parsing hostname failed.") $ fmap NodeRef (uri ^. uriAuthority)

  where

    parseLifted :: forall a mp. MonadError Text mp => MP.Parsec Void Text a -> Text -> mp a
    parseLifted p s = liftEither . left (T.pack . show) $ MP.runParser p "uri" s


-- | Render an authority useful for serialization to disk.
renderNodeRef :: NodeRef -> Text
renderNodeRef (NodeRef (URI.Authority mUser h mp)) =
    maybe "" ((<> "@") . renderUser) mUser <> URI.unRText h <> maybe "" ((":" <>) . tshow) mp
  where
    renderUser (URI.UserInfo name mPw) = URI.unRText name <> maybe "" ((":" <>) . URI.unRText) mPw
