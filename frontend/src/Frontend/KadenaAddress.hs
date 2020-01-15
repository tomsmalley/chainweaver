{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.KadenaAddress where

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Pact.Types.ChainId
import qualified Pact.Types.Term as Pact
import Kadena.SigningApi (AccountName(..))
import Data.Default (def)

import Common.Wallet

data KadenaAddress = KadenaAddress
  { _kadenaAddress_accountName :: AccountName
    -- ^ The account name associated with this address
  , _kadenaAddress_chainId :: ChainId
    -- ^ The chain where this account resides
  , _kadenaAddress_keyset :: Maybe AddressKeyset
    -- ^ Presence or absence of a keyset may be used to determine transfer vs
    -- transfer-create. If the keyset is present and the account already exists
    -- you could choose to do either a transfer or a transfer-create.
  }
  deriving (Show, Eq)

instance ToJSON KadenaAddress where
  toJSON o = object $ catMaybes
      [ Just $ "account" .= _kadenaAddress_accountName o
      , Just $ "chain" .= _kadenaAddress_chainId o
      , ("keyset" .=) <$> _kadenaAddress_keyset o
      ]

instance FromJSON KadenaAddress where
  parseJSON = withObject "KadenaAddress" $ \o -> KadenaAddress
    <$> o .: "account"
    <*> o .: "chain"
    <*> o .:? "keyset"
