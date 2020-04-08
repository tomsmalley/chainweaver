{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Pact.Types.Command
-- Copyright   :  (C) 2016 Stuart Popejoy, Will Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Will Martino <will@kadena.io>
--
-- Specifies types for commands in a consensus/DL setting.
--

module Pact.Types.Command
  ( Command(..)
  , CommandResult
  , RequestKey
  , requestKeyToB16Text
  ) where


import Control.Applicative
import Control.DeepSeq

import Data.Serialize as SZ
import Data.Hashable (Hashable)
import Data.Aeson as A
import Data.Text (Text)
import Data.Maybe  (fromMaybe)

import GHC.Generics


import Pact.Types.Capability
import Pact.Types.ChainId
import Pact.Types.Orphans ()
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)

import Pact.Types.Scheme (PPKScheme(..))


-- | Command is the signed, hashed envelope of a Pact execution instruction or command.
-- In 'Command ByteString', the 'ByteString' payload is hashed and signed; the ByteString
-- being the JSON serialization of 'Payload Text', where the 'Text' is the pact code; when
-- executed this is parsed to 'ParsedCode'.
-- Thus, 'Command (Payload m ParsedCode)' (with m representing platform-specific metadata)
-- is the fully executable specialization.
data Command a = Command
  { _cmdPayload :: !a
  , _cmdSigs :: ![UserSig]
  , _cmdHash :: !PactHash
  } deriving (Eq,Show,Ord,Generic,Functor,Foldable,Traversable)
instance (Serialize a) => Serialize (Command a)
instance (ToJSON a) => ToJSON (Command a) where
    toJSON (Command payload uSigs hsh) =
        object [ "cmd" .= payload
               , "sigs" .= toJSON uSigs
               , "hash" .= hsh
               ]
instance (FromJSON a) => FromJSON (Command a) where
    parseJSON = withObject "Command" $ \o ->
                Command <$> (o .: "cmd")
                        <*> (o .: "sigs" >>= parseJSON)
                        <*> (o .: "hash")
    {-# INLINE parseJSON #-}

instance NFData a => NFData (Command a)


-- | Strict Either thing for attempting to deserialize a Command.
data ProcessedCommand m a =
  ProcSucc !(Command (Payload m a)) |
  ProcFail !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance (NFData a,NFData m) => NFData (ProcessedCommand m a)


-- | Signer combines PPKScheme, PublicKey, and the Address (aka the
--   formatted PublicKey).
data Signer = Signer
 { _siScheme :: !(Maybe PPKScheme)
 -- ^ PPKScheme, which is defaulted to 'defPPKScheme' if not present
 , _siPubKey :: !Text
 -- ^ pub key value
 , _siAddress :: !(Maybe Text)
 -- ^ optional "address", for different pub key formats like ETH
 , _siCapList :: [SigCapability]
 -- ^ clist for designating signature to specific caps
 } deriving (Eq, Ord, Show, Generic)

instance NFData Signer
instance ToJSON Signer where
  toJSON Signer{..} = object $
    consMay "scheme" _siScheme $
    consMay "addr" _siAddress $
    consListMay "clist" _siCapList $
    [ "pubKey" .= _siPubKey ]
    where
      consMay f mv ol = maybe ol (consPair f ol) mv
      consPair f ol v = (f .= v):ol
      consListMay f cl ol
        | null cl = ol
        | otherwise = consPair f ol cl
instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> Signer
    <$> o .:? "scheme"
    <*> o .: "pubKey"
    <*> o .:? "addr"
    <*> (listMay <$> (o .:? "clist"))
    where
      listMay = fromMaybe []



-- | Payload combines a 'PactRPC' with a nonce and platform-specific metadata.
data Payload m c = Payload
  { _pPayload :: !(PactRPC c)
  , _pNonce :: !Text
  , _pMeta :: !m
  , _pSigners :: ![Signer]
  , _pNetworkId :: !(Maybe NetworkId)
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance (NFData a,NFData m) => NFData (Payload m a)
instance (ToJSON a,ToJSON m) => ToJSON (Payload m a) where toJSON = lensyToJSON 2
instance (FromJSON a,FromJSON m) => FromJSON (Payload m a) where parseJSON = lensyParseJSON 2



newtype UserSig = UserSig { _usSig :: Text }
  deriving (Eq, Ord, Show, Generic)

instance NFData UserSig
instance Serialize UserSig
instance ToJSON UserSig where
  toJSON UserSig {..} = object [ "sig" .= _usSig ]
instance FromJSON UserSig where
  parseJSON = withObject "UserSig" $ \o -> do
    UserSig <$> o .: "sig"


newtype PactResult = PactResult (Either PactError PactValue)
  deriving (Eq, Show, Generic)
instance ToJSON PactResult where
  toJSON (PactResult (Right s)) =
    object [ "status" .= ("success" :: String)
           , "data" .= s ]
  toJSON (PactResult (Left f)) =
    object [ "status" .= ("failure" :: String)
           , "error" .= f ]
instance FromJSON PactResult where
  parseJSON (A.Object o) = PactResult <$>
                           ((Left <$> o .: "error") <|>
                            (Right <$> o .: "data"))
  parseJSON p = fail $ "Invalid PactResult " ++ show p

-- | API result of attempting to execute a pact command, parametrized over level of logging type
data CommandResult l = CommandResult {
  -- | Request Key of command (the hash of the command payload)
    _crReqKey :: !RequestKey
  -- | Transaction id of this CommandResult
  , _crTxId :: !(Maybe TxId)
  -- | Pact execution result, either a PactError or the last pact expression output as a PactValue
  , _crResult :: !PactResult
  -- | Gas consummed by command
  , _crGas :: !Gas
  -- | Level of logging (i.e. full TxLog vs hashed logs)
  , _crLogs :: !(Maybe l)
  -- | Output of a Continuation if one occurred in the command.
  , _crContinuation :: !(Maybe PactExec)
  -- | Platform-specific data
  , _crMetaData :: !(Maybe Value)
  } deriving (Eq,Show,Generic)
instance (ToJSON l) => ToJSON (CommandResult l) where toJSON = lensyToJSON 3
instance (FromJSON l) => FromJSON (CommandResult l) where parseJSON = lensyParseJSON 3

requestKeyToB16Text :: RequestKey -> Text
requestKeyToB16Text (RequestKey h) = hashToText h


newtype RequestKey = RequestKey { unRequestKey :: Hash}
  deriving (Eq, Ord, Generic, Serialize, Hashable, ParseText, FromJSON, ToJSON, ToJSONKey)

instance Show RequestKey where
  show (RequestKey rk) = show rk
