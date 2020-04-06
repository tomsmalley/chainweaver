{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Storage.Class
  ( backupLocalStorage
  , StoreType (..)
  , HasStorage(..)
  , StoreKeyMetaPrefix(..)
  ) where

import Control.Monad.Reader
import Data.Aeson (ToJSON)
import Data.Constraint.Extras (Has')
import Data.Constraint.Forall (ForallF)
import Data.Dependent.Map (DMap, empty)
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Frontend.Foundation
import Numeric.Natural (Natural)
import Obelisk.Route.Frontend

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

{-- Notes for migration
 * We can't close off the keys to a single GADT because that would mean that
   desktop and web need to share the same storage keys, which will not work.
   Currently Desktop has the root BIP key on top of the rest of the storage.
 * The backup/restore process for the user is going to be interesting. Because we don't
   want to export the whole store because I don't think that we want to export the
   encrypted keys. It's not quite what we want for the versioning process so we are not
   going to get it "for free" sadly.
 * The current storage type locks things up in JSM which makes it really tricky to test.
   We could run the frontend storage tests in ghcjs but then we'd be stuck not being able to
   test the desktop migrations. We could parameterise Storage with a type parameter for the
   inner monad, but then HasStorage needs to become a MTPC and stuff gets weird for the rest
   of the app. Really, we just want to be able to test the derived functions from the algebra
   and we don't want "testability" stuff bleeding out into the app.
   For this reason, I'm going to change Storage to a Free Monad and HasStorage / StorageT just
   to something that holds an interpreter that can run the free in a given m. Way cleaner.
--}

data StoreType
  = StoreType_Local
  | StoreType_Session
  deriving (Eq, Show)

newtype StoreKeyMetaPrefix = StoreKeyMetaPrefix Text

encodeText :: Aeson.ToJSON a => a -> Text
encodeText = T.decodeUtf8 . BL.toStrict . Aeson.encode

backupKeyPrefixText :: StoreKeyMetaPrefix -> Natural -> Text
backupKeyPrefixText (StoreKeyMetaPrefix p) ver = (p <> "_Backups_V" <> tshow ver)

backupKeyText :: StoreKeyMetaPrefix -> Natural -> Natural -> Text
backupKeyText p ver seqNo = (backupKeyPrefixText p ver) <> "_" <> tshow seqNo

setBackup
  :: ( HasStorage m
     , ForallF ToJSON storeKeys
     , Has' ToJSON storeKeys f
     )
  => StoreKeyMetaPrefix
  -> Natural
  -> Natural
  -> DMap storeKeys f
  -> m ()
setBackup p ver seqNo dump = setItemStorage' localStorage (backupKeyText p ver seqNo) $ encodeText dump

backupLocalStorage
  :: forall storeKeys m
  . ( HasStorage m
    , Monad m
    , ForallF ToJSON storeKeys
    , Has' ToJSON storeKeys Identity
    )
  => StoreKeyMetaPrefix
  -> Natural  -- This version is the expectation set by the caller who has already chosen the key type
  -> m (Maybe (DMap storeKeys Identity))
backupLocalStorage p expectedVer = do
      setBackup p expectedVer 0 (empty :: DMap storeKeys Identity)
      pure Nothing

-- | Get access to browser's local storage.
localStorage :: StoreType
localStorage = StoreType_Local

class HasStorage m where
  getItemStorage' :: StoreType -> Text -> m (Maybe Text)
  setItemStorage' :: StoreType -> Text -> Text -> m ()
  removeItemStorage' :: StoreType -> Text -> m ()

  default getItemStorage' :: (m ~ t n, MonadTrans t, Monad n, HasStorage n) => StoreType -> Text -> m (Maybe Text)
  getItemStorage' t = lift . getItemStorage' t
  default setItemStorage' :: (m ~ t n, MonadTrans t, Monad n, HasStorage n) => StoreType -> Text -> Text -> m ()
  setItemStorage' t k = lift . setItemStorage' t k
  default removeItemStorage' :: (m ~ t n, MonadTrans t, Monad n, HasStorage n) => StoreType -> Text -> m ()
  removeItemStorage' t = lift . removeItemStorage' t

instance (HasStorage m, Monad m) => HasStorage (RoutedT t r m)
instance (HasStorage m, Monad m) => HasStorage (ReaderT r m)
