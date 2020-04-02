{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Store
  ( upgrade
  ) where

import Frontend.Storage.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy(Proxy))
import Reflex
import Reflex.Dom

import qualified Frontend.Store.V0 as V0
import qualified Frontend.Store.V1 as V1
import Frontend.Store.V1 as Latest
import Frontend.Crypto.Class

prefix :: StoreKeyMetaPrefix
prefix = StoreKeyMetaPrefix "StoreFrontend_Meta"

upgrade :: forall key m.
     ( ToJSON key
     , FromJSON key
     , Monad m
     , HasStorage m
     , HasCrypto key m
     )
  => m (Maybe VersioningError)
upgrade = do
      ver <- getCurrentVersion prefix
      case ver of
        0 -> do
          mDump <- backupLocalStorage prefix (Proxy @(V0.StoreFrontend key)) 0
          case mDump of
            Nothing -> error "TODO Add a version error case for this"
            Just dump -> do
              v1Dump <- V1.upgradeFromV0 dump
              removeKeyUniverse (Proxy @(V0.StoreFrontend key)) localStorage
              removeKeyUniverse (Proxy @(V0.StoreFrontend key)) sessionStorage
              restoreLocalStorageDump prefix v1Dump 1
              pure Nothing
        1 -> pure Nothing
        v -> pure $ Just $ VersioningError_UnknownVersion v
