{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Store
  ( upgrade
  ) where

import Frontend.Storage.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy(Proxy))

import qualified Frontend.Store.V0 as V0

prefix :: StoreKeyMetaPrefix
prefix = StoreKeyMetaPrefix "StoreFrontend_Meta"

upgrade :: forall key m.
     ( ToJSON key
     , FromJSON key
     , Monad m
     , HasStorage m
     )
  => m (Maybe VersioningError)
upgrade = do
  _mDump <- backupLocalStorage prefix (Proxy @(V0.StoreFrontend key)) 0
  pure Nothing
