{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend where

import           Reflex.Dom.Core
import Pact.Server.ApiV1Client (runTransactionLoggerT, logTransactionStdout)

import           Obelisk.Frontend
import           Obelisk.Route.Frontend

import           Common.Route
import           Frontend.Crypto.Browser
import           Frontend.Storage

import Data.Aeson (FromJSON, ToJSON)
import Language.Javascript.JSaddle (liftJSM)
import Obelisk.Route (R)

import Frontend.Crypto.Class
import qualified Frontend.Store as Store

import qualified Servant.Client.JSaddle            as S

import qualified Data.Text.Encoding as T
import Pact.Server.ApiV1Client as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Hash as Pact

app :: forall key t m.
     ( MonadWidget t m
     , HasStorage m
     , HasCrypto key m
     , FromJSON key, ToJSON key
     )
  => RoutedT t (R FrontendRoute) m ()
app = do
  _ <- _storageVersioner_upgrade $ Store.versioner @key

  let env = S.mkClientEnv $ S.BaseUrl S.Https "eu1.testnet.chainweb.com" 80 "/chainweb/0.0/testnet04/chain/8/pact"
      cmd = fmap T.decodeUtf8 $ Pact.Command "" [] $ Pact.hash ""

  _ <- liftJSM $ flip S.runClientM env $ Pact.local apiV1Client cmd

  pure ()

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = blank
  , _frontend_body = prerender_ blank $ do
    mapRoutedT (flip runTransactionLoggerT logTransactionStdout . runBrowserStorageT . runBrowserCryptoT) app
  }
