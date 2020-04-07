{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend where

import           Reflex.Dom.Core

import           Obelisk.Frontend
import           Obelisk.Route.Frontend

import           Common.Route

import Language.Javascript.JSaddle (liftJSM)
import Obelisk.Route (R)

import qualified Servant.Client.JSaddle            as S

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Pact.Server.ApiV1Client as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Hash as Pact

import qualified Frontend.Store.V0 as V0 (StoreFrontend)

encodeText :: Aeson.ToJSON a => a -> Text
encodeText = T.decodeUtf8 . BL.toStrict . Aeson.encode

app :: forall t m.
     ( MonadWidget t m
     )
  => RoutedT t (R FrontendRoute) m ()
app = do
  encodeText (DMap.empty :: DMap.DMap V0.StoreFrontend Identity) `seq` pure ()

  let env = S.mkClientEnv $ S.BaseUrl S.Https "eu1.testnet.chainweb.com" 80 "/chainweb/0.0/testnet04/chain/8/pact"
      cmd = fmap T.decodeUtf8 $ Pact.Command "" [] $ Pact.hash ""

  _ <- liftJSM $ flip S.runClientM env $ Pact.local apiV1Client cmd

  pure ()

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = blank
  , _frontend_body = prerender_ blank app
  }
