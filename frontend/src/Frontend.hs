{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.Javascript.JSaddle ( liftJSM )
import qualified Servant.API as S
import qualified Servant.Client.JSaddle as S
import qualified Data.Aeson as Aeson ( ToJSON, encode )
import qualified Data.ByteString.Lazy as BL ( toStrict )
import qualified Data.Dependent.Map as DMap ( DMap, empty )
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity ( Identity )
import Data.Text ( Text )
import qualified Data.Text.Encoding as T ( decodeUtf8 )
import qualified Frontend.Store.V0 as V0 ( StoreFrontend )
import Data.Proxy

encodeText :: Aeson.ToJSON a => a -> Text
encodeText = T.decodeUtf8 . BL.toStrict . Aeson.encode

type TrivialApi = S.Get '[S.JSON] ()

trivialClient :: S.ClientM ()
trivialClient = S.client $ Proxy @TrivialApi

main :: IO ()
main = do
  encodeText (DMap.empty :: DMap.DMap V0.StoreFrontend Identity) `seq` pure ()

  let env = S.mkClientEnv $ S.BaseUrl S.Https "eu1.testnet.chainweb.com" 80 "/chainweb/0.0/testnet04/chain/8/pact"

  _ <- liftJSM $ S.runClientM trivialClient env

  pure ()
