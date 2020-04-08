{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}


module Pact.Server.API
  ( apiV1API
  ) where

import Data.Proxy
import Data.Text (Text)
import Servant.API

import Pact.Types.Command
import Pact.Types.Hash

-- | Public Pact REST API.
type ApiV1API = "api" :> "v1" :> ApiLocal

apiV1API :: Proxy ApiV1API
apiV1API = Proxy

type ApiLocal = "local"
  :> ReqBody '[JSON] (Command Text)
  :> Post '[JSON] (CommandResult Hash)
