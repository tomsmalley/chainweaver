{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Limit API to the parts that are common to chainweb and `pact -s`.
module Pact.Server.ApiV1Client
  ( ApiV1Client(local)
  , apiV1Client
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader hiding (local)
import Data.Proxy
import Data.Text (Text)
import Pact.Server.API ( apiV1API )
import Pact.Types.Command ( Command, CommandResult )
import Pact.Types.Hash ( Hash )
import Servant.Client.Core hiding (Client)
import Servant.Client.JSaddle hiding (Client)

data ApiV1Client m = ApiV1Client
  { local :: Command Text -> m (CommandResult Hash)
  }

{- apiV1API :: Proxy ApiV1API -}
{- apiV1API = Proxy -}
apiV1Client :: forall m. (MonadIO m, MonadReader ClientEnv m, RunClient m) => ApiV1Client m
apiV1Client = ApiV1Client
  { local = localF
  }
  where
    localF = clientIn apiV1API (Proxy :: Proxy m)
