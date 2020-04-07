{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.Store.V0 ( StoreFrontend(StoreNetwork_Networks) ) where

import Data.Aeson ( FromJSON(parseJSON), ToJSON(toJSON) )
import Data.Aeson.GADT.TH ( deriveJSONGADT )
import Data.Constraint ( Dict(Dict) )
import Data.Constraint.Extras ( ArgDict(..) )
import Frontend.Store.V0.Wallet ( NetworkMap )

data StoreFrontend a where
   StoreNetwork_Networks :: StoreFrontend NetworkMap

instance ArgDict c StoreFrontend where
  type ConstraintsFor StoreFrontend c
    = ( c NetworkMap
      )
  argDict = \case
    StoreNetwork_Networks {} -> Dict

deriveJSONGADT ''StoreFrontend
