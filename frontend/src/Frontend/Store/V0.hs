{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.Store.V0 ( StoreFrontend(StoreNetwork_Networks) ) where

import Data.Aeson ( FromJSON(parseJSON), ToJSON(toJSON) )
import Data.Aeson.GADT.TH ( deriveJSONGADT )
import Data.Constraint ( Dict(Dict) )
import Data.Constraint.Extras ( ArgDict(..) )
import Common.Network ( NodeRef )

data StoreFrontend a where
   StoreNetwork_Networks :: StoreFrontend NodeRef

instance ArgDict c StoreFrontend where
  type ConstraintsFor StoreFrontend c
    = ( c NodeRef
      )
  argDict = \case
    StoreNetwork_Networks {} -> Dict

deriveJSONGADT ''StoreFrontend
