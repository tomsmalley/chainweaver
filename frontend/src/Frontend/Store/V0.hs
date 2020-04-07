{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.Store.V0 ( StoreFrontend(StoreNetwork_Networks) ) where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint (Dict(Dict))
import Data.Constraint.Extras

import Frontend.Store.V0.Wallet

data StoreFrontend key a where
   StoreNetwork_Networks :: StoreFrontend key NetworkMap

instance ArgDict c (StoreFrontend key) where
  type ConstraintsFor (StoreFrontend key) c
    = ( c NetworkMap
      )
  argDict = \case
    StoreNetwork_Networks {} -> Dict

deriveJSONGADT ''StoreFrontend
