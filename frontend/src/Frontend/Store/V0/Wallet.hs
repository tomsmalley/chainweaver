module Frontend.Store.V0.Wallet (NetworkMap(..)) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Common.Network (NetworkName, NodeRef)

newtype NetworkMap = NetworkMap { unNetworkMap :: Map NetworkName [NodeRef] }

instance ToJSON NetworkMap where toJSON = toJSON . Map.toList . unNetworkMap
instance FromJSON NetworkMap where parseJSON = fmap (NetworkMap . Map.fromList) . parseJSON
