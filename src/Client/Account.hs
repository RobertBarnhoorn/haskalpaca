module Client.Account where

import Client.Endpoints (accountEndpoint)
import Data.Aeson (Value)
import Data.ByteString.Lazy ()
import Network.Wreq (Options, Response, asValue, getWith)

queryAccount :: Options -> IO (Response Value)
queryAccount auth =
  asValue =<< getWith auth accountEndpoint
