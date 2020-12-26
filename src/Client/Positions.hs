module Client.Positions where

import Client.Endpoints (positionsEndpoint)
import Data.Aeson (Value)
import Network.Wreq (Options, Response, asValue, deleteWith, getWith)

queryPosition :: Options -> String -> IO (Response Value)
queryPosition opts symbol = asValue =<< getWith opts (positionsEndpoint ++ symbol)

queryPositions :: Options -> IO (Response Value)
queryPositions opts = asValue =<< getWith opts positionsEndpoint

closePosition :: Options -> String -> IO (Response Value)
closePosition opts symbol =
  asValue =<< deleteWith opts (positionsEndpoint ++ symbol)

closeAllPositions :: Options -> IO (Response Value)
closeAllPositions opts = asValue =<< deleteWith opts positionsEndpoint
