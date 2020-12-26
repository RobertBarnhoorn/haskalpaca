module Client.Endpoints where

baseEndpoint :: String
baseEndpoint = "https://paper-api.alpaca.markets"

accountEndpoint :: String
accountEndpoint = baseEndpoint <> "/v2/account"

ordersEndpoint :: String
ordersEndpoint = baseEndpoint <> "/v2/orders"

positionsEndpoint :: String
positionsEndpoint = baseEndpoint <> "/v2/positions"
