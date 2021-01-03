module Alpaca.Endpoints where

baseEndpoint :: String
baseEndpoint = "https://paper-api.alpaca.markets" /. "v2"

accountEndpoint :: String
accountEndpoint = baseEndpoint /. "account"

ordersEndpoint :: String
ordersEndpoint = baseEndpoint /. "orders"

positionsEndpoint :: String
positionsEndpoint = baseEndpoint /. "positions"

-- | Append to a URL path
(/.) :: String -> String -> String
x /. y = x ++ "/" ++ y
