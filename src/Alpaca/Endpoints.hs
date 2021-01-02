module Alpaca.Endpoints
  ( (/.),
    accountEndpoint,
    ordersEndpoint,
    positionsEndpoint,
  )
where

(/.) :: String -> String -> String
x /. y = x ++ "/" ++ y

baseEndpoint :: String
baseEndpoint = "https://paper-api.alpaca.markets" /. "v2"

accountEndpoint :: String
accountEndpoint = baseEndpoint /. "account"

ordersEndpoint :: String
ordersEndpoint = baseEndpoint /. "orders"

positionsEndpoint :: String
positionsEndpoint = baseEndpoint /. "positions"
