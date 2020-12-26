module TradeEngine where

import Client.Account
import Client.Authentication
import Client.Orders
import Client.Positions
import Config (getAuthKeys)
import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Data.Aeson hiding (Options)
import Network.Wreq (Options, responseBody)

-- a million microseconds
seconds :: Int
seconds = 1000000

run :: IO ()
run = do
  print "Initializing:"
  initialize
  print "Trading:"
  tradeLoop keyId secretKey

initialize = do
  print "-- Retrieving authentication keys from local environment..."
  keys <-
    getAuthKeys
      >>= ( \case
              Just c -> return c
              Nothing -> error "---- Problem retrieving auth keys. Quitting."
          )
  let auth = authOptions keys
  print "---- Found keys and generated auth headers"

  print "-- Querying account status..."
  accountReponse <- queryAccount auth
  let account = accountReponse ^. responseBody
  print account
  print "-- Success"

  print "--Querying orders..."
  ordersResponse <- queryOrders auth
  let orders = ordersResponse ^. responseBody
  print orders
  print "-- Success"

  print "--Querying positions..."
  positionsResponse <- queryPositions auth
  let positions = positionsResponse ^. responseBody
  print positions
  print "-- Success"

  return auth

tradeLoop :: Options -> IO ()
tradeLoop auth = do
  print "Placing order..."
  let order =
        Order
          { _type = MARKET,
            symbol = "PLTR",
            quantity = 1,
            side = BUY,
            timeInForce = GTC,
            orderClass = SIMPLE,
            orderId = Nothing,
            advanced = Nothing
          }
  print $ encode order
  orderResponse <- placeOrder auth order
  print orderResponse
  threadDelay $ 10 * seconds
  tradeLoop auth
