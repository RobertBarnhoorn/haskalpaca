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
run = authenticate >>= initialize >>= tradeLoop

authenticate :: IO Options
authenticate =
  getAuthKeys >>= \case
    Just c -> return $ authOptions c
    Nothing -> error "---- Problem retrieving auth keys. Quitting."

initialize :: Options -> IO Options
initialize auth = do
  print "-- Querying account status..."
  print =<< queryAccount auth
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
