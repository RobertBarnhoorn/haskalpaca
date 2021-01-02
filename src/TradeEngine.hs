module TradeEngine where

import Alpaca.Account
import Alpaca.Authentication
import Alpaca.Orders
import Alpaca.Positions
import Control.Concurrent (threadDelay)
import Network.Wreq (Options)

run :: IO ()
run = authenticate >>= initialize >>= tradeLoop

initialize :: Options -> IO Options
initialize opts = do
  print "-- Querying account status..."
  print =<< getAccount opts
  print "-- Success"

  print "-- Querying orders..."
  print =<< getOrders opts
  print "-- Success"

  print "-- Querying a specific order..."
  print =<< getOrder "98257221-ea8d-44a8-bffc-7411b4b610d1" opts
  print "-- Success"

  print "-- Querying a specific position..."
  print =<< getPosition "AAPL" opts

  print "-- Querying positions..."
  print =<< getAllPositions opts

  return opts

tradeLoop :: Options -> IO ()
tradeLoop opts = do
  print "Placing $PLTR order..."
  let order =
        OrderRequest
          { _type = MARKET,
            symbol = "PLTR",
            quantity = 1,
            side = BUY,
            timeInForce = GTC,
            orderClass = SIMPLE,
            orderId = Nothing,
            advanced = Nothing
          }
  orderResponse <- placeOrder order opts
  print orderResponse

  print "-- Closing $PLTR position..."
  print =<< liquidate "PLTR" 1 opts
  print "-- Success"

  threadDelay $ 10 * seconds
  tradeLoop opts
  where
    seconds = 1000000 -- a million microseconds
