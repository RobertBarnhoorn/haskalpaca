-- | Example library usage
module Main where

import Alpaca.Account
import Alpaca.Authentication
import Alpaca.Orders
import Alpaca.Positions
import Control.Concurrent (threadDelay)
import Network.Wreq (Options)

main :: IO ()
main = getAuth >>= showDiagnostics >>= tradeLoop

showDiagnostics :: Options -> IO Options
showDiagnostics opts = do
  print "Querying account status..."
  print =<< getAccount opts

  print "Querying orders..."
  print =<< getOrders opts

  print "Querying positions..."
  print =<< getPositions opts

  return opts

tradeLoop :: Options -> IO ()
tradeLoop opts = do
  let order =
        OrderRequest
          { orderType = MARKET,
            symbol = "PLTR",
            quantity = 1,
            side = BUY,
            timeInForce = GTC,
            orderClass = SIMPLE,
            trackingId = Nothing,
            advanced = Nothing
          }

  print "Placing $PLTR order..."
  print =<< placeOrder order opts

  print "Replacing $PLTR with $AAPL order..."
  print =<< replaceOrder order {symbol = "AAPL"} opts

  print "Cancelling orders and liquidating all positions..."
  print =<< exitMarket opts

  threadDelay tenSeconds
  tradeLoop opts
  where
    tenSeconds = 10000000 -- 10 million microseconds
