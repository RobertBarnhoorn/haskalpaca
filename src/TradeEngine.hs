module TradeEngine (run, initialize, tradeLoop) where

import Alpaca 
import Data.Aeson
import Control.Lens ((^.))
import Control.Concurrent (threadDelay)
import Network.Wreq (responseBody)

-- a million microseconds
seconds :: Int
seconds = 1000000

run :: IO ()
run = do
  print "Initializing:"
  (keyId, secretKey) <- initialize
  print "Trading:"
  --tradeLoop keyId secretKey 

initialize = do
  print "-- Retrieving authentication keys from local environment..."
  maybeKeys <- authKeys
  let (keyId, secretKey) = case maybeKeys of
        (Just k1, Just k2) -> (k1, k2)
        (Nothing, Nothing) -> error $ "---- Neither Key ID nor secret " ++
                                      "key present.  Quitting..."
        (_, Nothing) -> error "---- Key ID present but secret key is not. Quitting..."
        (Nothing, _) -> error "---- Secret key present but key ID is not. Quitting..."
  print    "---- Keys present"
  print $ "---- Key ID: " ++ keyId
  print $ "---- Secret key: " ++ secretKey

  print "-- Querying account status..."
  accountReponse <- queryAccount keyId secretKey
  let account = accountReponse ^. responseBody
  print account

  print "--Querying orders..."
  ordersResponse <- queryOrders keyId secretKey
  let orders = ordersResponse ^. responseBody
  print orders

  print "--Querying positions..."
  positionsResponse <- queryPositions keyId secretKey
  let positions = positionsResponse ^. responseBody
  print positions 
  return (keyId, secretKey)

tradeLoop :: String -> String -> IO ()
tradeLoop keyId secretKey = do
  print "Scanning..."

  print "3. MAKING ORDER..."
  let order = Order {
    _type       = MARKET,
    symbol      = "AAPL",
    quantity    = 1,
    side        = BUY,
    timeInForce = GTC,
    orderClass  = SIMPLE,
    orderId     = Nothing,
    advanced    = Nothing
  }
  print $ encode order
  orderResponse <- placeOrder keyId secretKey order
  print orderResponse
  threadDelay $ 10 * seconds
  tradeLoop keyId secretKey
