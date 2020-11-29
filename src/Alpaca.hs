{-# LANGUAGE OverloadedStrings #-}

module Alpaca where

import Control.Lens ((&), (.~))
import Data.Aeson ((.=), encode, toJSON, object, ToJSON, Value)
import Data.ByteString.Lazy()
import qualified Data.ByteString.UTF8 as BSU (fromString)
import Data.Text ()
import Network.Wreq (asValue, customPayloadMethodWith, defaults, deleteWith,
                     getWith, header, postWith, Options, Response)
import System.Environment (lookupEnv)

data TimeInForce = DAY | GTC | OPG | CLS | IOC | FOK
  deriving (Read, Show)

data OrderType = MARKET | LIMIT | STOP | STOP_LIMIT | TRAILING_STOP
  deriving (Read, Show)

data Side = BUY | SELL
  deriving (Read, Show)

data OrderClass = SIMPLE | BRACKET | OCO | OTO
  deriving (Read, Show)

newtype TakeProfit = TakeProfit {
  takeProfitlimitPrice :: Double -- required for bracket orders
} deriving (Read, Show)

data StopLoss = StopLoss {
  stopLossStopPrice  :: Double, -- required for bracket orders
  stopLosslimitPrice :: Double  -- the stop-loss order becomes a stop-limit order if specified
} deriving (Read, Show)

data AdvancedParams = 
  LimitParams {
    limitPrice    :: Maybe Double,   -- req. if type is limit or stop_limit
    stopPrice     :: Maybe Double,   -- req. if type is stop or stop_limit
    extendedHours :: Maybe Bool      -- only: limit and TOF = day
  } |
  TrailingStopParams {
  trailPrice    :: Maybe Double,     -- price or percent required if trailing_stop
  trailPercent  :: Maybe Double,     -- price or percent required if trailing_stop
  takeProfit    :: Maybe TakeProfit, -- for take-profit leg of advanced orders
  stopLoss      :: Maybe StopLoss    -- for stop-loss leg of advanced orders
  } deriving (Read, Show)

data Order = Order {
  _type       :: OrderType,
  symbol      :: String,           -- symbol or id to identify the asset
  quantity    :: Int,
  side        :: Side,
  timeInForce :: TimeInForce,
  orderClass  :: OrderClass,
  orderId     :: Maybe String,     -- id for tracking; auto-generated if not sent
  advanced    :: Maybe AdvancedParams
}

instance ToJSON Order where
  toJSON Order {_type=_type, symbol=sym, quantity=qty, side=side, timeInForce=tif,
                 orderClass=cls, orderId=_, advanced=_} =
    object [
      "type" .= case _type of
        MARKET -> "market" :: String
        LIMIT -> "limit"
        STOP -> "stop"
        STOP_LIMIT -> "stop_limit"
        TRAILING_STOP -> "trailing_stop",
      "symbol" .= sym,
      "qty" .= show qty,
      "side" .= case side of
        BUY -> "buy" :: String
        SELL -> "sell",
      "time_in_force" .= case tif of
        DAY -> "day" :: String
        GTC -> "gtc"
        OPG -> "opg"
        CLS -> "cls"
        IOC -> "ioc"
        FOK -> "fok",
      "order_class" .= case cls of
        SIMPLE -> "simple" :: String
        BRACKET -> "bracket"
        OCO -> "oco"
        OTO -> "oto"
    ]

baseEndpoint :: String
baseEndpoint = "https://paper-api.alpaca.markets"

accountEndpoint :: String
accountEndpoint = baseEndpoint ++ "/v2/account"

ordersEndpoint :: String
ordersEndpoint = baseEndpoint ++ "/v2/orders"

positionsEndpoint :: String
positionsEndpoint = baseEndpoint ++ "/v2/positions"

authKeys :: IO (Maybe String, Maybe String)
authKeys = do
  keyId <- lookupEnv "ALPACA_KEY_ID"
  secretKey <- lookupEnv "ALPACA_SECRET_KEY"
  return (keyId, secretKey)

authOptions :: String -> String -> Options
authOptions keyId secretKey =
  defaults
    & (header "APCA-API-KEY-ID" .~ [BSU.fromString keyId])
    & (header "APCA-API-SECRET-KEY" .~ [BSU.fromString secretKey])

queryAccount :: String -> String -> IO (Response Value)
queryAccount keyId secretKey =
  asValue =<< getWith (authOptions keyId secretKey) accountEndpoint

queryOrders :: String -> String -> IO (Response Value)
queryOrders keyId secretKey =
  asValue =<< getWith (authOptions keyId secretKey) ordersEndpoint

queryOrder :: String -> String -> String -> IO (Response Value)
queryOrder keyId secretKey orderId =
  asValue =<< getWith (authOptions keyId secretKey) 
                      (ordersEndpoint ++ "/" ++ orderId)

placeOrder :: String -> String -> Order -> IO (Response Value)
placeOrder keyId secretKey order =
  asValue =<< postWith (authOptions keyId secretKey)
                       ordersEndpoint
                       (encode order)

replaceOrder :: String -> String -> Order -> IO (Response Value)
replaceOrder keyId secretKey order = 
  asValue =<< patchWith (authOptions keyId secretKey)
                        ordersEndpoint
                        (encode order)
    where patchWith = customPayloadMethodWith "PATCH" 

cancelAllOrders :: String -> String -> IO (Response Value)
cancelAllOrders keyId secretKey =
  asValue =<< deleteWith (authOptions keyId secretKey) ordersEndpoint

cancelOrder :: String -> String -> String -> IO (Response Value)
cancelOrder keyId secretKey orderId =
  asValue =<< deleteWith (authOptions keyId secretKey) 
                         (ordersEndpoint ++ "/" ++ orderId)

queryPositions :: String -> String -> IO (Response Value)
queryPositions keyId secretKey =
  asValue =<< getWith (authOptions keyId secretKey)
                      positionsEndpoint
