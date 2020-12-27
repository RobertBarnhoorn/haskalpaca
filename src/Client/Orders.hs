module Client.Orders where

import Client.Endpoints (ordersEndpoint)
import Data.Aeson (ToJSON, Value, encode, object, toJSON, (.=))
import Network.Wreq
  ( Options,
    Response,
    asValue,
    customPayloadMethodWith,
    deleteWith,
    getWith,
    postWith,
  )

data TimeInForce = DAY | GTC | OPG | CLS | IOC | FOK
  deriving (Read, Show)

data OrderType = MARKET | LIMIT | STOP | STOP_LIMIT | TRAILING_STOP
  deriving (Read, Show)

data Side = BUY | SELL
  deriving (Read, Show)

data OrderClass = SIMPLE | BRACKET | OCO | OTO
  deriving (Read, Show)

newtype TakeProfit = TakeProfit
  { takeProfitlimitPrice :: Double -- required for bracket orders
  }
  deriving (Read, Show)

data StopLoss = StopLoss
  { stopLossStopPrice :: Double, -- required for bracket orders
    stopLosslimitPrice :: Double -- the stop-loss order becomes a stop-limit order if specified
  }
  deriving (Read, Show)

data AdvancedParams
  = LimitParams
      { limitPrice :: Maybe Double, -- req. if type is limit or stop_limit
        stopPrice :: Maybe Double, -- req. if type is stop or stop_limit
        extendedHours :: Maybe Bool -- only: limit and TIF = day
      }
  | TrailingStopParams
      { trailPrice :: Maybe Double, -- price or percent required if trailing_stop
        trailPercent :: Maybe Double, -- price or percent required if trailing_stop
        takeProfit :: Maybe TakeProfit, -- for take-profit leg of advanced orders
        stopLoss :: Maybe StopLoss -- for stop-loss leg of advanced orders
      }
  deriving (Read, Show)

data Order = Order
  { _type :: OrderType,
    symbol :: String, -- symbol or id to identify the asset
    quantity :: Int,
    side :: Side,
    timeInForce :: TimeInForce,
    orderClass :: OrderClass,
    orderId :: Maybe String, -- id for tracking; auto-generated if not sent
    advanced :: Maybe AdvancedParams
  }

instance ToJSON Order where
  toJSON
    Order
      { _type = _type,
        symbol = sym,
        quantity = qty,
        side = side,
        timeInForce = tif,
        orderClass = cls,
        orderId = _,
        advanced = _
      } =
      object
        [ "type" .= case _type of
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

queryOrders :: Options -> IO (Response Value)
queryOrders opts =
  asValue =<< getWith opts ordersEndpoint

queryOrder :: Options -> String -> IO (Response Value)
queryOrder opts orderId =
  asValue =<< getWith opts (ordersEndpoint <> "/" <> orderId)

placeOrder :: Options -> Order -> IO (Response Value)
placeOrder opts order =
  asValue =<< postWith opts ordersEndpoint (encode order)

replaceOrder :: Options -> Order -> IO (Response Value)
replaceOrder opts order =
  asValue =<< patchWith opts ordersEndpoint (encode order)
  where
    patchWith = customPayloadMethodWith "PATCH"

cancelAllOrders :: Options -> IO (Response Value)
cancelAllOrders opts = asValue =<< deleteWith opts ordersEndpoint

cancelOrder :: Options -> String -> IO (Response Value)
cancelOrder opts orderId =
  asValue =<< deleteWith opts (ordersEndpoint <> "/" <> orderId)
