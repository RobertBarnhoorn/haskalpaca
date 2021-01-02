module Alpaca.Orders where

import Alpaca.Endpoints (ordersEndpoint, (/.))
import Alpaca.Query (patchWith', postWith', query)
import Control.Lens ((<&>))
import Data.Aeson
  ( FromJSON,
    ToJSON,
    encode,
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower, toUpper)
import Network.Wreq (Options, Response, deleteWith, getWith)

data OrderType = MARKET | LIMIT | STOP | STOP_LIMIT | TRAILING_STOP
  deriving (Read, Show)

data Side = BUY | SELL
  deriving (Read, Show)

data OrderClass = SIMPLE | BRACKET | OCO | OTO
  deriving (Read, Show)

data TimeInForce = DAY | GTC | OPG | CLS | IOC | FOK
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

data OrderRequest = OrderRequest
  { _type :: OrderType,
    symbol :: String, -- symbol or id to identify the asset
    quantity :: Int,
    side :: Side,
    timeInForce :: TimeInForce,
    orderClass :: OrderClass,
    orderId :: Maybe String, -- id for tracking; auto-generated if not sent
    advanced :: Maybe AdvancedParams
  }
  deriving (Read, Show)

instance ToJSON OrderRequest where
  toJSON
    OrderRequest
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
        [ "type" .= map toLower (show _type),
          "symbol" .= sym,
          "qty" .= show qty,
          "side" .= map toLower (show side),
          "time_in_force" .= map toLower (show tif),
          "order_class" .= map toLower (show cls)
        ]

-- | The lifecycle of an order consists of transitions through these states
-- An order may be canceled through the API up until the point it reaches a state of either filled, canceled, or expired
-- "Most users will likely never see their orders reach these states": ACCEPTED,
-- PENDING_NEW, ACCEPTED_FOR_BIDDING, STOPPED, REJECTED, SUSPENDED, CALCULATED
data Status
  = NEW
  | PARTIALLY_FILLED
  | FILLED
  | DONE_FOR_DAY
  | CANCELED
  | EXPIRED
  | REPLACED
  | PENDING_CANCEL
  | PENDING_REPLACE
  | ACCEPTED
  | PENDING_NEW
  | ACCEPTED_FOR_BIDDING
  | STOPPED
  | REJECTED
  | SUSPENDED
  | CALCULATED
  deriving (Read, Show)

data Order = Order
  { id :: String,
    clientOrderId :: String, -- same as id if not specified at order request
    createdAt :: String,
    updatedAt :: Maybe String,
    submittedAt :: Maybe String,
    filledAt :: Maybe String,
    expiredAt :: Maybe String,
    canceledAt :: Maybe String,
    failedAt :: Maybe String,
    replacedAt :: Maybe String,
    replacedBy :: Maybe String, -- id of the replacement order
    replaces :: Maybe String, -- id of order which was replaced
    asset_id :: String,
    symbol :: String,
    asset_class :: String,
    qty :: Int,
    filled_qty :: Int,
    _type :: OrderType,
    side :: Side,
    timeInForce :: TimeInForce,
    limitPrice :: Maybe Double,
    stopPrice :: Maybe Double,
    filledAvgPrice :: Maybe Double,
    status :: Status,
    extendedHours :: Bool, -- eligibility for execution outside regular hours
    legs :: Maybe [Order], -- when querying nested non-simple order class orders
    -- the below 3 parameters only  only apply for trailing-stop order types
    trailPrice :: Maybe Double, -- dollars away from high water mark
    trailPercent :: Maybe Double, -- percent away from the high water mark
    hwm :: Maybe Double -- highest/lowest market price seen since order made
  }
  deriving (Read, Show)

instance FromJSON Order where
  parseJSON = withObject "Order" $ \o ->
    Order
      <$> o .: "id"
      <*> o .: "client_order_id"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "submitted_at"
      <*> o .: "filled_at"
      <*> o .: "expired_at"
      <*> o .: "canceled_at"
      <*> o .: "failed_at"
      <*> o .: "replaced_at"
      <*> o .: "replaced_by"
      <*> o .: "replaces"
      <*> o .: "asset_id"
      <*> o .: "symbol"
      <*> o .: "asset_class"
      <*> (o .: "qty" <&> read)
      <*> (o .: "filled_qty" <&> read)
      <*> (o .: "type" <&> read . map toUpper)
      <*> (o .: "side" <&> read . map toUpper)
      <*> (o .: "time_in_force" <&> read . map toUpper)
      <*> (o .:? "limit_price" <&> (<&> read))
      <*> (o .:? "stop_price")
      <*> (o .:? "filled_avg_price")
      <*> (o .: "status" <&> read . map toUpper)
      <*> o .: "extended_hours"
      <*> o .: "legs"
      <*> (o .:? "trail_price")
      <*> (o .:? "trail_percent")
      <*> (o .:? "hwm")

-- | Run the HTTP query against the given order
queryOrder ::
  FromJSON a =>
  String ->
  (String -> IO (Response ByteString)) ->
  IO (Either String a)
queryOrder orderId = query $ ordersEndpoint /. orderId

-- | Run the HTTP query against the general orders endpoint
queryOrders ::
  FromJSON a => (String -> IO (Response ByteString)) -> IO (Either String a)
queryOrders = query ordersEndpoint

-- | Get an order by Id
getOrder :: String -> Options -> IO (Either String Order)
getOrder orderId opts = queryOrder orderId (getWith opts)

-- | Get all orders
getOrders :: Options -> IO (Either String [Order])
getOrders opts = queryOrders $ getWith opts

-- | Place an order
placeOrder :: OrderRequest -> Options -> IO (Either String Order)
placeOrder order opts = do
  print =<< postWith' opts (encode order) ordersEndpoint
  queryOrders $ postWith' opts (encode order)

-- | Replaces an existing order, updating parameters to the new values
replaceOrder :: OrderRequest -> Options -> IO (Either String Order)
replaceOrder order opts = queryOrders $ patchWith' opts (encode order)

-- | Cancel an order by Id
cancelOrder :: String -> Options -> IO (Either String Order)
cancelOrder orderId opts = queryOrder orderId (deleteWith opts)

-- | Cancel all orders
cancelAllOrders :: Options -> IO (Either String [Order])
cancelAllOrders opts = queryOrders (deleteWith opts)
