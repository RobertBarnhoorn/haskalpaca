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
  { -- | Required for bracket orders
    takeProfitlimitPrice :: Double
  }
  deriving (Read, Show)

data StopLoss = StopLoss
  { -- | Required for bracket orders
    stopLossStopPrice :: Double,
    -- | Turns a a stop-loss order into a stop-limit order
    stopLosslimitPrice :: Double
  }
  deriving (Read, Show)

data AdvancedParams
  = LimitParams
      { -- | Required if order type is limit or stop limit
        limitPrice :: Maybe Double,
        -- | Required  if order type is stop or stop limit
        stopPrice :: Maybe Double,
        -- | Only applicable to limit orders where time in force = DAY
        extendedHours :: Maybe Bool
      }
  | TrailingStopParams
      { -- | Price or percent required for trailing stop orders
        trailPrice :: Maybe Double,
        -- | Price or percent required for trailing stop orders
        trailPercent :: Maybe Double,
        takeProfit :: Maybe TakeProfit,
        stopLoss :: Maybe StopLoss
      }
  deriving (Read, Show)

data OrderRequest = OrderRequest
  { orderType :: OrderType,
    symbol :: String,
    quantity :: Int,
    side :: Side,
    timeInForce :: TimeInForce,
    orderClass :: OrderClass,
    -- | Id for tracking by client; auto-generated if not sent
    trackingId :: Maybe String,
    advanced :: Maybe AdvancedParams
  }
  deriving (Read, Show)

instance ToJSON OrderRequest where
  toJSON
    OrderRequest
      { orderType = orderType,
        symbol = sym,
        quantity = qty,
        side = side,
        timeInForce = tif,
        orderClass = cls,
        trackingId = _, -- TODO custom order Id
        advanced = _ -- TODO serialize advanced orders
      } =
      object
        [ "type" .= map toLower (show orderType),
          "symbol" .= sym,
          "qty" .= show qty,
          "side" .= map toLower (show side),
          "time_in_force" .= map toLower (show tif),
          "order_class" .= map toLower (show cls)
        ]

-- | An order may be canceled through the API up until the point it reaches a
-- state of either filled, canceled, or expired
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
  { orderId :: String,
    clientOrderId :: String,
    createdAt :: String,
    updatedAt :: Maybe String,
    submittedAt :: Maybe String,
    filledAt :: Maybe String,
    expiredAt :: Maybe String,
    canceledAt :: Maybe String,
    failedAt :: Maybe String,
    replacedAt :: Maybe String,
    -- | Id of the replacement order
    replacedBy :: Maybe String,
    -- | Id of order which was replaced
    replaces :: Maybe String,
    assetId :: String,
    symbol :: String,
    asset_class :: String,
    qty :: Int,
    filled_qty :: Int,
    orderType :: OrderType,
    side :: Side,
    timeInForce :: TimeInForce,
    limitPrice :: Maybe Double,
    stopPrice :: Maybe Double,
    filledAvgPrice :: Maybe Double,
    status :: Status,
    -- | Eligibility for execution outside regular hours
    extendedHours :: Bool,
    -- | All relevant orders when querying nested non-simple order class orders
    legs :: Maybe [Order],
    -- | Only trailing-stop orders: dollars away from high water mark
    trailPrice :: Maybe Double,
    -- | Only trailing-stop orders: percent away from the high water mark
    trailPercent :: Maybe Double,
    -- | Only trailing-stop orders: max/min market price seen since order made
    hwm :: Maybe Double
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
  queryOrders $ postWith' opts (encode order)

-- | Replaces an existing order, updating parameters to the new values
replaceOrder :: OrderRequest -> Options -> IO (Either String Order)
replaceOrder order opts = queryOrders $ patchWith' opts (encode order)

-- | Cancel an order by Id
cancelOrder :: String -> Options -> IO (Either String Order)
cancelOrder orderId opts = queryOrder orderId (deleteWith opts)

-- | Cancel all orders
cancelOrders :: Options -> IO (Either String [Order])
cancelOrders opts = queryOrders (deleteWith opts)
