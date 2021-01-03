module Alpaca.Positions where

import Alpaca.Endpoints (positionsEndpoint, (/.))
import Alpaca.Orders (Order)
import Alpaca.Query (query)
import Control.Lens ((&), (.~), (<&>))
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.ByteString.Lazy (ByteString)
import Data.Char (toUpper)
import Data.Text (pack)
import Network.Wreq (Options, Response, deleteWith, getWith, params)

data Side = LONG | SHORT
  deriving (Read, Show)

-- | Represents the position for a specific 'symbol'
data Position = Position
  { assetId :: String,
    symbol :: String,
    exchange :: String,
    assetClass :: String,
    avgEntryPrice :: Double,
    qty :: Int,
    side :: Side,
    -- | Total dollar amount of the position
    marketValue :: Double,
    -- | Total dollar cost basis
    costBasis :: Double,
    -- | Unrealized dollar P/L
    unrealizedPl :: Double,
    -- | Unrealized P/L percent
    unrealizedPlPct :: Double,
    -- | Unrealized dollar P/L for the day
    unrealizedIntradayPl :: Double,
    -- | Unrealized P/L percent for the day
    unrealizedIntradayPlPct :: Double,
    currentPrice :: Double,
    -- | Closing value of the last trading day
    lastdayPrice :: Double,
    -- | Percent change from last day price
    changeToday :: Double
  }
  deriving (Read, Show)

instance FromJSON Position where
  parseJSON = withObject "Position" $ \o ->
    Position
      <$> o .: "asset_id"
      <*> o .: "symbol"
      <*> o .: "exchange"
      <*> o .: "asset_class"
      <*> (o .: "avg_entry_price" <&> read)
      <*> (o .: "qty" <&> read)
      <*> (o .: "side" <&> (read . map toUpper))
      <*> (o .: "market_value" <&> read)
      <*> (o .: "cost_basis" <&> read)
      <*> (o .: "unrealized_pl" <&> read)
      <*> (o .: "unrealized_plpc" <&> read)
      <*> (o .: "unrealized_intraday_pl" <&> read)
      <*> (o .: "unrealized_intraday_plpc" <&> read)
      <*> (o .: "current_price" <&> read)
      <*> (o .: "lastday_price" <&> read)
      <*> (o .: "change_today" <&> read)

-- | Represents the success or failure of an attempted position liquidation,
-- along with relevant diagnostics
data LiquidationResult = LiquidationResult
  { symbol :: String,
    success :: Bool,
    available :: Integer,
    existingQty :: Integer,
    heldForOrders :: Integer,
    relatedOrders :: [String],
    message :: String
  }
  deriving (Read, Show)

instance FromJSON LiquidationResult where
  parseJSON = withObject "LiquidationResult" $ \o ->
    LiquidationResult
      <$> o .: "symbol"
      <*> ( o .: "status" <&> \x -> case (x :: Int) of
              200 -> True -- HTTP 200 OK
              _ -> False
          )
      <*> ((o .: "body") >>= (.: "available") <&> read)
      <*> ((o .: "body") >>= (.: "existing_qty") <&> read)
      <*> ((o .: "body") >>= (.: "held_for_orders") <&> read)
      <*> ((o .: "body") >>= (.: "related_orders"))
      <*> ((o .: "body") >>= (.: "message"))

-- | Represents an 'Order' which is helping to exit the market. One of:
--     * An 'Order' placed specifically to liquidate a position
--     * A previously open 'Order' which has been cancelled
newtype ExitMarketResult = ExitMarketResult Order
  deriving (Read, Show)

instance FromJSON ExitMarketResult where
  parseJSON = withObject "ExitMarketResult" $ \o ->
    o .: "body" >>= fmap ExitMarketResult . parseJSON

-- | Run the HTTP query against the open position of the given symbol
queryPosition ::
  FromJSON a =>
  String ->
  (String -> IO (Response ByteString)) ->
  IO (Either String a)
queryPosition symbol = query $ positionsEndpoint /. symbol

-- | Run the HTTP query against all open positions
queryPositions ::
  FromJSON a => (String -> IO (Response ByteString)) -> IO (Either String a)
queryPositions = query positionsEndpoint

-- | Get the open position for the given symbol
getPosition :: String -> Options -> IO (Either String Position)
getPosition symbol opts = queryPosition symbol (getWith opts)

-- | Get all open positions
getPositions :: Options -> IO (Either String [Position])
getPositions opts = queryPositions $ getWith opts

-- | Liquidate a number of shares for the given symbol. Places and returns the
-- liquidation order. Works for both short and long positions
liquidate :: String -> Int -> Options -> IO (Either String Order)
liquidate symbol qty opts = queryPosition symbol (deleteWith opts')
  where
    opts' = opts & params .~ [("qty", pack . show $ qty)]

-- | Nuclear option. Attempt to entirely liquidate all open positions. Does not
-- affect open orders
liquidateEverything :: Options -> IO (Either String [LiquidationResult])
liquidateEverything opts = queryPositions $ deleteWith opts

-- | Nuclear option. Attempt to cancel all open orders and then entirely
-- liquidate all open positions
exitMarket :: Options -> IO (Either String [ExitMarketResult])
exitMarket opts = queryPositions $ deleteWith opts'
  where
    opts' = opts & params .~ [("cancel_orders", pack . show $ True)]
