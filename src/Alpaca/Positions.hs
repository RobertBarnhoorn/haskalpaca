module Alpaca.Positions
  ( Position,
    getPosition,
    getAllPositions,
    liquidate,
    liquidateEverything,
    exitMarket,
  )
where

import Alpaca.Endpoints (positionsEndpoint, (/.))
import Alpaca.Orders hiding (Side)
import Alpaca.Query (query)
import Control.Lens ((&), (.~), (<&>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, withObject, (.:), (.:?))
import Data.ByteString.Lazy (ByteString)
import Data.Char (toUpper)
import Data.Text (pack)
import Network.Wreq (Options, Response, deleteWith, getWith, params)

data Side = LONG | SHORT
  deriving (Read, Show)

data Position = Position
  { assetId :: String,
    symbol :: String,
    exchange :: String,
    assetClass :: String,
    avgEntryPrice :: Double,
    qty :: Int,
    side :: Side,
    marketValue :: Double, -- total dollar amount of the position
    costBasis :: Double, -- total dollar cost basis
    unrealizedPl :: Double, -- unrealized dollar P/L
    unrealizedPlPct :: Double, -- unrealized P/L percent
    unrealizedIntradayPl :: Double, -- unrealized dollar P/L for the day
    unrealizedIntradayPlPct :: Double, -- unrealized P/L percent for the day
    currentPrice :: Double,
    lastdayPrice :: Double, -- closing value of the last trading day
    changeToday :: Double -- percent change from last day price
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

newtype ExitMarketResult = ExitMarketResult Order
  deriving (Read, Show)

instance FromJSON ExitMarketResult where
  parseJSON = withObject "ExitMarketResult" $ \o ->
    o .: "body" >>= \b -> fmap ExitMarketResult . parseJSON $ b

-- | Run the HTTP query against our open position for the given symbol
queryPosition ::
  FromJSON a =>
  String ->
  (String -> IO (Response ByteString)) ->
  IO (Either String a)
queryPosition symbol = query $ positionsEndpoint /. symbol

-- | Run the HTTP query against all of our open positions for the given symbol
queryPositions ::
  FromJSON a => (String -> IO (Response ByteString)) -> IO (Either String a)
queryPositions = query positionsEndpoint

-- | Get our open position for the given symbol
getPosition :: String -> Options -> IO (Either String Position)
getPosition symbol opts = queryPosition symbol (getWith opts)

-- | Get all of our open positions
getAllPositions :: Options -> IO (Either String [Position])
getAllPositions opts = queryPositions $ getWith opts

-- | Liquidate a number of shares in our position for the given symbol. Places
-- and returns the liquidation order. Works for both short and long positions
liquidate :: String -> Int -> Options -> IO (Either String Order)
liquidate symbol qty opts = queryPosition symbol (deleteWith opts')
  where
    opts' = opts & params .~ [("qty", pack . show $ qty)]

-- | Attempt to entirely liquidate all of our open positions
liquidateEverything :: Options -> IO (Either String [LiquidationResult])
liquidateEverything opts = queryPositions $ deleteWith opts

-- | Attempt to cancel all open orders and then entirely liquidate all of our
-- open positions
exitMarket :: Options -> IO (Either String [ExitMarketResult])
exitMarket opts = queryPositions $ deleteWith opts'
  where
    opts' = opts & params .~ [("cancel_orders", pack . show $ True)]
