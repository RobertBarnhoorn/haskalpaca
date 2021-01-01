module Alpaca.Positions
  ( Position,
    getPosition,
    getAllPositions,
    liquidate,
    liquidateEverything,
  )
where

import Alpaca.Endpoints (positionsEndpoint, (/.))
import Alpaca.Orders hiding (Side)
import Alpaca.Query (query)
import Control.Lens ((&), (.~), (<&>))
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
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
-- and returns the liquidation order. Works for both short and long positions.
liquidate :: String -> Int -> Options -> IO (Either String Order)
liquidate symbol qty opts = queryPosition symbol (deleteWith opts')
  where
    opts' = opts & params .~ [("qty", pack $ show qty)]

-- | Entirely liquidate all of our open positions
-- TODO send query params
-- TODO wrong decode type
liquidateEverything :: Options -> IO (Either String [Position])
liquidateEverything opts = queryPositions $ deleteWith opts
