module Alpaca.Positions
  ( Position,
    getPosition,
    getAllPositions,
    closePosition,
    closeAllPositions,
  )
where

import Alpaca.Endpoints (positionsEndpoint, (/.))
import Alpaca.Query (query)
import Control.Lens ((<&>))
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.ByteString.Lazy (ByteString)
import Data.Char (toUpper)
import Network.Wreq (Options, Response, deleteWith, getWith)

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

-- | Liquidate our open position for the given symbol
-- TODO send query params
-- TODO wrong decode type
closePosition :: String -> Options -> IO (Either String Position)
closePosition symbol opts = queryPosition symbol (deleteWith opts)

-- | Liquidate all of our open positions
-- TODO send query params
-- TODO wrong decode type
closeAllPositions :: Options -> IO (Either String [Position])
closeAllPositions opts = queryPositions $ deleteWith opts
