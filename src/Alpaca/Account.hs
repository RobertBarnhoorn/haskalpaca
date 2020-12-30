module Alpaca.Account (Account (..), AccountStatus, getAccount) where

import Alpaca.Endpoints (accountEndpoint)
import Alpaca.Query (query)
import Control.Lens ((<&>))
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.ByteString.Lazy (ByteString)
import Network.Wreq (Options, Response, getWith)

data AccountStatus
  = ONBOARDING
  | SUBMISSION_FAILED
  | SUBMITTED
  | ACCOUNT_UPDATED
  | APPROVAL_PENDING
  | ACTIVE
  deriving (Read, Show)

data Account = Account
  { id :: String,
    accountNumber :: String,
    status :: AccountStatus,
    currency :: String,
    cash :: Double, -- account balance
    patternDayTrader :: Bool,
    tradeSuspendedByUser :: Bool,
    tradingBlocked :: Bool,
    transfersBlocked :: Bool,
    accountBlocked :: Bool,
    createdAt :: String,
    shortingEnabled :: Bool,
    longMarketValue :: Double, -- real-time MtM value of all long positions
    shortMarketValue :: Double, -- real-time MtM value of all short positions
    equity :: Double, -- cash + long market value + short market value
    lastEquity :: Double, -- equity as of previous trading day at 16:00:00 ET
    multiplier :: Int, -- buying power multiplier a.k.a margin classification
    buyingPower :: Double, -- currently available dollar buying power
    initialMargin :: Double, -- Reg T initial margin requirement
    maintenanceMargin :: Double, -- maintenance margin requirement
    sma :: Double, -- value of special memorandum account
    daytradeCount :: Int, -- number of daytrades made in the last 5 trading days
    lastMaintenanceMargin :: Double, -- last day maintenance margin requirement
    daytradingBuyingPower :: Double, -- buying power for day trades
    regtBuyingPower :: Double -- buying power under Regulation T
  }
  deriving (Read, Show)

instance FromJSON Account where
  parseJSON = withObject "Account" $ \o ->
    Account
      <$> o .: "id"
      <*> o .: "account_number"
      <*> (o .: "status" <&> read)
      <*> o .: "currency"
      <*> (o .: "cash" <&> read)
      <*> o .: "pattern_day_trader"
      <*> o .: "trade_suspended_by_user"
      <*> o .: "trading_blocked"
      <*> o .: "transfers_blocked"
      <*> o .: "account_blocked"
      <*> o .: "created_at"
      <*> o .: "shorting_enabled"
      <*> (o .: "long_market_value" <&> read)
      <*> (o .: "short_market_value" <&> read)
      <*> (o .: "equity" <&> read)
      <*> (o .: "last_equity" <&> read)
      <*> (o .: "multiplier" <&> read)
      <*> (o .: "buying_power" <&> read)
      <*> (o .: "initial_margin" <&> read)
      <*> (o .: "maintenance_margin" <&> read)
      <*> (o .: "sma" <&> read)
      <*> o .: "daytrade_count"
      <*> (o .: "last_maintenance_margin" <&> read)
      <*> (o .: "daytrading_buying_power" <&> read)
      <*> (o .: "regt_buying_power" <&> read)

-- | Run the HTTP query against our open position for the given symbol
queryAccount ::
  FromJSON a => (String -> IO (Response ByteString)) -> IO (Either String a)
queryAccount = query accountEndpoint

-- | Get information about our account
getAccount :: Options -> IO (Either String Account)
getAccount opts = queryAccount $ getWith opts
