module Alpaca.Account where

import Alpaca.Endpoints (accountEndpoint, (/.))
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
  { accountId :: String,
    accountNumber :: String,
    status :: AccountStatus,
    currency :: String,
    cash :: Double,
    patternDayTrader :: Bool,
    tradeSuspendedByUser :: Bool,
    tradingBlocked :: Bool,
    transfersBlocked :: Bool,
    accountBlocked :: Bool,
    createdAt :: String,
    shortingEnabled :: Bool,
    -- | Real-time MtM value of all long positions
    longMarketValue :: Double,
    -- | Real-time MtM value of all short positions
    shortMarketValue :: Double,
    -- | Cash + long market value + short market value
    equity :: Double,
    -- | Equity as of previous trading day at 16:00:00 ET
    lastEquity :: Double,
    -- | Buying power multiplier a.k.a margin classification
    multiplier :: Int,
    -- | Currently available dollar buying power
    buyingPower :: Double,
    -- | Regulation T initial margin requirement
    initialMargin :: Double,
    -- | Maintenance margin requirement
    maintenanceMargin :: Double,
    -- | Value of special memorandum account
    sma :: Double,
    -- | Number of daytrades made in the last 5 trading days
    daytradeCount :: Int,
    -- | Last day maintenance margin requirement
    lastMaintenanceMargin :: Double,
    -- | Buying power for day trades
    daytradingBuyingPower :: Double,
    -- | Buying power under Regulation T
    regtBuyingPower :: Double
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

-- | Run the HTTP query against the account
queryAccount ::
  FromJSON a => (String -> IO (Response ByteString)) -> IO (Either String a)
queryAccount = query accountEndpoint

-- | Get information about the account
getAccount :: Options -> IO (Either String Account)
getAccount opts = queryAccount $ getWith opts
