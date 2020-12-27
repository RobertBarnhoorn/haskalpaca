module Client.Account where

import Client.Endpoints (accountEndpoint)
import Control.Lens (view, (<&>))
import Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject, (.:))
import Data.ByteString.Lazy ()
import Network.Wreq (Options, getWith, responseBody)

data Account = Account
  { id :: String,
    accountNumber :: String,
    status :: String,
    currency :: String,
    cash :: Double, -- account balance
    patternDayTrader :: Bool,
    tradeSuspendedByUser :: Bool,
    tradingBlocked :: Bool,
    transfersBlocked :: Bool,
    accountBlocked :: Bool,
    createdAt :: String,
    shortingEnabled :: Bool,
    longMarketValue :: Double, -- real-time MtM value of all long positions held in the account
    shortMarketValue :: Double, -- real-time MtM value of all short positions held in the account
    equity :: Double, -- cash + long market value + short market value
    lastEquity :: Double, -- equity as of previous trading day at 16:00:00 ET
    multiplier :: Int, -- buying power multiplier that represents account margin classification
    buyingPower :: Double, -- Current available dollar buying power
    initialMargin :: Double, -- Reg T initial margin requirement
    maintenanceMargin :: Double, -- Maintenance margin requirement
    sma :: Double, -- Value of special memorandum account
    daytradeCount :: Int, -- The current number of daytrades that have been made in the last 5 trading days
    lastMaintenanceMargin :: Double, -- Your maintenance margin requirement on the previous trading da
    daytradingBuyingPower :: Double, -- Your buying power for day trades
    regtBuyingPower :: Double -- Your buying power under Regulation T
  }
  deriving (Read, Show)

instance FromJSON Account where
  parseJSON = withObject "Account" $ \x ->
    Account
      <$> x .: "id"
      <*> x .: "account_number"
      <*> x .: "status"
      <*> x .: "currency"
      <*> (x .: "cash" <&> read)
      <*> x .: "pattern_day_trader"
      <*> x .: "trade_suspended_by_user"
      <*> x .: "trading_blocked"
      <*> x .: "transfers_blocked"
      <*> x .: "account_blocked"
      <*> x .: "created_at"
      <*> x .: "shorting_enabled"
      <*> (x .: "long_market_value" <&> read)
      <*> (x .: "short_market_value" <&> read)
      <*> (x .: "equity" <&> read)
      <*> (x .: "last_equity" <&> read)
      <*> (x .: "multiplier" <&> read)
      <*> (x .: "buying_power" <&> read)
      <*> (x .: "initial_margin" <&> read)
      <*> (x .: "maintenance_margin" <&> read)
      <*> (x .: "sma" <&> read)
      <*> x .: "daytrade_count"
      <*> (x .: "last_maintenance_margin" <&> read)
      <*> (x .: "daytrading_buying_power" <&> read)
      <*> (x .: "regt_buying_power" <&> read)

queryAccount :: Options -> IO (Either String Account)
queryAccount auth = eitherDecode . view responseBody <$> getWith auth accountEndpoint
