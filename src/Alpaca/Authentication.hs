module Alpaca.Authentication where

import Control.Lens ((&), (.~))
import qualified Data.ByteString.UTF8 as BSU (fromString)
import Network.Wreq (Options, defaults, header)
import System.Environment (lookupEnv)

-- | Represents the keys that need to be included with all HTTP query requests
-- in order for Alpaca to authenticate them
data AuthConfig = AuthConfig
  { keyId :: String,
    secretKey :: String
  }
  deriving (Read)

-- | Fetch authentication keys from the system environment and create
-- authenticated HTTP 'Options'
getAuth :: IO Options
getAuth =
  getAuthKeys >>= \case
    Just c -> return $ makeAuthOptions c
    Nothing -> error "Problem retrieving auth keys. Quitting."

-- | Creates HTTP Options with the given 'AuthConfig' injected as headers
makeAuthOptions :: AuthConfig -> Options
makeAuthOptions (AuthConfig k s) =
  defaults
    & (header "APCA-API-KEY-ID" .~ [BSU.fromString k])
    & (header "APCA-API-SECRET-KEY" .~ [BSU.fromString s])
    & (header "User-Agent" .~ [BSU.fromString "haskalpaca"])

-- | Returns the ALPACA_KEY_ID and ALPACA_SECRET_KEY environment variables, or
-- Nothing if either is not defined
getAuthKeys :: IO (Maybe AuthConfig)
getAuthKeys = do
  keyId <- lookupEnv "ALPACA_KEY_ID"
  secretKey <- lookupEnv "ALPACA_SECRET_KEY"
  return $ case (keyId, secretKey) of
    (Just k, Just s) -> Just (AuthConfig k s)
    _ -> Nothing
