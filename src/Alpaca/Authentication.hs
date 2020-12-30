module Alpaca.Authentication (authenticate, authOptions) where

import Control.Lens ((&), (.~))
import qualified Data.ByteString.UTF8 as BSU (fromString)
import Network.Wreq (Options, defaults, header)
import System.Environment (lookupEnv)

data AuthConfig = AuthConfig
  { keyId :: String,
    secretKey :: String
  }
  deriving (Read, Show)

-- | Fetch authentication keys from the system and create authentic HTTP Options
authenticate :: IO Options
authenticate =
  getAuthKeys >>= \case
    Just c -> return $ authOptions c
    Nothing -> error "---- Problem retrieving auth keys. Quitting."

-- | Creates HTTP Options which include Alpaca authentication headers
authOptions :: AuthConfig -> Options
authOptions (AuthConfig k s) =
  defaults
    & (header "APCA-API-KEY-ID" .~ [BSU.fromString k])
    & (header "APCA-API-SECRET-KEY" .~ [BSU.fromString s])

-- | Returns the ALPACA_KEY_ID and ALPACA_SECRET_KEY environment variables,
-- or Nothing if either is not defined
getAuthKeys :: IO (Maybe AuthConfig)
getAuthKeys = do
  keyId <- lookupEnv "ALPACA_KEY_ID"
  secretKey <- lookupEnv "ALPACA_SECRET_KEY"
  return $ case (keyId, secretKey) of
    (Just k, Just s) -> Just (AuthConfig k s)
    (_, _) -> Nothing
