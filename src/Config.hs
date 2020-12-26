module Config where

import System.Environment (lookupEnv)

data AuthConfig = AuthConfig
  { keyId :: String,
    secretKey :: String
  }
  deriving (Read, Show)

getAuthKeys :: IO (Maybe AuthConfig)
getAuthKeys = do
  keyId <- lookupEnv "ALPACA_KEY_ID"
  secretKey <- lookupEnv "ALPACA_SECRET_KEY"
  return $ case (keyId, secretKey) of
    (Just k, Just s) -> Just (AuthConfig k s)
    (_, _) -> Nothing
