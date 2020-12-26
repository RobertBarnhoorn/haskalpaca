module Client.Authentication where

import Config
import Control.Lens ((&), (.~))
import qualified Data.ByteString.UTF8 as BSU (fromString)
import Network.Wreq (Options, defaults, header)

authOptions :: AuthConfig -> Options
authOptions c =
  defaults
    & (header "APCA-API-KEY-ID" .~ [BSU.fromString $ keyId c])
    & (header "APCA-API-SECRET-KEY" .~ [BSU.fromString $ secretKey c])
