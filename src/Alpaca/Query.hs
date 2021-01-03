module Alpaca.Query where

import Control.Exception (SomeException, try)
import Control.Lens (view)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.Wreq
  ( Options,
    Response,
    customPayloadMethodWith,
    postWith,
    responseBody,
  )
import Network.Wreq.Types (Postable)

-- | Run the HTTP method against the endpoint and decode the ByteString response
-- body into the relevant type. HTTP and decoding errors are reported with Left
query ::
  FromJSON a =>
  String ->
  (String -> IO (Response ByteString)) ->
  IO (Either String a)
query endpoint f =
  sendRequest >>= \case
    Left e -> return $ Left (show e)
    Right r -> return $ eitherDecode . view responseBody $ r
  where
    sendRequest :: IO (Either SomeException (Response ByteString))
    sendRequest = try $ f endpoint

-- | Re-order the arguments to work with query
postWith' :: (Postable a) => Options -> a -> String -> IO (Response ByteString)
postWith' opts = flip $ postWith opts

-- | HTTP PATCH. Works like other Wreq functions
patchWith' :: (Postable a) => Options -> a -> String -> IO (Response ByteString)
patchWith' opts postable endpoint =
  customPayloadMethodWith "PATCH" opts endpoint postable
