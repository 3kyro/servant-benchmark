{-# LANGUAGE OverloadedStrings #-}

module Servant.Benchmark.Internal.BasicAuth where

import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 as BS8
import Network.HTTP.Types (Header, hAuthorization)
import Servant (BasicAuthData (..))
import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen (generate)

-- Given a function from `a`  to `BasicAuthData`, produce an authorization header from a
-- random value of `a`
encodeBasicAuth :: (a -> BasicAuthData) -> Gen a -> IO Header
encodeBasicAuth f gen = do
    basicAuthData <- f <$> generate gen
    let bs64 = BS8.pack "Basic" <> encode (basicAuthUsername basicAuthData <> BS8.singleton ':' <> basicAuthPassword basicAuthData)
    pure (hAuthorization, bs64)
