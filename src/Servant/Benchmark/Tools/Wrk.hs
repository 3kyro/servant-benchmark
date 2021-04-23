{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
This module provides support for the [wrk](https://github.com/wg/wrk) benchmarking tool.

Given a Servant API and a list of `Endpoint`s, the `export` function can produce a requests file containing
a JSON representation of the provided `Endpoint`s.

In order to provide wrk the request data, you can use a simple lua script as described in [this](http://czerasz.com/2015/07/19/wrk-http-benchmarking-tool-example/) tutorial.

An adapted version of the original script by Michael Czeraszkiewicz can be found in the project's [ repository ](https://github.com/3kyro/servant-benchmark/tree/main/scripts)
-}
module Servant.Benchmark.Tools.Wrk (export) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.CaseInsensitive (original)
import Network.HTTP.Types (Header)
import Servant.Benchmark (Endpoint (..))
import Servant.Benchmark.Endpoint (pack)
import Servant.Benchmark.ToText

newtype Output = MkOutput Endpoint

instance ToJSON Output where
    toJSON (MkOutput endpoint) =
        object
            [ "path" .= path endpoint
            , "body" .= fmap toText (body endpoint)
            , "method" .= fmap toText (method endpoint)
            , "headers"
                .= object
                    (headerToValue <$> headers endpoint)
            ]

    toEncoding (MkOutput endpoint) =
        pairs $
            "path" .= path endpoint
                <> "body" .= fmap toText (body endpoint)
                <> "method" .= fmap toText (method endpoint)
                <> "headers"
                    .= object
                        (headerToValue <$> headers endpoint)

headerToValue :: Header -> Pair
headerToValue (headerName, value) =
    toText (original headerName) .= toText value

-- | Export a requests file given a list of `Endpoint`s
export :: FilePath -> [Endpoint] -> IO ()
export filepath endpoints = do
    let encoding = encode $ MkOutput . pack <$> endpoints
    BS.writeFile filepath $ BSL.toStrict encoding
