{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Servant.Benchmark.Drill (Settings (..), encode) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson.Types (Pair, Value)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (original)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Y
import Network.HTTP.Types (Header)
import Servant.Benchmark.Endpoint

data Settings = MkSettings
    { concurrency :: Word
    , base :: T.Text
    , iterations :: Word
    , rampup :: Word
    }

data Output = MkOutput Settings [Endpoint]

instance ToJSON Output where
    toJSON (MkOutput settings plan) =
        object
            [ "concurrency" .= concurrency settings
            , "base" .= base settings
            , "iterations" .= iterations settings
            , "rampup" .= rampup settings
            , "plan"
                .= requests plan
            ]

requests :: [Endpoint] -> [Value]
requests endpoints = endpointToJSON <$> endpoints

endpointToJSON :: Endpoint -> Value
endpointToJSON endpoint =
    object
        [ "name" .= name endpoint
        , "request"
            .= object
                [ "url" .= T.concat (path endpoint)
                , "method" .= fmap T.decodeUtf8 (method endpoint)
                , "body" .= fmap T.decodeUtf8 (body endpoint)
                , "headers" .= object (headerToValue <$> headers endpoint)
                ]
        ]

headerToValue :: Header -> Pair
headerToValue (headerName, value) =
    T.decodeUtf8 (original headerName) .= show value

encode :: Settings -> [Endpoint] -> ByteString
encode sett endpoints =
    Y.encode $ MkOutput sett endpoints
