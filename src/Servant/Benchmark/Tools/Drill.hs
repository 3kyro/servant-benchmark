{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Support for the [Drill](https://github.com/fcsonline/drill) load testing application
-}
module Servant.Benchmark.Tools.Drill (Settings (..), export) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair, Value)
import qualified Data.ByteString as BS
import Data.CaseInsensitive (original)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Yaml.Pretty as Y
import Network.HTTP.Types (Header)
import Servant.Benchmark.Endpoint
import Servant.Benchmark.ToText

-- | Drill specific settings. See the project's [ documentation ](https://github.com/fcsonline/drill) for more details
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
                [ "url" .= path endpoint
                , "method" .= fmap toText (method endpoint)
                , "body" .= fmap toText (body endpoint)
                , "headers"
                    .= object
                        (headerToValue <$> headers endpoint)
                ]
        ]

headerToValue :: Header -> Pair
headerToValue (headerName, value) =
    fromText (toText (original headerName)) .= toText value

-- | Export a benchmark file given a list of `Endpoint`s
export :: FilePath -> Settings -> [Endpoint] -> IO ()
export filepath settings endpoints = do
    let output = MkOutput settings $ pack <$> endpoints
    let encoding = Y.encodePretty config output
    BS.writeFile filepath encoding

config :: Y.Config
config =
    Y.setConfCompare ordering Y.defConfig

-- Explicit ordering for root Yaml fields
ordering :: T.Text -> T.Text -> Ordering
ordering "plan" _ = GT
ordering _ "plan" = LT
ordering t1 t2 = comparing T.length t1 t2
