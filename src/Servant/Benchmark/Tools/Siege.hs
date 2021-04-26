{-# LANGUAGE OverloadedStrings #-}

{- |

Support for the [Siege](https://www.joedog.org/siege-home/) http load testing and benchmarking utility.
-}
module Servant.Benchmark.Tools.Siege (export, Settings (..)) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (parseMethod)
import Servant.API.Verbs
import Servant.Benchmark.Endpoint

-- | Siege settings.
newtype Settings = MkSettings
    {root :: T.Text}

serialize :: Settings -> Endpoint -> BS.ByteString
serialize (MkSettings rootPath) endpoint =
    case method endpoint of
        Nothing -> T.encodeUtf8 $ rootPath <> path endpoint
        Just actualMethod ->
            case parseMethod actualMethod of
                Right GET -> T.encodeUtf8 $ rootPath <> path endpoint
                Right POST -> T.encodeUtf8 (rootPath <> path endpoint <> " POST ") <> fromMaybe "" (body endpoint)
                _ -> BS.empty

{- | Export a URL files path.
  Note that since Siege-@.06 and later, only the POST and GET directives are supported.
  All other methods will not produce a url in the URLs file.
-}
export :: FilePath -> Settings -> [Endpoint] -> IO ()
export file settings endpoints = do
    let serialized = BS8.unlines $ filter (not . BS8.null) $ map (serialize settings . pack) endpoints
    BS.writeFile file serialized
