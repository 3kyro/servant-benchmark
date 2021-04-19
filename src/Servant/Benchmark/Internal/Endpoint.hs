{-# LANGUAGE OverloadedStrings #-}

module Servant.Benchmark.Internal.Endpoint where

import Control.Applicative ((<|>))
import Data.Aeson (Value)
import Data.ByteString.UTF8 (fromString)
import Data.CaseInsensitive (mk)
import qualified Data.Text as T
import Network.HTTP.Types (Method)
import Network.HTTP.Types.Header (Header)

{- | An API endpoint.
-
-}
data Endpoint = MkEndpoint
    { -- All endpoint request paths
      path :: [T.Text]
    , -- The endpoint request method
      method :: Maybe Method
    , -- | The request value, where applicable.
      -- Only the first encountered request value is taken into consideration
      -- eg. "user" :> ReqBody '[JSON] Text :> ReqBody '[JSON] Int :> Get '[JSON] User
      -- will produce only a `Text` based request value
      body :: Maybe Value
    , headers :: [Header]
    }
    deriving (Show, Eq)

instance Semigroup Endpoint where
    a <> b =
        MkEndpoint
            (path a <> path b)
            (method a <> method b)
            -- left biased alternative for request value
            (body a <|> body b)
            (headers a <> headers b)

instance Monoid Endpoint where
    mempty = MkEndpoint mempty mempty Nothing mempty

-- Create a `Header` from a string and a value
mkHeader :: Show a => String -> a -> Header
mkHeader name value =
    (mk $ fromString name, fromString $ show value)
