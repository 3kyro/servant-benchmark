{-# LANGUAGE OverloadedStrings #-}

module Servant.Benchmark.Endpoint where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.CaseInsensitive (mk)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Media (MediaType, RenderHeader (renderHeader), (//), (/:))
import Network.HTTP.Types (Method, hContentType)
import Network.HTTP.Types.Header (Header)

{- | An API endpoint.
-
-}
data Endpoint = MkEndpoint
    { name :: T.Text
    , -- All endpoint request paths
      path :: T.Text
    , -- The endpoint request method
      method :: Maybe Method
    , -- | The request value, where applicable.
      -- Only the first encountered request value is taken into consideration
      -- eg. "user" :> ReqBody '[JSON] Text :> ReqBody '[JSON] Int :> Get '[JSON] User
      -- will produce only a `Text` based request value
      body :: Maybe BS.ByteString
    , -- | The requests content type.
      -- Only the first encountered content type is taken into consideration.
      -- If you're building an endpoint manually, you should enter the media type here
      -- rather than directly in headers. All implementations automatically include the
      -- content type header during benchmark configuration output.
      contentType :: Maybe MediaType
    , -- | The request headers
      headers :: [Header]
    }
    deriving (Show, Eq)

instance Semigroup Endpoint where
    a <> b =
        MkEndpoint
            (name a <> name b)
            (path a <> path b)
            (method a <> method b)
            (body a <|> body b)
            (contentType a <|> contentType b)
            (headers a <> headers b)

instance Monoid Endpoint where
    mempty = MkEndpoint mempty mempty mempty Nothing Nothing []

{- | Pack an endpoint created from an API interpretation in a form
 ready to be serialized.
 - This is only useful if your are building your own output.
-}
pack :: Endpoint -> Endpoint
pack endpoint =
    endpoint
        { contentType = Nothing
        , headers = ct ++ headers endpoint
        }
  where
    ct = maybeToList ctHeader
    ctHeader = (,) hContentType <$> fmap renderHeader (contentType endpoint)

-- | Create a `Header` from two `Text` inputs
mkHeader :: T.Text -> T.Text -> Header
mkHeader ciName value =
    (mk $ T.encodeUtf8 ciName, T.encodeUtf8 value)

-- * Content Types

-- | application/json
ctJSON :: MediaType
ctJSON = hApplication // hJSON

-- | text/plain ; charset=utf-8
ctPlainText :: MediaType
ctPlainText = hText // hPlain /: (hCharset, hUTF8)

-- | application
hApplication :: BS.ByteString
hApplication = "application"

-- | json
hJSON :: BS.ByteString
hJSON = "json"

-- | text
hText :: BS.ByteString
hText = "text"

-- | plain
hPlain :: BS.ByteString
hPlain = "plain"

-- | charset
hCharset :: BS.ByteString
hCharset = "charset"

-- | utf-8
hUTF8 :: BS.ByteString
hUTF8 = "utf-8"
