{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decode, encode)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.UTF8 (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (hAuthorization, methodDelete, methodGet, methodPost, methodPut)
import Servant
import Servant.Benchmark
import qualified Servant.Benchmark.Drill as D
import Test.Hspec
import Test.QuickCheck (arbitrary)

main :: IO ()
main = do
    generateSpec
    basicAuthSpec
    encodeSpec

generators =
    ("get", 3)
        :<|> arbitrary :>: ("zero", 0)
        :<|> arbitrary :>: ("first", 1)
        :<|> arbitrary :>: ("third", 1)
        :<|> arbitrary :>: ("context", 1)
        :<|> ("capture", 1)
        :<|> arbitrary :>: arbitrary :>: ("headers", 1)
        :<|> arbitrary :>: arbitrary :>: ("summary", 1)
        :<|> arbitrary :>: ("description", 1)
        :<|> ("raw", 1)

type API =
    "get" :> Get '[JSON] String
        :<|> "zero" :> ReqBody '[JSON] String :> Post '[JSON] String
        :<|> "first" :> "second" :> ReqBody '[JSON] Int :> Post '[JSON] String
        :<|> "third" :> ReqBody '[PlainText] String :> Put '[JSON] String
        :<|> WithNamedContext "context" '[] ("time" :> QueryParams "seconds" Int :> Put '[JSON] Int)
        :<|> "capture" :> HttpVersion :> QueryFlag "flag" :> Get '[JSON] String
        :<|> "headers" :> IsSecure :> Header "first" String :> Header "second" Int :> Delete '[JSON] Int
        :<|> Summary "Summary" :> "capture" :> RemoteHost :> Capture "first" Int :> CaptureAll "second" String :> Post '[JSON] Int
        :<|> Description "description" :> "fragment" :> Fragment String :> Get '[JSON] String
        :<|> Raw

generateSpec :: IO ()
generateSpec = do
    endpoints <- liftIO $ generate (Proxy @API) generators
    hspec $
        describe "generate" $ do
            it "correctly retrieves endpoint weight and method" $ do
                let gets = take 3 endpoints
                method <$> gets `shouldBe` replicate 3 (Just methodGet)
                drop 3 (method <$> endpoints)
                    `shouldBe` [ Just methodPost
                               , Just methodPut
                               , Just methodPut
                               , Just methodGet
                               , Just methodDelete
                               , Just methodPost
                               , Just methodGet
                               , Nothing
                               ]
            it "correctly retrieves endpoint names" $ do
                name <$> drop 2 endpoints
                    `shouldBe` [ "get"
                               , "first"
                               , "third"
                               , "context"
                               , "capture"
                               , "headers"
                               , "summary"
                               , "description"
                               , "raw"
                               ]

type BasicAuthSpecAPI = BasicAuth "realm" User :> Get '[JSON] User

basicAuthGenerator = fromUser :>: pure (MkUser "foo_user" "bar_pass") :>: ("basic auth", 1)
data User = MkUser T.Text T.Text

fromUser :: User -> BasicAuthData
fromUser (MkUser name pass) =
    BasicAuthData (T.encodeUtf8 name) (T.encodeUtf8 pass)

basicAuthSpec :: IO ()
basicAuthSpec =
    hspec $
        describe "BasicAuth support" $
            it "correctly produces authorization headers" $ do
                endpointHeader <- headers . head <$> generate (Proxy @BasicAuthSpecAPI) basicAuthGenerator
                let bs64 = BS8.pack "Basic " <> encode (fromString "foo_user:bar_pass")
                endpointHeader `shouldBe` [(hAuthorization, bs64)]

-- Produce a yaml output file for debugging
encodeSpec :: IO ()
encodeSpec = do
    endpoints <- liftIO $ generate (Proxy @API) generators
    let settings = D.MkSettings 4 "localhost" 3 3
    BS8.writeFile "/home/kyro/repos/servant-benchmark/output.yaml" $ D.encode settings endpoints
