{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Data (Proxy (..))
import Servant.API
import Servant.Benchmark
import Servant.Server
import Test.QuickCheck (arbitrary)

main :: IO ()
main = do
    -- pure ()

    -- run 8080 app
    let generators =
            arbitrary :>: 1
                :<|> arbitrary :>: 1
                :<|> 1
                :<|> arbitrary :>: arbitrary :>: 1
                :<|> arbitrary :>: arbitrary :>: 1
                :<|> arbitrary :>: 1
                :<|> 3

    generate (Proxy @API) generators >>= print

type API =
    "first" :> "second" :> ReqBody '[JSON] Int :> Get '[JSON] String
        :<|> WithNamedContext "context" '[] ("time" :> QueryParams "seconds" Int :> Put '[JSON] Int)
        :<|> "capture" :> HttpVersion :> QueryFlag "flag" :> Get '[JSON] String
        :<|> "headers" :> IsSecure :> Header "first" String :> Header "second" Int :> Delete '[JSON] Int
        :<|> Summary "Summary" :> "capture" :> RemoteHost :> Capture "first" Int :> CaptureAll "second" String :> Get '[JSON] Int
        :<|> Description "deascription" :> "fragment" :> Fragment String :> Get '[JSON] String
        :<|> EmptyAPI

server :: Server API
server = undefined

app = serve (Proxy @API) server

instance HasContextEntry '[] (NamedContext "context" '[]) where
    getContextEntry _ = NamedContext EmptyContext
