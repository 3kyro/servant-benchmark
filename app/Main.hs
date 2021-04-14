{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Data (Proxy (..))
import qualified Data.Text as T
import Lib
import Servant.API

main :: IO ()
main = do
    putStrLn "yo"
    getAPI (Proxy :: Proxy API) >>= print

type API =
    "user" :> Get '[JSON] String
        :<|> "post" :> Header "time" Int :> ReqBody '[JSON] Int :> Post '[JSON] String
        :<|> EmptyAPI
        :<|> "books" :> QueryFlag "published" :> HttpVersion :> Capture "isbn" T.Text :> Get '[JSON] String
        :<|> "src" :> CaptureAll "segments" T.Text :> Get '[JSON] String

type Test =
    "post" :> ReqBody '[JSON] Int :> Post '[JSON] String
