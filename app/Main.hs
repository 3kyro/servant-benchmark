{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Data (Proxy (..))
import Lib
import Servant.API

main :: IO ()
main = do
    putStrLn "yo"
    getAPI (Proxy :: Proxy API) >>= print

type API =
    "user" :> Get '[JSON] String
        :<|> "post" :> ReqBody '[JSON] Int :> Post '[JSON] String
        :<|> EmptyAPI

type Test =
    "post" :> ReqBody '[JSON] Int :> Post '[JSON] String
