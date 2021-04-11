{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import qualified Data.Text as T
import Servant.API

type API =
    "user" :> Get '[JSON] Int
        :<|> "post" :> ReqBody '[JSON] Int :> Post '[JSON] T.Text

type Test =
    "user" :> "posts" :> Int
        :<|> "chamber" :> Int
