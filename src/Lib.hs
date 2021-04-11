{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib (
    someFunc,
    getPath,
    getAPI,
) where

import Data.Data (Proxy (..))
import Data.Kind (Constraint, Type)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API (EmptyAPI, (:<|>), (:>))
import Servant.API.Alternative ((:<|>) ((:<|>)))
import Servant.API.Empty (EmptyAPI (EmptyAPI))
import Server

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Endpoint = MkEndpoint
    {path :: [T.Text]}
    deriving (Show, Eq)

class HasEnpoint (api :: *) where
    getEndpoint :: Proxy (api :: *) -> [Endpoint]

instance (HasEnpoint b, KnownSymbol a) => HasEnpoint ((a :: Symbol) :> b) where
    getEndpoint proxy = getPath (Proxy :: Proxy a) : getEndpoint (Proxy :: Proxy b)

instance HasEnpoint Int where
    getEndpoint _ = []

-- endPoint :: [Endpoint]
-- endPoint = getEndpoint (Proxy :: Proxy Test)

getAPI :: (HasEnpoint a, HasEnpoint b) => Proxy (a :<|> b) -> [Endpoint]
getAPI _ = getEndpoint (Proxy :: (Proxy :: a)) ++ getEndpoint (Proxy :: (Proxy :: b))

--
-- getEndpoint :: forall a. Proxy (a :: Symbol) -> Endpoint
-- getEndpoint _ = getPath (Proxy :: a)

getPath :: forall a. (KnownSymbol a) => Proxy (a :: Symbol) -> Endpoint
getPath proxy = MkEndpoint [T.pack $ symbol proxy]
  where
    symbol :: forall a. (KnownSymbol a) => Proxy (a :: Symbol) -> String
    symbol proxy' = symbolVal proxy'
