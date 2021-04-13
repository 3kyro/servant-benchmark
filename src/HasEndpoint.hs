{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HasEndpoint where

import Data.Aeson (ToJSON (toJSON))
import Data.Data (Proxy (..))
import Data.Kind (Type)
import qualified Data.Text as T
import Endpoint (Endpoint (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API (EmptyAPI, ReflectMethod (reflectMethod), ReqBody, Verb, (:>))
import Test.QuickCheck (Arbitrary (arbitrary), generate)

class HasEndpoint (api :: k) where
    getEndpoint :: Proxy (api :: k) -> IO Endpoint

instance forall k (a :: k) (b :: Type). (HasEndpoint a, HasEndpoint b) => HasEndpoint (a :> b) where
    getEndpoint _ = do
        left <- getEndpoint (Proxy @a)
        right <- getEndpoint (Proxy @b)
        pure $ left <> right

instance forall (a :: Symbol). (KnownSymbol a) => HasEndpoint a where
    getEndpoint proxy =
        pure $ MkEndpoint [T.pack $ symbol proxy] mempty Nothing
      where
        symbol :: Proxy a -> String
        symbol proxy' = symbolVal proxy'
instance
    forall method statusCode contentTypes a.
    (ReflectMethod method, Arbitrary a, ToJSON a) =>
    HasEndpoint (Verb method statusCode contentTypes a)
    where
    getEndpoint _ = pure $ MkEndpoint mempty (Just $ reflectMethod (Proxy @method)) Nothing

instance forall contentTypes a. (Arbitrary a, ToJSON a) => HasEndpoint (ReqBody contentTypes a) where
    getEndpoint _ = do
        value <- generate (arbitrary @a)
        pure $ MkEndpoint mempty mempty $ Just $ toJSON value

instance HasEndpoint EmptyAPI where
    getEndpoint _ = pure $ mempty
