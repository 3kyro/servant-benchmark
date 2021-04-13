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
import qualified Header as H
import Servant.API (Capture, CaptureAll, EmptyAPI, Header, ReflectMethod (reflectMethod), ReqBody, Verb, (:>))
import Test.QuickCheck (Arbitrary (arbitrary), generate)

-- | An Endpoint in the interpreted API
class HasEndpoint (api :: k) where
    getEndpoint :: Proxy (api :: k) -> IO Endpoint

-- Endpoint parts are constructed using `:>`
instance forall k (a :: k) (b :: Type). (HasEndpoint a, HasEndpoint b) => HasEndpoint (a :> b) where
    getEndpoint _ = do
        left <- getEndpoint (Proxy @a)
        right <- getEndpoint (Proxy @b)
        pure $ left <> right

instance forall (a :: Symbol). (KnownSymbol a) => HasEndpoint a where
    getEndpoint proxy =
        pure $ MkEndpoint [T.pack $ symbol proxy] mempty Nothing mempty
      where
        symbol :: Proxy a -> String
        symbol proxy' = symbolVal proxy'

-- CaptureAll: Ignore all capture parts for now
instance forall (sym :: Symbol) (a :: Type). HasEndpoint (CaptureAll sym a) where
    getEndpoint _ = mempty

instance forall (sym :: Symbol) (a :: Type). HasEndpoint (Capture sym a) where
    getEndpoint _ = mempty

instance forall (sym :: Symbol) (a :: Type). (KnownSymbol sym, Arbitrary a, ToJSON a) => HasEndpoint (Header sym a) where
    getEndpoint _ = do
        let symbol = T.pack $ symbolVal (Proxy @sym)
        value <- generate $ arbitrary @a
        let header = H.MkHeader symbol $ toJSON value
        pure $ mempty{headers = [header]}

instance
    forall method statusCode contentTypes a.
    (ReflectMethod method, Arbitrary a, ToJSON a) =>
    HasEndpoint (Verb method statusCode contentTypes a)
    where
    getEndpoint _ = pure $ MkEndpoint mempty (Just $ reflectMethod (Proxy @method)) Nothing mempty

instance forall contentTypes a. (Arbitrary a, ToJSON a) => HasEndpoint (ReqBody contentTypes a) where
    getEndpoint _ = do
        value <- generate (arbitrary @a)
        pure $ MkEndpoint mempty mempty (Just $ toJSON value) mempty

instance HasEndpoint EmptyAPI where
    getEndpoint _ = pure mempty
