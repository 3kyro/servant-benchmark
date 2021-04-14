{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module HasEndpoint where

import Data.Aeson (ToJSON (toJSON))
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.List (foldl')
import qualified Data.Text as T
import Endpoint (Endpoint (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Header as H
import Servant.API (Capture, CaptureAll, EmptyAPI, Header, HttpVersion, QueryFlag, QueryParams, ReflectMethod (reflectMethod), ReqBody, Verb, (:>))
import Test.QuickCheck (Arbitrary (arbitrary), generate)

-- | An Endpoint in the interpreted API
class HasEndpoint (api :: Type) where
    getEndpoint :: Proxy (api :: Type) -> IO Endpoint

-- Endpoint parts are constructed using `:>`
instance {-# OVERLAPPING #-} forall (a :: Type) (b :: Type). (HasEndpoint a, HasEndpoint b) => HasEndpoint (a :> b) where
    getEndpoint _ = do
        left <- getEndpoint (Proxy @a)
        right <- getEndpoint (Proxy @b)
        pure $ left <> right

instance
    {-# OVERLAPPING #-}
    forall (sym :: Symbol) (rest :: Type).
    (KnownSymbol sym, HasEndpoint rest) =>
    HasEndpoint (sym :> rest)
    where
    getEndpoint _ = do
        let left = mempty{path = [T.pack $ symbolVal (Proxy @sym)]}
        right <- getEndpoint (Proxy @rest)
        pure $ left <> right

instance
    {-# OVERLAPPING #-}
    forall (path :: Symbol) (sym :: Symbol) (rest :: Type).
    (KnownSymbol path, KnownSymbol sym, HasEndpoint rest) =>
    HasEndpoint (path :> QueryFlag sym :> rest)
    where
    getEndpoint _ = do
        let left = mempty{path = [T.pack $ qPath ++ "?" ++ qFlag]}
        right <- getEndpoint (Proxy @rest)
        pure $ left <> right
      where
        qPath = symbolVal (Proxy @path)
        qFlag = symbolVal (Proxy @sym)

instance
    {-# OVERLAPPING #-}
    forall (path :: Symbol) (params :: Symbol) (a :: Type) (rest :: Type).
    (KnownSymbol path, KnownSymbol params, Arbitrary a, Show a, HasEndpoint rest) =>
    HasEndpoint (path :> QueryParams params [a] :> rest)
    where
    getEndpoint _ = do
        let basePath = symbolVal (Proxy @path)
        let queryPath = symbolVal (Proxy @params)
        arbParams <- generate $ arbitrary @[a]
        let queryParams = init $ foldl' (addParam queryPath) "" arbParams
        let left = mempty{path = [T.pack $ basePath ++ "?" ++ queryParams]}
        right <- getEndpoint (Proxy @rest)
        pure $ left <> right
      where
        addParam :: String -> String -> a -> String
        addParam root acc a =
            acc ++ root ++ "[]=<" ++ show a ++ ">&"

-- CaptureAll: Ignore all capture parts for now
instance {-# OVERLAPPING #-} forall (sym :: Symbol) (a :: Type). HasEndpoint (CaptureAll sym a) where
    getEndpoint _ = mempty

instance {-# OVERLAPPING #-} forall (sym :: Symbol) (a :: Type). HasEndpoint (Capture sym a) where
    getEndpoint _ = mempty

instance {-# OVERLAPPING #-} forall (sym :: Symbol) (a :: Type). (KnownSymbol sym, Arbitrary a, ToJSON a) => HasEndpoint (Header sym a) where
    getEndpoint _ = do
        let symbol = T.pack $ symbolVal (Proxy @sym)
        value <- generate $ arbitrary @a
        let header = H.MkHeader symbol $ toJSON value
        pure $ mempty{headers = [header]}

instance {-# OVERLAPPING #-} HasEndpoint HttpVersion where
    getEndpoint _ = mempty

instance
    {-# OVERLAPPING #-}
    forall method statusCode contentTypes a.
    (ReflectMethod method, Arbitrary a, ToJSON a) =>
    HasEndpoint (Verb method statusCode contentTypes a)
    where
    getEndpoint _ = pure $ mempty{method = Just $ reflectMethod (Proxy @method)}

instance {-# OVERLAPPING #-} forall contentTypes a. (Arbitrary a, ToJSON a) => HasEndpoint (ReqBody contentTypes a) where
    getEndpoint _ = do
        value <- generate (arbitrary @a)
        pure $ mempty{requestValue = Just $ toJSON value}

instance HasEndpoint EmptyAPI where
    getEndpoint _ = pure mempty
