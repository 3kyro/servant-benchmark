{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Benchmark.HasEndpoint (HasEndpoint (..)) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits (KnownSymbol, Nat, Symbol, symbolVal)
import Servant.API hiding (contentType, contentTypes)
import Servant.Benchmark.BasicAuth (encodeBasicAuth)
import Servant.Benchmark.Endpoint (Endpoint (..), ctJSON, ctPlainText, mkHeader)
import Servant.Benchmark.Generator (Generator, (:>:) (..))
import Servant.Benchmark.ToText
import Test.QuickCheck (generate, listOf)

-- | HasEndpoint provides type level interpretation of an API Endpoint
class HasEndpoint (api :: Type) where
    getEndpoint :: Proxy api -> Generator api -> IO Endpoint
    weight :: Proxy api -> Generator api -> Word

-- Instance for ' "path" :> .... " parts
-- Add sym to the path and continue
instance
    forall (sym :: Symbol) (rest :: Type).
    (KnownSymbol sym, HasEndpoint rest) =>
    HasEndpoint (sym :> rest)
    where
    getEndpoint _ gen = do
        (<>) mempty{path = T.pack $ '/' : symbolVal (Proxy @sym)}
            <$> getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Instance for common verbs - e.g. GET, PUT etc
-- A verb is always the last part of the API and so does not continue the interpretation. As such the
-- endpoint's description and weight are parsed here.
instance
    forall k (method :: k) (statusCode :: Nat) (contentTypes :: [Type]) (a :: Type).
    ReflectMethod method =>
    HasEndpoint (Verb method statusCode contentTypes a)
    where
    getEndpoint _ gen =
        pure $
            mempty
                { name = fst gen
                , method = Just $ reflectMethod (Proxy @method)
                }
    weight _ gen = snd gen

-- Instance for Request body combinators.
-- A separate instance for each content type is provided.
-- Instances exist for JSON and PLAINTEXT content types.
-- A Generator for body type is expected. This generator is extracted and
-- the remaining part is passed on.
instance
    forall (a :: Type) (rest :: Type).
    (ToJSON a, HasEndpoint rest) =>
    HasEndpoint (ReqBody '[JSON] a :> rest)
    where
    getEndpoint _ (genLeft :>: genRest) = do
        value <- generate genLeft
        (<>)
            mempty
                { body = Just $ BS.toStrict $ A.encode value
                , contentType = Just ctJSON
                }
            <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    forall (a :: Type) (rest :: Type).
    (ToText a, HasEndpoint rest) =>
    HasEndpoint (ReqBody '[PlainText] a :> rest)
    where
    getEndpoint _ (genLeft :>: genRest) = do
        value <- generate genLeft
        (<>)
            mempty
                { body = Just $ T.encodeUtf8 $ toText value
                , contentType = Just ctPlainText
                }
            <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

-- Instance for query parameters.
-- Query parameters of the form `QueryParams "root" Word` should produce a path segment similar to:
-- `?root[]=<1>&root[]=<2>`
-- A Generator for the query type is expected. This generator is extracted and
-- the remaining part is passed on.
instance
    forall (params :: Symbol) (a :: Type) (rest :: Type).
    (KnownSymbol params, ToText a, HasEndpoint rest) =>
    HasEndpoint (QueryParams params a :> rest)
    where
    getEndpoint _ (genLeft :>: genRest) = do
        let queryPath = T.pack $ symbolVal (Proxy @params)
        arbParams <- generate $ listOf genLeft
        -- build the query parameters, throwing away the trailing '&'
        let queryParams = T.init $ foldl' (addParam queryPath) "" arbParams
        -- concat the endpoint with the rest
        (<>) mempty{path = '?' `T.cons` queryParams}
            <$> getEndpoint (Proxy @rest) genRest
      where
        addParam :: T.Text -> T.Text -> a -> T.Text
        addParam root acc a =
            acc <> root <> "[]=<" <> toText a <> ">&"
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

-- Instance for query flags
-- Query flags of the form `QueryFlag "flag"` should produce a path segment similar to `?flag`
instance
    forall (sym :: Symbol) (rest :: Type).
    (KnownSymbol sym, HasEndpoint rest) =>
    HasEndpoint (QueryFlag sym :> rest)
    where
    getEndpoint _ gen =
        (<>) mempty{path = T.pack $ "?" ++ symbolVal (Proxy @sym)}
            <$> getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Instance for Capture combinator
-- A capture of the form `Capture "root" Int" should produce a path segment `/12`
-- A Generator for the capture type is expected. This generator is extracted and
-- the remaining part is passed on.
instance
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (ToText a, HasEndpoint rest) =>
    HasEndpoint (Capture sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        value <- generate gen
        -- concat the endpoint with the rest
        (<>) mempty{path = '/' `T.cons` toText value} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

-- Instance for CaptureAll combinator
-- A capture of the form `CaptureAll "root" Int" should produce a path segment `/12`
-- A Generator for the capture type is expected. This generator is extracted and
-- the remaining part is passed on.
instance
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (ToText a, HasEndpoint rest) =>
    HasEndpoint (CaptureAll sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        value <- generate gen
        -- concat the endpoint with the rest
        (<>) mempty{path = '/' `T.cons` toText value} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

-- Instance for Header combinator
-- A Generator for the header type is expected. This generator is extracted and
-- the remaining part is passed on.
instance
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (KnownSymbol sym, ToText a, HasEndpoint rest) =>
    HasEndpoint (Header sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        let headerName = T.pack $ symbolVal (Proxy @sym)
        headerValue <- generate gen
        let header = mkHeader headerName $ toText headerValue
        -- concat the endpoint with the rest
        (<>) mempty{headers = [header]} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

-- Instance for HttpVersion combinator
-- No intermediate endpoint is produced
instance
    forall (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (HttpVersion :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Instance for the EmptyAPI combinator.
-- EmptyAPI is considered the unit value for top level API combinators.
-- As such, including EmptyAPI will finish the interpretation with an empty endpoint and zero weight.
instance HasEndpoint EmptyAPI where
    getEndpoint _ gen = pure mempty{name = fst gen}
    weight _ _ = 0

-- Instance for the fragment combinator.
-- A Generator for the fragment type is expected. This generator is extracted and
-- the remaining part is passed on.
instance
    forall (a :: Type) (rest :: Type).
    (ToText a, HasEndpoint rest) =>
    HasEndpoint (Fragment a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        value <- generate gen
        (<>) mempty{path = '#' `T.cons` toText value}
            <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

-- Instance for RemoteHost combinator
-- No intermediate endpoint is produced
instance
    forall (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (RemoteHost :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Instance for IsSecure combinator
-- No intermediate endpoint is produced
instance
    forall (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (IsSecure :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Instance for the WithNamedContext combinator.
-- No intermediate endpoint is produced
instance
    forall (name :: Symbol) (sub :: [Type]) (api :: Type).
    HasEndpoint api =>
    HasEndpoint (WithNamedContext name sub api)
    where
    getEndpoint _ gen = getEndpoint (Proxy @api) gen
    weight _ gen = weight (Proxy @api) gen

-- Instance for the BasicAuth combinator.
-- An authorization header is added to the endpoint.
-- A Generator for the userData type is expected. This generator is extracted and
-- the remaining part is passed on.
instance
    forall (realm :: Symbol) (userData :: Type) (rest :: Type).
    (HasEndpoint rest) =>
    HasEndpoint (BasicAuth realm userData :> rest)
    where
    getEndpoint _ (f :>: genUserData :>: genRest) = do
        authHeader <- encodeBasicAuth f genUserData
        -- concat the endpoint with the rest
        (<>)
            mempty{headers = [authHeader]}
            <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: _ :>: genRest) = weight (Proxy @rest) genRest

-- Instance for the Description combinator.
-- No intermediate endpoint is produced
instance
    forall (sym :: Symbol) (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (Description sym :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Instance for the Summary combinator.
-- No intermediate endpoint is produced
instance
    forall (sym :: Symbol) (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (Summary sym :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Instance for the Raw combinator.
-- Raw is a final combinator and so endpoint name and weight are
-- extracted.
instance HasEndpoint Raw where
    getEndpoint _ gen = pure mempty{name = fst gen}
    weight _ gen = snd gen
