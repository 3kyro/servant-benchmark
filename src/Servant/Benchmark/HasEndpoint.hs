{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Servant.Benchmark.HasEndpoint where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits (KnownSymbol, Nat, Symbol, symbolVal)
import Servant.API
import Servant.Benchmark.Endpoint (Endpoint (..), mkHeader)
import Servant.Benchmark.Generator (Generator, (:>:) (..))
import Servant.Benchmark.Internal.BasicAuth (encodeBasicAuth)
import Servant.Benchmark.ToText
import Test.QuickCheck (generate, listOf)

-- | HasEndpoint provides type level interpretation of an API Endpoint
class HasEndpoint (api :: Type) where
    getEndpoint :: Proxy api -> Generator api -> IO Endpoint
    weight :: Proxy api -> Generator api -> Word

instance
    forall (sym :: Symbol) (rest :: Type).
    (KnownSymbol sym, HasEndpoint rest) =>
    HasEndpoint (sym :> rest)
    where
    getEndpoint _ gen = do
        (<>) mempty{path = T.pack $ '/' : symbolVal (Proxy @sym)}
            <$> getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

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

instance
    forall (a :: Type) (rest :: Type).
    (ToJSON a, HasEndpoint rest) =>
    HasEndpoint (ReqBody '[JSON] a :> rest)
    where
    getEndpoint _ (genLeft :>: genRest) = do
        value <- generate genLeft
        (<>) mempty{body = Just $ BS.toStrict $ A.encode value}
            <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    forall (a :: Type) (rest :: Type).
    (ToText a, HasEndpoint rest) =>
    HasEndpoint (ReqBody '[PlainText] a :> rest)
    where
    getEndpoint _ (genLeft :>: genRest) = do
        value <- generate genLeft
        (<>) mempty{body = Just $ T.encodeUtf8 $ toText value}
            <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    forall (params :: Symbol) (a :: Type) (rest :: Type).
    (KnownSymbol params, ToText a, HasEndpoint rest) =>
    HasEndpoint (QueryParams params a :> rest)
    where
    getEndpoint _ (genLeft :>: genRest) = do
        let queryPath = T.pack $ symbolVal (Proxy @params)
        arbParams <- generate $ listOf genLeft
        let queryParams = T.init $ foldl' (addParam queryPath) "" arbParams
        (<>) mempty{path = '?' `T.cons` queryParams}
            <$> getEndpoint (Proxy @rest) genRest
      where
        addParam :: T.Text -> T.Text -> a -> T.Text
        addParam root acc a =
            acc <> root <> "[]=<" <> toText a <> ">&"
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    forall (sym :: Symbol) (rest :: Type).
    (KnownSymbol sym, HasEndpoint rest) =>
    HasEndpoint (QueryFlag sym :> rest)
    where
    getEndpoint _ gen =
        (<>) mempty{path = T.pack $ "?" ++ symbolVal (Proxy @sym)}
            <$> getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

instance
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (ToText a, HasEndpoint rest) =>
    HasEndpoint (Capture sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        value <- generate gen
        (<>) mempty{path = '/' `T.cons` toText value} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

-- CaptureAll: Ignore all capture parts for now
instance
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (ToText a, HasEndpoint rest) =>
    HasEndpoint (CaptureAll sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        value <- generate gen
        (<>) mempty{path = '/' `T.cons` toText value} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (KnownSymbol sym, ToText a, HasEndpoint rest) =>
    HasEndpoint (Header sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        let symbol = T.pack $ symbolVal (Proxy @sym)
        value <- generate gen
        let header = mkHeader symbol $ toText value
        (<>) mempty{headers = [header]} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    forall (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (HttpVersion :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Including EmptyAPI will always result in zero weight for the Endpoint
instance HasEndpoint EmptyAPI where
    getEndpoint _ gen = pure mempty{name = fst gen}
    weight _ _ = 0

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

instance
    forall (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (RemoteHost :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

instance
    forall (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (IsSecure :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

instance
    forall (name :: Symbol) (sub :: [Type]) (api :: Type).
    HasEndpoint api =>
    HasEndpoint (WithNamedContext name sub api)
    where
    getEndpoint _ gen = getEndpoint (Proxy @api) gen
    weight _ gen = weight (Proxy @api) gen

instance
    forall (realm :: Symbol) (userData :: Type) (rest :: Type).
    (HasEndpoint rest) =>
    HasEndpoint (BasicAuth realm userData :> rest)
    where
    getEndpoint _ (f :>: genUserData :>: genRest) = do
        authHeader <- encodeBasicAuth f genUserData
        (<>)
            mempty{headers = [authHeader]}
            <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: _ :>: genRest) = weight (Proxy @rest) genRest

instance
    forall (sym :: Symbol) (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (Description sym :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

instance
    forall (sym :: Symbol) (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (Summary sym :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

instance HasEndpoint Raw where
    getEndpoint _ gen = pure mempty{name = fst gen}
    weight _ gen = snd gen
