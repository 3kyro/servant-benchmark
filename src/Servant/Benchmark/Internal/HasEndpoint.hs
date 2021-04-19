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

module Servant.Benchmark.Internal.HasEndpoint where

import Data.Aeson (ToJSON (toJSON))
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.List (foldl')
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API
import Servant.Benchmark.Internal.Endpoint (Endpoint (..))
import Servant.Benchmark.Internal.Generator (Generator, (:>:) (..))
import qualified Servant.Benchmark.Internal.Header as H
import Test.QuickCheck (Arbitrary, generate, listOf)

-- | HasEndpoint provides type level interpretation of an API Endpoint
class HasEndpoint (api :: Type) where
    getEndpoint :: Proxy (api :: Type) -> Generator api -> IO Endpoint
    weight :: Proxy api -> Generator api -> Word

instance
    {-# OVERLAPPING #-}
    forall (sym :: Symbol) (rest :: Type).
    (KnownSymbol sym, HasEndpoint rest) =>
    HasEndpoint (sym :> rest)
    where
    getEndpoint _ gen = do
        (<>) mempty{path = [T.pack $ symbolVal (Proxy @sym)]}
            <$> getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

instance
    {-# OVERLAPPING #-}
    forall method statusCode contentTypes a.
    (ReflectMethod method, Arbitrary a, ToJSON a) =>
    HasEndpoint (Verb method statusCode contentTypes a)
    where
    getEndpoint _ _ =
        pure $
            mempty
                { method = Just $ reflectMethod (Proxy @method)
                }
    weight _ n = n

instance
    {-# OVERLAPPING #-}
    forall contentTypes a rest.
    (Arbitrary a, ToJSON a, HasEndpoint rest) =>
    HasEndpoint (ReqBody contentTypes a :> rest)
    where
    getEndpoint _ (genLeft :>: genRest) = do
        value <- generate genLeft
        (<>) mempty{body = Just $ toJSON value}
            <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    {-# OVERLAPPING #-}
    forall (params :: Symbol) (a :: Type) (rest :: Type).
    (KnownSymbol params, Arbitrary a, Show a, HasEndpoint rest) =>
    HasEndpoint (QueryParams params a :> rest)
    where
    getEndpoint _ (genLeft :>: genRest) = do
        let queryPath = symbolVal (Proxy @params)
        arbParams <- generate $ listOf genLeft
        let queryParams = init $ foldl' (addParam queryPath) "" arbParams
        (<>) mempty{path = [T.pack $ "?" ++ queryParams]}
            <$> getEndpoint (Proxy @rest) genRest
      where
        addParam :: String -> String -> a -> String
        addParam root acc a =
            acc ++ root ++ "[]=<" ++ show a ++ ">&"
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    {-# OVERLAPPING #-}
    forall (sym :: Symbol) (rest :: Type).
    (KnownSymbol sym, HasEndpoint rest) =>
    HasEndpoint (QueryFlag sym :> rest)
    where
    getEndpoint _ gen =
        (<>) mempty{path = [T.pack $ "?" ++ symbolVal (Proxy @sym)]}
            <$> getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

instance
    {-# OVERLAPPING #-}
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (Show a, HasEndpoint rest) =>
    HasEndpoint (Capture sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        value <- generate gen
        (<>) mempty{path = [T.pack $ show value]} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

-- CaptureAll: Ignore all capture parts for now
instance
    {-# OVERLAPPING #-}
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (Show a, HasEndpoint rest) =>
    HasEndpoint (CaptureAll sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        value <- generate gen
        (<>) mempty{path = [T.pack $ show value]} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    {-# OVERLAPPING #-}
    forall (sym :: Symbol) (a :: Type) (rest :: Type).
    (KnownSymbol sym, Arbitrary a, ToJSON a, HasEndpoint rest) =>
    HasEndpoint (Header sym a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        let symbol = T.pack $ symbolVal (Proxy @sym)
        value <- generate gen
        let header = H.MkHeader symbol $ toJSON value
        (<>) mempty{headers = [header]} <$> getEndpoint (Proxy @rest) genRest
    weight _ (_ :>: genRest) = weight (Proxy @rest) genRest

instance
    {-# OVERLAPPING #-}
    forall (rest :: Type).
    HasEndpoint rest =>
    HasEndpoint (HttpVersion :> rest)
    where
    getEndpoint _ gen = getEndpoint (Proxy @rest) gen
    weight _ gen = weight (Proxy @rest) gen

-- Including EmptyAPI will always result in zero weight for the Endpoint
instance HasEndpoint EmptyAPI where
    getEndpoint _ = pure mempty
    weight _ _ = 0

instance
    forall (a :: Type) (rest :: Type).
    (Show a, Arbitrary a, ToJSON a, HasEndpoint rest) =>
    HasEndpoint (Fragment a :> rest)
    where
    getEndpoint _ (gen :>: genRest) = do
        value <- generate gen
        (<>) mempty{path = [T.pack $ '#' : show value]}
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
    getEndpoint _ _ = mempty
    weight _ n = n
