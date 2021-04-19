{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Benchmark.Internal.Generator where

import Data.Kind (Type)
import GHC.Base (Nat, Symbol)
import Servant.API
import Test.QuickCheck (Gen)

data (a :: Type) :>: (b :: Type) = a :>: b
infixr 9 :>:

-- | a Generator provides value level interpretation of our API
type family Generator (api :: Type) where
    Generator (a :<|> b) = Generator a :<|> Generator b
    Generator (Verb (method :: k) (statusCode :: Nat) (contentTypes :: [Type]) (a :: Type)) = Word
    Generator (ReqBody (contentTypes :: [Type]) (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (QueryParams params a :> rest) = Gen a :>: Generator rest
    Generator ((sym :: Symbol) :> rest) = Generator rest
    Generator (HttpVersion :> rest) = Generator rest
    Generator (QueryFlag (sym :: Symbol) :> rest) = Generator rest
    Generator (Capture (sym :: Symbol) (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (CaptureAll (sym :: Symbol) (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (Header (sym :: Symbol) (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (Fragment (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator EmptyAPI = Word
    Generator (RemoteHost :> rest) = Generator rest
    Generator (IsSecure :> rest) = Generator rest
    Generator (WithNamedContext (name :: Symbol) (sub :: [Type]) (api :: Type)) = Generator api
    Generator (BasicAuth (realm :: Symbol) (userData :: Type) :> rest) =
        (userData -> BasicAuthData) :>: Gen userData :>: Generator rest
    Generator (Description (sym :: Symbol) :> rest) = Generator rest
    Generator (Summary (sym :: Symbol) :> rest) = Generator rest
    Generator Raw = Word
