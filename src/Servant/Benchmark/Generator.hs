{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Benchmark.Generator where

import Data.Kind (Type)
import qualified Data.Text as T
import GHC.Base (Nat, Symbol)
import Servant.API
import Test.QuickCheck (Gen)

{- | Value level `Generator` combinator. Combine endpoint generators to build an API generator

example:

@
-- The API we want to benchmark
type API = "books" :> Get '[JSON] Book :<|> "authors" :> "authors" :> ReqBody '[PlainText] String :> Post '[JSON] [Author]

generator :: Generator API
generator = ("books", 1) :|: elements ["Cervantes", "Kant"] :>: ("authors", 2)
@
-}
data (a :: Type) :|: (b :: Type) = a :|: b

infixr 3 :|:

{- | Value level `Generator` combinator. Build endpoint generators by combining `Gen` values with (description, weight) tuples

 example:

 @
 -- A single endpoint API
 type API = "authors" :> "authors" :> ReqBody '[PlainText] String :> Post '[JSON] [Author]

generator :: Generator API
generator = elements ["Cervantes", "Kant"] :>: ("authors", 2)
@
-}
data (a :: Type) :>: (b :: Type) = a :>: b

infixr 9 :>:

-- | A Generator provides value level interpretation of an API
type family Generator (api :: Type) where
    Generator (a :<|> b) = Generator a :|: Generator b
    Generator (Verb (method :: k) (statusCode :: Nat) (contentTypes :: [Type]) (a :: Type)) = (T.Text, Word)
    Generator (ReqBody '[JSON] (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (ReqBody '[PlainText] (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (QueryParams params a :> rest) = Gen a :>: Generator rest
    Generator ((sym :: Symbol) :> rest) = Generator rest
    Generator (HttpVersion :> rest) = Generator rest
    Generator (QueryFlag (sym :: Symbol) :> rest) = Generator rest
    Generator (Capture (sym :: Symbol) String :> rest) = Gen String :>: Generator rest
    Generator (Capture (sym :: Symbol) (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (CaptureAll (sym :: Symbol) (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (Header (sym :: Symbol) (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator (Fragment (a :: Type) :> rest) = Gen a :>: Generator rest
    Generator EmptyAPI = (T.Text, Word)
    Generator (RemoteHost :> rest) = Generator rest
    Generator (IsSecure :> rest) = Generator rest
    Generator (WithNamedContext (name :: Symbol) (sub :: [Type]) (api :: Type)) = Generator api
    Generator (BasicAuth (realm :: Symbol) (userData :: Type) :> rest) =
        (userData -> BasicAuthData) :>: Gen userData :>: Generator rest
    Generator (Description (sym :: Symbol) :> rest) = Generator rest
    Generator (Summary (sym :: Symbol) :> rest) = Generator rest
    Generator Raw = (T.Text, Word)
