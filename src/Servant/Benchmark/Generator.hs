{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |

A Generator provides value level interpretation of an API.
-}
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

{- |

A Generator provides value level interpretation of an API.

The `Generator` type must closely follow the structure of the `Servant` API.

* Different endpoints are combined with the `:|:` operator
* Different generators are combined with the `:>:` operator
* Every endpoint must end with a  `(Text, Word)` tuple consisting of the endpoint name and its corresponding weight.
  Endpoint names are only used for additional information passed to the benchmark implementations
  and do not have to follow specific rules. That being said, generators for extensive APIs can get
  rather big and hard to read, so providing sensible naming could be very beneficial.
* The weight of an endpoint specifies the number of instances per testing run
  of the API. Endpoints with 0 weight will be ignored.
* For every API combinator expecting a request value, a `Gen a` random value generator from the
  [QuickCheck](https://hackage.haskell.org/package/QuickCheck) package must be provided.
* The following combinators require a value generator:

    * `ReqBody`
    * `QueryParams`
    * `Capture`
    * `CaptureAll`
    * `Header`
    * `Fragment`

* For the `BasicAuth` combinator, see the dedicated section below

As an example, the following is a valid `Generator` for a contrived servant API

@
type API =
        "books" :> Get '[JSON] [Book]
        :<|> "view-my-referer" :> Header "from" Referer :> Get '[JSON] Referer
        :<|> "users" :> Capture "userId" Integer :> ReqBody '[JSON] User :> Put '[JSON] User
        :<|> "post" :> QueryParam "segments" Text :> Get '[JSON] Post
        :<|> Raw

generator :: Generator API
let generator =
    ("books", 1)
    :|: arbitrary :>: ("referer", 2)
    :|: pure 1001 :>: arbitrary :>: ("users endpoint", 2)
    :|: elements ["title", "contents", "post"] :>: ("post", 4)
    :|: ("raw", 0)
@

The first endpoint "books" does not require request data and so only the name / weight tuple is
provided.

The "view-my-referer" endpoint requires a "from" header with an accompanying `Referer` value. Here
we assume `Referer` has an `Arbitrary` instance to provide a random value. The endpoint generator
finishes with the name/weight indication.

The "users" endpoint requires two different request values. An Integer capture representing a user
id as well as a `User` value. We hard-code the user id to `1001` using the monadic `pure` and assume that
`User` has an `Arbitrary` instance to produce a random value. We finish with the endpoint's name/weight as necessary.

The "post" endpoint requires a `Text` query parameter. We provide a fixed set of possible values
using the `elements` function from the `QuickCheck` package. With a weight of 4, four instances of
the "post" endpoint will be produced, each with a random value from the specified set.

Finally our API provides a `Raw` endpoint for serving static files, but we'd rather not benchmark
it. Providing a 0 weight ensures that no request will be generated

== Basic Auth

A generator for an endpoint using a `BasicAuth` combinator requires both a function to convert the
requested user data type to `BasicAuthData` as well as a `Gen` value for the requested user data.

example:

@
type privateAPI = "private" :> BasicAuth "foo-realm" User :> PrivateAPI

toBasicAuthData :: User -> BasicAuthData
toBasicAuthData user = ...

-- assuming `User` has an `Arbitrary` instance
let generator = toBasicAuthData :>: arbitrary :>: ("basicAuth", 1)
@

The information will be encoded as an `Authorization` header.
-}
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
