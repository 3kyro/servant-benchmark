# servant-benchmark

A library for producing random request data from *Servant* APIs. 

## Building a `Generator`

The `Generator` type must closely follow the structure of the *Servant* API. 

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
  The following combinators require a value generator:
    * `ReqBody`
    * `QueryParams`
    * `Capture`
    * `CaptureAll`
    * `Header`
    * `Fragment`
* For the `BasicAuth` combinator, see the dedicated section below 

As an example, the following is a valid `Generator` for a contrived servant API

````haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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
````

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

### Basic Auth

A generator for an endpoint using a `BasicAuth` combinator requires both a function to convert the
requested user data type to `BasicAuthData` as well as a `Gen` value for the requested user data. 

example:

````haskell
type privateAPI = "private" :> BasicAuth "foo-realm" User :> PrivateAPI

toBasicAuthData :: User -> BasicAuthData
toBasicAuthData user = ... 

-- assuming `User` has an `Arbitrary` instance
let generator = toBasicAuthData :>: arbitrary :>: ("basicAuth", 1)
````

The information will be encoded as an `Authorization` header.

## Supported tools

The following benchmarking tools are supported :

- [wrk](https://github.com/wg/wrk)
- [Drill](https://github.com/fcsonline/drill)
- [Siege](src/Servant/Benchmark/Tools/Siege.hs)

If you'd like your favorite tool to be supported, don't hesitate to tell me so in an issue,
or better yet submit a PR.

## Next steps

* Provide support for *servant-auth* combinators
* Expand the support for benchmarking frameworks
