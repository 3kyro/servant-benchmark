# servant-benchmark

A library for automatically producing random request data from *Servant* APIs. 

## Building a `Generator`

The `Generator` type must closely follow the structure of the *servant* API. 

* Different endpoints are combined with the `:<|>` operator
* Different generators are combined with the `:>:` operator
* Every endpoint must end with its corresponding weight of type `Word`
* For every API combinator expecting a request value, a `Gen a` random value generator
  must be provided. The following combinators need a value generator:
    * `ReqBody`
    * `QueryParams`
    * `Capture`
    * `CaptureAll`
    * `Header`
    * `Fragment`

As an example, the following is a valid `Generator` for a contrived servant API

````haskell
type API = 
        "books" :> Get '[JSON] [Book]
        :<|> "view-my-referer" :> Header "from" Referer :> Get '[JSON] Referer
        :<|> "users" :> Capture "userId" Integer :> ReqBody '[JSON] User :> Put '[JSON] User
        :<|> "post" :> QueryParam "segments" Text :> Get '[JSON] Post
        :<|> Raw

let generator =
    1
    :<|> arbitrary :>: 2
    :<|> pure 1001 :>: arbitrary :>: 2
    :<|> elements ["title", "contents", "post"] :>: 1
    :<|> 0
````

The first endpoint "books" does not require request data and so only the weight, 1 in this case, is
provided.

The "view-my-referer" endpoint requires a "from" header with an accompanying `Referer` value. Here
we assume `Referer` has an `Arbitrary` instance to provide a random value. The endpoint generator
finishes with the weight indication.

The "users" endpoint requires two different request values. An Integer capture representing a user
id as well as a `User`. We hardcode the user id to `1001` using the monadic `pure` and assume that
`User` has an `Arbitrary` isntance. We finish with the endpoint's weight as necessary.

The "post" endpoint requires a `Text` query parameter. We provide a fixed set of possible values
using the `elements` function from the `QuickCheck` package.

Finally our API provides a `Raw` endpoint for serving static files, but we'd rather not benchmark
it. Providing a 0 weight ensures that no request will be generated 

