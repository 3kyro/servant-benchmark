{-# LANGUAGE ExplicitNamespaces #-}

{- | A library for producing random request data from Servant APIs.

Please visit the project's [repository](https://github.com/3kyro/servant-benchmark) for more information.
-}
module Servant.Benchmark (
    Generator,

    -- * Generator builders
    (:|:) (..),
    (:>:) (..),
    HasGenerator (..),
    HasEndpoint (..),
    Endpoint (..),
) where

import Servant.Benchmark.Endpoint
import Servant.Benchmark.Generator
import Servant.Benchmark.HasEndpoint (HasEndpoint (..))
import Servant.Benchmark.HasGenerator
