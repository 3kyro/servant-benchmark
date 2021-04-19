module Servant.Benchmark (
    HasGenerator (..),
    (:>:) (..),
    HasEndpoint (..),
    Generator,
    Endpoint (..),
) where

import Servant.Benchmark.Endpoint
import Servant.Benchmark.Generator
import Servant.Benchmark.HasEndpoint (HasEndpoint (..))
import Servant.Benchmark.HasGenerator
