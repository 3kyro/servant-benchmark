module Servant.Benchmark (
    HasGenerator (..),
    (:>:) (..),
    HasEndpoint (..),
) where

import Servant.Benchmark.Internal.Generator
import Servant.Benchmark.Internal.HasEndpoint (HasEndpoint (..))
import Servant.Benchmark.Internal.HasGenerator
