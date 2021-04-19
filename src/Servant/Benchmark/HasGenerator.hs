{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Benchmark.HasGenerator where

import Control.Monad (replicateM)
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Servant.API
import Servant.Benchmark.Endpoint (Endpoint)
import Servant.Benchmark.Generator (Generator)
import Servant.Benchmark.HasEndpoint

{- | HasGenerator provides combined type and value level interpretation of an API,
  producing corresponding `Endpoint` values.

  Instructions on forming a Generator type can be found on the module documentation.
-}
class HasGenerator api where
    generate :: Proxy api -> Generator api -> IO [Endpoint]

instance {-# OVERLAPPING #-} (HasEndpoint a, HasGenerator b) => HasGenerator (a :<|> b) where
    generate :: Proxy (a :<|> b) -> Generator (a :<|> b) -> IO [Endpoint]
    generate _ (hLeft :<|> hRight) = do
        left <- generate (Proxy @a) hLeft
        right <- generate (Proxy @b) hRight
        pure $ left ++ right

instance {-# OVERLAPPING #-} forall (a :: Type). HasEndpoint a => HasGenerator a where
    generate p gen =
        replicateM (fromIntegral $ weight p gen) (getEndpoint p gen)
