{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HasAPI where

import Data.Data (Proxy (..))
import Data.Kind (Type)
import Endpoint (Endpoint)
import HasEndpoint (HasEndpoint (..))
import Servant.API ((:<|>))

class HasAPI (api :: *) where
    getAPI :: Proxy api -> IO [Endpoint]

instance {-# OVERLAPPING #-} (HasEndpoint a, HasAPI b) => HasAPI ((a :: Type) :<|> (b :: Type)) where
    getAPI _ = do
        left <- getEndpoint (Proxy :: Proxy a)
        right <- getAPI (Proxy :: Proxy b)
        pure $ left : right

instance {-# OVERLAPPING #-} forall (a :: Type). (HasEndpoint a) => HasAPI a where
    getAPI p = do
        endpoint <- getEndpoint p
        pure [endpoint]
