{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Benchmark.ToText (ToText (..)) where

import qualified Data.ByteString as BS
import Data.Data (Proxy (Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (Method)

-- | Types that can be converted to Text and serialized.
class ToText a where
    toText :: a -> T.Text

instance (HasText a ~ textType, HasTextRepresentation textType a) => ToText a where
    toText = typeToText (Proxy @textType)

-- Types that have a Text representation as defined by the HasText type family.
-- The class helps to avoid using `show` on types that can be easily converted to `Text`
class HasTextRepresentation (s :: TextType) a where
    typeToText :: Proxy s -> a -> T.Text

instance HasTextRepresentation 'TypeString String where
    typeToText _ = T.pack

instance HasTextRepresentation 'TypeText T.Text where
    typeToText _ = id

instance HasTextRepresentation 'TypeByteString BS.ByteString where
    typeToText _ = T.decodeUtf8

instance HasTextRepresentation 'TypeMethod Method where
    typeToText _ = T.decodeUtf8

instance Show a => HasTextRepresentation 'TypeShow a where
    typeToText _ = T.pack . show

data TextType
    = TypeString
    | TypeText
    | TypeByteString
    | TypeMethod
    | TypeShow

type family HasText a where
    HasText String = 'TypeString
    HasText T.Text = 'TypeText
    HasText BS.ByteString = 'TypeByteString
    HasText a = 'TypeShow
