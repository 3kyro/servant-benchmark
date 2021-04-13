module Header where

import Data.Aeson (Value)
import qualified Data.Text as T

data Header = MkHeader
    { name :: T.Text
    , value :: Value
    }
    deriving (Show, Eq)
