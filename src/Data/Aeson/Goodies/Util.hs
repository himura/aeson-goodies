module Data.Aeson.Goodies.Util where

import Data.Aeson.Types (Parser, prependFailure)

prependContext :: String -> Parser a -> Parser a
prependContext name = prependFailure ("parsing " ++ name ++ " failed, ")
