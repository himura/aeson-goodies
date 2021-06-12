{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Goodies.NumBool
    ( NumBool(..)
    ) where

import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), Value(Bool, Number, String), typeMismatch)
import Data.Aeson.Goodies.Util (prependContext)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype NumBool =
    NumBool
        { unNumBool :: Bool
        }
    deriving (Show, Eq, Ord, Bounded, Generic)

instance FromJSON NumBool where
    parseJSON (String s) = pure (NumBool v)
      where
        v =
            case T.toLower s of
                ls
                    | T.isPrefixOf "t" ls -> True
                ls
                    | T.isPrefixOf "1" ls -> True
                _ -> False
    parseJSON (Number n) = pure $ NumBool $ n > 0
    parseJSON (Bool b) = pure $ NumBool b
    parseJSON v = prependContext "NumBool" $ typeMismatch "Bool, String or Number" v

instance ToJSON NumBool where
    toJSON (NumBool b) = Bool b
