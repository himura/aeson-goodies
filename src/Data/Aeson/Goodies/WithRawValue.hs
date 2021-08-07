{-# LANGUAGE DeriveGeneric #-}

module Data.Aeson.Goodies.WithRawValue
    ( WithRawValue(..)
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value)
import GHC.Generics (Generic)

data WithRawValue a = WithRawValue
    { value :: a
    , rawValue :: Value
    } deriving (Show, Eq, Generic)
instance FromJSON a => FromJSON (WithRawValue a) where
    parseJSON v = WithRawValue <$> parseJSON v <*> pure v
instance ToJSON (WithRawValue a) where
    toJSON = toJSON . rawValue
