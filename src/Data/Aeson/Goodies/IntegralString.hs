{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Goodies.IntegralString
    ( IntegralString(..)
    , IntString
    ) where

import Control.DeepSeq
import Data.Aeson.Goodies.Util
import Data.Aeson.Types hiding (Parser)
import Data.Attoparsec.Text
import Data.Ix
import Data.Scientific
import qualified Data.Text as T
import GHC.Generics

type IntString = IntegralString Int

newtype IntegralString a =
    IntegralString
        { unIntegralString :: a
        }
    deriving ( Bounded
             , Enum
             , Eq
             , Foldable
             , FromJSONKey
             , Functor
             , Generic
             , Integral
             , Ix
             , Num
             , Ord
             , Real
             , Show
             , Traversable
             )

instance (Bounded a, Integral a) => FromJSON (IntegralString a) where
    parseJSON = prependContext "IntegralString" . p
      where
        p (Number n)
            | Just i <- toBoundedInteger n = pure $ IntegralString i
            | otherwise = fail $ "fail to convert number to integer: " ++ show n
        p (String s) = do
            case parseOnly (intParser <* endOfInput) s of
                Left err -> fail $ "fail to parse as integer: " ++ T.unpack s ++ ", reason: " ++ err
                Right n -> pure $ IntegralString n
        p v = typeMismatch "Number or String" v

instance Show a => ToJSON (IntegralString a) where
    toJSON (IntegralString n) = String . T.pack . show $ n

instance NFData a => NFData (IntegralString a)

intParser :: Integral a => Parser a
intParser = signed decimal
