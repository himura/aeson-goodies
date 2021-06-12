{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Goodies.StringInt
    ( StringInt(..)
    ) where

import Data.Scientific
import Data.Attoparsec.Text
import Data.Aeson.Types hiding (Parser)
import Data.Aeson.Goodies.Util
import qualified Data.Text as T
import GHC.Generics

newtype StringInt a =
    StringInt
        { unStringInt :: a
        }
    deriving (Generic, Show, Eq, Ord, Bounded, Num, Enum, Real, Integral, Functor, Foldable, Traversable)

instance (Bounded a, Integral a) => FromJSON (StringInt a) where
    parseJSON = prependContext "StringInt" . p
      where
        p (Number n)
            | Just i <- toBoundedInteger n = pure $ StringInt i
            | otherwise = fail $ "fail to convert number to integer: " ++ show n
        p (String s) = do
            case parseOnly (intParser <* endOfInput) s of
                Left err -> fail $ "fail to parse as integer: " ++ T.unpack s ++ ", reason: " ++ err
                Right n -> pure $ StringInt n
        p v = typeMismatch "Number or String" v

instance Show a => ToJSON (StringInt a) where
    toJSON (StringInt n) = String . T.pack . show $ n

intParser :: Integral a => Parser a
intParser = signed decimal
