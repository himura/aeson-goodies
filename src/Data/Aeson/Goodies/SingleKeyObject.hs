{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aeson.Goodies.SingleKeyObject where

import Control.DeepSeq ( NFData )
import Data.Aeson
    ( FromJSON(parseJSON)
    , FromJSON1(liftParseJSON)
    , KeyValue((.=))
    , ToJSON(toEncoding, toJSON)
    , ToJSON1(liftToEncoding, liftToJSON)
    , (.:)
    , object
    , pairs
    , parseJSON1
    , toEncoding1
    , toJSON1
    , withObject
    )
import Data.Aeson.Encoding (pair)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.Key as AesonKey
#else
import qualified Data.Text as T
#endif

#if MIN_VERSION_aeson(2, 0, 0)
keyFromSymbol :: forall n proxy. KnownSymbol n => proxy n -> AesonKey.Key
keyFromSymbol = AesonKey.fromString . symbolVal
#else
keyFromSymbol :: forall n proxy. KnownSymbol n => proxy n -> T.Text
keyFromSymbol = T.pack . symbolVal
#endif

newtype SingleKeyObject (s :: Symbol) a =
    SingleKeyObject
        { getSingleKeyObject :: a
        }
    deriving (Eq, Foldable, Functor, Generic, Generic1, Show, Traversable)

instance KnownSymbol s => FromJSON1 (SingleKeyObject s) where
    liftParseJSON p _ =
        withObject ("SingleKeyObject \"" ++ keyStr ++ "\"") $ \obj -> SingleKeyObject <$> (obj .: key >>= p)
      where
        keyStr = symbolVal (Proxy :: Proxy s)
        key = keyFromSymbol (Proxy :: Proxy s)

instance (KnownSymbol s, FromJSON a) => FromJSON (SingleKeyObject s a) where
    parseJSON = parseJSON1

instance KnownSymbol s => ToJSON1 (SingleKeyObject s) where
    liftToJSON to' _ (SingleKeyObject a) = object [key .= to' a]
      where
        key = keyFromSymbol (Proxy :: Proxy s)
    liftToEncoding to' _ (SingleKeyObject a) = pairs (pair key $ to' a)
      where
        key = keyFromSymbol (Proxy :: Proxy s)

instance (KnownSymbol s, ToJSON a) => ToJSON (SingleKeyObject s a) where
    toJSON = toJSON1
    toEncoding = toEncoding1

instance NFData a => NFData (SingleKeyObject s a)
