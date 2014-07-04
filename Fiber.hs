{-# LANGUAGE OverloadedStrings #-}
module Fiber
    ( Path
    , Fiber (..)
    , equal
    , include
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      object, (.:), (.=))
import           Data.Aeson.Types    (Parser)
import           Data.Text           (Text)


-- | Path
type Path = Text

-- | Fiber
data Fiber = Equal !Path !Value
           | Include !Path !Value
           deriving (Eq, Show)

instance FromJSON Fiber where
    parseJSON (Object v) = do
        typ <- v .: "type" :: Parser Text
        case typ of
            "equal" -> Equal <$> v .: "path" <*> v .: "value"
            "include" -> Include <$> v .: "path" <*> v .: "value"
            _ -> fail "parseJSON: unknown declaration type"
    parseJSON _ = fail "parseJSON: failed to parse a JSON into Fiber"

instance ToJSON Fiber where
    toJSON (Equal path value) = object [ "type"  .= ("equal" :: Text)
                                       , "path"  .= path
                                       , "value" .= value ]
    toJSON (Include path value) = object [ "type"  .= ("include" :: Text)
                                         , "path"  .= path
                                         , "value" .= value ]

equal :: ToJSON a => Path -> a -> Fiber
equal path value = Equal path (toJSON value)

include :: ToJSON a => Path -> a -> Fiber
include path value = Include path (toJSON value)
