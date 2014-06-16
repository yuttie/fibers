{-# LANGUAGE OverloadedStrings #-}
module Fiber where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      object, (.:), (.=))
import           Data.Aeson.Types    (Parser)
import           Data.Text           (Text)


-- | Path
newtype Path = Path Text

instance FromJSON Path where
    parseJSON (String v) = return $ Path v
    parseJSON _ = fail "parseJSON: failed to parse a JSON into Path"

instance ToJSON Path where
    toJSON (Path t) = String t

-- | Decl
data Decl = Exist !Path !Value
type Fiber = Decl

instance FromJSON Decl where
    parseJSON (Object v) = do
        typ <- v .: "type" :: Parser Text
        case typ of
            "exist" -> Exist
                   <$> v .: "path"
                   <*> v .: "value"
            _ -> fail "parseJSON: unknown declaration type"
    parseJSON _ = fail "parseJSON: failed to parse a JSON into Decl"

instance ToJSON Decl where
    toJSON (Exist path value) = object [ "type"  .= ("exist" :: Text)
                                       , "path"  .= path
                                       , "value" .= value ]
