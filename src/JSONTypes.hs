module JSONTypes where

import Data.Aeson
import Data.Monoid

-- | References an URL where further CREST-Data can be found
data Ref = Ref String deriving (Show, Eq)

instance FromJSON Ref where
    parseJSON (Object o) = Ref <$> o .: "href"
    parseJSON a = fail $ "Error: " <> show a

-- | References an external URL with typically HTML-Content
data ExternalRef = ExternalRef String deriving (Show, Eq)

instance FromJSON ExternalRef where
    parseJSON (Object o) = ExternalRef <$> o .: "href"
    parseJSON a = fail $ "Error: " <> show a

-- | captures an Uri
data Uri = Uri String deriving (Show, Eq)

instance FromJSON Uri where
    parseJSON (Object o) = Uri <$> o .: "href"
    parseJSON a = fail $ "Error: " <> show a

