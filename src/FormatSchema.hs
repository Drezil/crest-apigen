module FormatSchema where

import Control.Lens.TH
import Data.Aeson (FromJSON, parseJSON, Value(..), (.:), (.:?))
import Data.HashMap.Strict (HashMap)

data Format = Schema
            { _description    :: Maybe String
            , _isOptional     :: Bool
            , _extraData      :: Maybe String
            , _subContent     :: Maybe (HashMap String Format) --warning: This assumes all subcontent to be Objects. Could be a Problem in the future. If so, fix with new SUM-Type (for Object, Array, ...)
            , _typePrettyName :: String
            , _contentType    :: String
            } deriving (Show, Eq)

instance FromJSON Format where
        parseJSON (Object v) = Schema
                               <$> v .:? "description"
                               <*> v .:  "isOptional"
                               <*> v .:? "extraData"
                               <*> v .:? "subContent"
                               <*> v .:  "typePrettyName"
                               <*> v .:  "type"
        parseJSON a          = fail $ "Error parsing Format: " ++ show a


makeLenses(''Format)
