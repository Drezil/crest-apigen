module Representations where

import Control.Lens.TH
import Data.Aeson (FromJSON,parseJSON,Value(..),(.:),eitherDecode)
import Control.Monad (mzero)
import Data.Vector (toList)
import Control.Exception (Exception,SomeException,toException)
import Network.HTTP.Conduit (parseUrl,httpLbs, responseBody, Manager, Request(..))
import Data.Bifunctor (first)

newtype Representations = Representations { unrep :: [Representation] } deriving (Show, Eq)

instance FromJSON Representations where
        parseJSON (Object v) = v .: "representations"
        parseJSON (Array a)  = Representations <$> sequenceA (parseJSON <$> toList a)
        parseJSON a          = fail $ "Representations: " ++ show a

data Representation = Representation
                    { _acceptType :: RepAcceptType
                    , _verb       :: String
                    , _version    :: Int
                    , _versionStr :: String
                    } deriving (Show, Eq)

instance FromJSON Representation where
        parseJSON (Object v) = Representation
                               <$> v .: "acceptType"
                               <*> v .: "verb"
                               <*> v .: "version"
                               <*> v .: "version_str"
        parseJSON a          = fail $ "Representation: " ++ show a

data RepAcceptType = RepAcceptType
                   { _name                :: String
                   , _jsonDumpOfStructure :: String
                   } deriving (Show, Eq)

instance FromJSON RepAcceptType where
        parseJSON (Object v) = RepAcceptType
                               <$> v .: "name"
                               <*> v .: "jsonDumpOfStructure"
        parseJSON a          = fail $ "RepAcceptType: " ++ show a

makeLenses(''Representation)
makeLenses(''RepAcceptType)

newtype ParseException = ParseException String
                                deriving (Show)

instance Exception ParseException

getRepresentations :: Manager -> String -> IO (Either SomeException Representations)
getRepresentations man url' = do
        url <- parseUrl url'
        let req = url { method = "OPTIONS" }
        response <- httpLbs req man
        print $ responseBody response
        return . first (toException . ParseException) . eitherDecode . responseBody $ response
