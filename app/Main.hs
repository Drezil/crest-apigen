module Main where

import Representations
import FormatSchema
import JSONTypes
import Network.HTTP.Conduit (newManager,tlsManagerSettings, Manager, parseUrl, httpLbs, Request(..),responseBody)
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict', Value(..))
import qualified Data.ByteString.Char8 as BS (pack)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (catMaybes)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Data.Monoid ((<>))
import Data.HashMap.Strict (elems, mapWithKey, HashMap)
import qualified Data.HashMap.Strict as HM (fromList, toList, (!), lookup)
import qualified Data.Vector as V (toList, (!))
import Data.Char (toUpper, toLower)
import Data.List (intersperse, isPrefixOf)
import qualified Data.Text as T (pack,unpack)

crestEndpoint = "https://public-crest.eveonline.com"

outputDir = "./generated/"

main :: IO ()
main = do
        man <-newManager tlsManagerSettings
        reps' <- getRepresentations man crestEndpoint
        case reps' of
          Left err   -> putStrLn $ "An error occured: " ++ show err
          Right (Representations reps) -> do
                putStrLn "Found Representations for:"
                mapM_ (putStrLn . (\(a,b) -> show a ++ ": " ++ (_name . _acceptType $ b))) (zip [1..] reps)
                putStrLn ""
                numRep <- readReps (length reps)
                let selRep = reps !! (numRep - 1)
                putStrLn $ "Generationg for " ++ (_name . _acceptType $ selRep)
                let fmt = eitherDecodeStrict' . BS.pack . _jsonDumpOfStructure . _acceptType $ selRep :: Either String Format
                case fmt of
                  Left err -> putStrLn $ "Error generating API: " <> err
                  Right fmt' -> generateApi man crestEndpoint outputDir fmt'


readReps :: Int -> IO Int
readReps lim = do
        putStrLn $ "Please Select [1-" ++ show lim ++ "]"
        l <- readMaybe <$> getLine
        case l of
          (Just l') -> if l' > 0 && l' <= lim then
                           return l'
                       else do
                           putStrLn "Invalid Input"
                           readReps lim
          _ -> do
                  putStrLn "Invalid Input"
                  readReps lim

generateApi :: Manager -> String -> FilePath -> Format -> IO ()
generateApi man c o f = do
        createDirectoryIfMissing True o
        (content, refs) <- interpretMain man c f
        writeFile (o </> "Api.hs") content
        --mapM_ (generateRefApi man o) refs

interpretMain :: Manager -> String -> Format -> IO (String, [([String], Ref)])
interpretMain man url' f = do
        url <- parseUrl url'
        let req = url { method = "GET" }
        response <- httpLbs req man
        let raw = eitherDecodeStrict' . toStrict . responseBody $ response :: Either String Value
        case raw of
          Left err -> do
                  putStrLn ("Error occured: " ++ err)
                  return ("",[])
          Right structure -> return $ generateMain structure f

generateMain :: Value -> Format -> (String, [([String], Ref)])
generateMain vs f = (generateHaskell "Api" datatypes, generateRefs vs f)
        where datatypes = generateType "api" f

generateRefs :: Value -> Format -> [([String], Ref)]
generateRefs = go []
        where
                go :: [String] -> Value -> Format -> [([String], Ref)]
                go path (Object o) (Schema _ _ _ (Just s) _ "Dict") = concat . catMaybes $ (\(k,f) -> do
                                               v' <- HM.lookup (T.pack k) o
                                               return $ go (path <> [k]) v' f) <$> HM.toList s
                go path (Array  a) s@(Schema _ _ _ _ _ "Array") = V.toList a >>= (\v -> go path v s)
                go path (Object o) (Schema _ _ _ (Just s) _ "Ref") = case o HM.! "href" of
                                                                       String s -> [(path,Ref (T.unpack s))]
                                                                       _ -> []
                go path (Object o) (Schema _ _ _ (Just s) _ _) = concat . catMaybes $ (\(k,f) -> do
                                               v' <- HM.lookup (T.pack k) o
                                               return $ go (path <> [k]) v' f) <$> HM.toList s
                go _ _ _ = []

generateHaskell :: String -> [DataType] -> String
generateHaskell mod dt = generateHeader mod ++ (unlines . intersperse "" . fmap dataTypeToHaskell $ dt)
    where
            generateHeader mod = unlines
                               [ "module "<>mod<>" where"
                               , ""
                               , "import Data.Aeson"
                               , "import JSONTypes"
                               , "import Data.Monoid"
                               , "import Data.Int"
                               , ""
                               ]
            dataTypeToHaskell :: DataType -> String
            dataTypeToHaskell (DataType n des fs) = unlines $
                      "data "<> n <> " = " <> n
                    : "     { " <> fst (head f) <> " :: " <> snd (head f)
                    : ((\f' -> "     , " <> fst f' <> " :: " <> snd f') <$> tail f)
                    ++ 
                      "     } deriving (Show, Eq)"
                    : ""
                    : "instance FromJSON "<>n <>" where"
                    : "  parseJSON (Object o) = " <> n
                    : "                       <$> o " <> mp (head p)
                    : ((\p' -> "                       <*> o " <> mp p') <$> tail p)
                    ++ ["  parseJSON a          = fail $ \"Error: \" <> show a",""]
                where
                    f :: [(String, String)]
                    f = HM.toList $ fst <$> fs
                    p :: [(String, String)]
                    p = elems fs
                    mp :: (String, String) -> String
                    mp (f,s)
                      | "Maybe" `isPrefixOf` f = " .:? \""<>s<>"\""
                      | otherwise              = " .:  \""<>s<>"\""


generateType :: String -> Format -> [DataType]
generateType name (Schema d _ _ (Just s) _ "Dict") = DataType (capitalize name) d (convertFormat name s):(concat . elems $ mapWithKey generateType s)
generateType name (Schema d _ _ (Just s) _ "Array") = DataType (capitalize name) d (convertFormat name s):(concat . elems $ mapWithKey generateType s)
generateType _ _ = []


data DataType = DataType
              { name :: String
              , description :: Maybe String
              , fields :: HashMap String (String,String)
              } deriving (Show, Eq)

convertFormat :: String -> HashMap String Format -> HashMap String (String,String)
convertFormat n = HM.fromList . catMaybes . fmap go . HM.toList
  where
          go :: (String, Format) -> Maybe (String, (String,String))
          go (k,Schema _ o _ _ _ "Dict") = if o then
                                                  Just ("_" <> lower n <> capitalize k,("Maybe "<>capitalize k,k))
                                             else
                                                  Just ("_" <> lower n <> capitalize k,(capitalize k,k))
          go (k,Schema _ o _ _ _ "Array") = if o then
                                                  Just ("_" <> lower n <> capitalize k,("Maybe ["<>capitalize k<>"]",k))
                                             else
                                                  Just ("_" <> lower n <> capitalize k,("["<>capitalize k<>"]",k))
          go (k,Schema _ o _ _ _ "Ref") = if o then
                                                 Just ("_" <> lower n <> capitalize k,("Maybe Ref", k))
                                            else
                                                 Just ("_" <> lower n <> capitalize k,("Ref",k))
          go (k,Schema _ o _ _ _ "ExternalRef") = if o then
                                                 Just ("_" <> lower n <> capitalize k,("Maybe ExternalRef", k))
                                            else
                                                 Just ("_" <> lower n <> capitalize k,("ExternalRef",k))
          go (k,Schema _ o _ _ _ "String") = if o then
                                                 Just ("_" <> lower n <> capitalize k,("Maybe String", k))
                                            else
                                                 Just ("_" <> lower n <> capitalize k,("String",k))
          go (k,Schema _ o _ _ _ "Long") = if o then
                                                 Just ("_" <> lower n <> capitalize k,("Maybe Int64", k))
                                            else
                                                 Just ("_" <> lower n <> capitalize k,("Int64",k))
          go (k,Schema _ o _ _ _ "URI") = if o then
                                                 Just ("_" <> lower n <> capitalize k,("Maybe Uri", k))
                                            else
                                                 Just ("_" <> lower n <> capitalize k,("Uri",k))

capitalize :: String -> String
capitalize s = toUpper (head s):tail s

lower :: String -> String
lower s = toLower (head s):tail s

generateRefApi :: Manager -> FilePath -> ([String], Ref) -> IO ()
generateRefApi = undefined
