module Main where

import Representations
import FormatSchema
import Network.HTTP.Conduit (newManager,tlsManagerSettings)
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as BS (pack)

crestEndpoint = "https://crest.eveonline.com"

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
                let fmt = eitherDecodeStrict . BS.pack . _jsonDumpOfStructure . _acceptType $ selRep :: Either String Format
                print fmt


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
