{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen = replicateM 7 $ randomElement alphaNum

-- Persist URI to Redis
saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

-- Maybe get the shorten url
getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

-- Web functions
linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack $ show resp
            , " shorty is: ", TL.pack $ linkShorty shawty
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"] 

shortyDuplicated :: TL.Text -> TL.Text
shortyDuplicated shorty =
  TL.concat [ "Shorty "
            , shorty 
            , " is duplicated! Please try again."
            ]

utf8 :: BC.ByteString -> TL.Text
utf8 = TL.fromStrict . decodeUtf8

-- Web app
app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    case parseURI (TL.unpack uri) of
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
        exists <- liftIO $ getURI rConn shorty -- Verifying existence of the shorty generated
        case exists of
          Left reply -> text $ TL.pack $ show reply
          Right dupl -> case dupl of
            Just bs -> 
              text $ shortyDuplicated $ utf8 shorty
            Nothing -> do
              let uri' = encodeUtf8 $ TL.toStrict uri
              persist <- liftIO $ saveURI rConn shorty uri'             
              case persist of
                Left err   -> text $ TL.pack $ show err
                Right resp -> html $ shortyCreated resp shawty
      Nothing -> text $ shortyAintUri uri
  get "/:short" $ do
    short <- param "short"
    uri   <- liftIO $ getURI rConn short
    case uri of
      Left reply -> text $ TL.pack $ show reply
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html $ shortyFound $ utf8 bs

-- Main function
main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 $ app rConn
