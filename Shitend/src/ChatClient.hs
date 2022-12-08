{-# LANGUAGE OverloadedStrings #-}

-- Has to be "Main" to be an executable. wat
module Main (main) where

import System.Environment (getArgs)
import Network.Socket (withSocketsDo)
import Network.WebSockets (runClientWith, defaultConnectionOptions, Headers, sendTextData, receiveData)

import Data.ByteString.Base64 (encodeBase64')
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Functor ((<&>))
import Control.Monad.IO.Class (liftIO)
import Data.String (IsString(fromString))
import Network.WebSockets.Client (runClient)


credentialsToHeader :: ByteString -> ByteString -> Headers
credentialsToHeader user pass =
  let base64d = encodeBase64' $ user <> ":" <> pass
      headerValue = "Basic " <> base64d
  in [("Authorization", headerValue)]


prepareArgs :: IO (ByteString, ByteString, Int64)
prepareArgs = getArgs <&>
  \[user, pass, other] -> (fromString user, fromString pass, read other)

main :: IO ()
main = do
  --print name
  --print pass

  withSocketsDo $ runClient "localhost" 8080 "" $ \conn -> do
    sendTextData conn ("borbiks" :: ByteString)
    print =<< (receiveData conn :: IO ByteString)
