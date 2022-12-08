{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Socket (withSocketsDo)
import Control.Exception (bracket)
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Data.Text.Encoding (encodeUtf8)


main :: IO ()
main = withSocketsDo $ do
  manager <- newManager defaultManagerSettings
  req <- parseRequest "http://localhost:8080/post-pic"
  resp <- flip httpLbs manager  =<< formDataBody form (applyBasicAuth "bob@bob.bob" "bobbobbob" req)
  print resp

  where form =
          [ partFileSource "file" "./img/ted.jpg"
          ]
