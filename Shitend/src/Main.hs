{-# LANGUAGE DataKinds, TypeOperators, DeriveGeneric, OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Servant
import Auth (Auth, serverContext, Account, RegisterError)
import qualified Auth
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run, runSettings, setPort, defaultSettings)
import Data.Aeson.Types (FromJSON)
import Data.Text
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Database.Selda
import Table (initDB, Verdict, addPic, getPic, findContacts, findMatches, decideMatch, editProfile)
import Servant.API.WebSocket (WebSocketPending, WebSocket)

import qualified Network.WebSockets as WS
import Control.Monad (forever, (>=>))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Types.Types (RegisterProfile, ContactDigest, PersonalData)
import Types.Suggestion (Suggestion)
import qualified Types.Suggestion as S
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Auth (Account(..))
import Database.Selda.SQLite (withSQLite, SQLite)
import Data.List.NonEmpty (NonEmpty)
import Text.Read (readMaybe)
import Control.Exception (throw)
import Servant.Multipart
    ( MultipartOptions(..),
      FileData(fdPayload, fdFileCType),
      Mem,
      MultipartData(files, inputs) )
import Control.Monad.Extra (when)
import Control.Monad (unless)
import Network.Wai.Parse (noLimitParseRequestBodyOptions)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Control.Concurrent (forkIO)
import Control.Monad (void)
import API






withDB :: SeldaM SQLite a -> App a
withDB action = ask >>= \path -> liftIO $ withSQLite path action 

register :: RegisterProfile -> App (Maybe (NonEmpty RegisterError))
register rp = withDB (Auth.register rp)

edit :: Account -> PersonalData -> App NoContent
edit acc od = fmap (const NoContent) $ withDB $ editProfile acc od

current :: Account -> App [Suggestion]
current acc = withDB $ findMatches acc

decide :: Account -> Int64 -> Verdict -> App NoContent
decide acc who = withDB . fmap (const NoContent) . decideMatch acc who

postPic :: Account -> MultipartData Mem -> App NoContent
postPic acc mpd = do
  -- Bad, but I don't care.
  -- Get the only file, otherwise throw error.
  case files mpd of
    [pic] -> do
      -- Basic & bad validation
      let mime = fdFileCType pic
      liftIO $ print mime
      unless ("image/" `isPrefixOf` mime) $
        throw $ err400 { errBody = "Invalid MIME type." }

      let content = fdPayload pic
      withDB $ addPic acc mime content
      return NoContent
    [] -> throw $ err400 { errBody = "No files..." }
    _ -> throw $ err400 { errBody = "Only a single file allowed." }

pic :: Int64 -> App (Headers '[Header "Content-Type" Text] LBS.ByteString)
pic picId = withDB (getPic picId) >>= \case
  Nothing -> throw err404  -- Oh no, it's so bad.
  Just (mime, content) -> do
    return $ addHeader mime content

contacts :: Account -> App [ContactDigest]
contacts acc = withDB $ findContacts acc

messages :: Account -> Int64 -> App [Message]
messages = undefined

server :: ServerT API App
server 
  =     (postPic
  :<|>  pic)
  :<|>  register
  :<|>  edit
  :<|>  current
  :<|>  decide
  :<|>  contacts
  :<|>  messages

nt :: FilePath -> App a -> Handler a
nt fp = (`runReaderT` fp) 

myMultipartOptions :: MultipartOptions Mem
myMultipartOptions = MultipartOptions
  { generalOptions = noLimitParseRequestBodyOptions
  , backendOptions = ()
  }

startServer :: FilePath -> IO ()
startServer fp = run 8080 $ logStdoutDev $ serveWithContextT (Proxy :: Proxy API) (myMultipartOptions :. serverContext fp) (nt fp) server

main :: IO ()
main = do
  let db = "fuck.sqlite"
  
  initDB db


  -- void $ forkIO $ WS.runServer "localhost" 8080 $ WS.acceptRequest >=> \conn -> forever $ do
  --   x <- WS.receiveData conn
  --   WS.sendTextData conn (x :: Text)
  
  startServer db
  
  
