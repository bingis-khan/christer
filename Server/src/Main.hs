{-# LANGUAGE DataKinds, TypeOperators, DeriveGeneric, OverloadedStrings, FlexibleInstances #-}
module Main (main) where

import Servant
import Auth (Auth, serverContext, Account, RegisterError)
import qualified Auth
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Data.Aeson.Types (FromJSON)
import Data.Text
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Database.Selda
import Table (initDB, Verdict, findMatch, decideMatch, findContacts, candidate, Person (pid))
import Servant.API.WebSocket (WebSocketPending, WebSocket)
import Chat (chatApp)
import qualified Network.WebSockets as WS
import Control.Monad (forever, (>=>))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Types.Types (RegisterProfile, OptionalData, DecisionResult (AllGood), ContactDigest)
import Types.Suggestion (Suggestion)
import qualified Types.Suggestion as S
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Auth (Account(..))
import Database.Selda.SQLite (withSQLite, SQLite)
import Data.List.NonEmpty (NonEmpty)
import Text.Read (readMaybe)
import Control.Exception (throw)


-- Monad for tha request context.
type App = ReaderT FilePath Handler




data Message = Message
  { content :: Text
  , incoming :: Bool
  , sendDate :: UTCTime
  } deriving Generic
instance ToJSON Message

-- Whole API
type API
  -- Setup
  =     "register" :> ReqBody '[JSON] RegisterProfile :> Post '[JSON] (Maybe (NonEmpty RegisterError))

  -- Edit data
  :<|>  Auth :> "edit" :> ReqBody '[JSON] OptionalData :> PutNoContent

  -- Matching
  :<|>  Auth :> "current" :> Get '[JSON] (Maybe Suggestion)
  
  -- There seems to be a bug with matching custom datatypes.
  :<|>  Auth :> "decide" :> QueryParam' '[Required] "verdict" Verdict :> Put '[JSON] DecisionResult

  -- Pics (fileserver)
  -- todo

  -- Messaging
  :<|>  Auth :> "contacts" :> Get '[JSON] [ContactDigest]
  :<|>  Auth :> "messages" :> Capture "recipient" Int64 :> Get '[JSON] [Message]


  -- fuck
  :<|>  "chat"  :> WebSocket




withDB :: SeldaM SQLite a -> App a
withDB action = ask >>= \path -> liftIO $ withSQLite path action 

register :: RegisterProfile -> App (Maybe (NonEmpty RegisterError))
register rp = withDB (Auth.register rp)

edit :: Account -> OptionalData -> App NoContent
edit acc od = withDB undefined

current :: Account -> App (Maybe Suggestion)
current acc = withDB $ findMatch acc

decide :: Account -> Verdict -> App DecisionResult
decide acc = withDB . decideMatch acc

contacts :: Account -> App [ContactDigest]
contacts acc = withDB $ findContacts acc

messages :: Account -> Int64 -> App [Message]
messages = undefined

server :: ServerT API App
server 
  =     register
  :<|>  edit
  :<|>  current
  :<|>  decide
  :<|>  contacts
  :<|>  messages
  :<|>  undefined

nt :: FilePath -> App a -> Handler a
nt fp = (`runReaderT` fp) 

startServer :: FilePath -> IO ()
startServer fp = run 8080 $ logStdoutDev $ serveWithContextT (Proxy :: Proxy API) (serverContext fp) (nt fp) server

main :: IO ()
main = do
  let db = "fuck.sqlite"
  
  initDB db
  --p <- withSQLite db $ candidate $ Account { person = toId 1 }
  --print $ fmap pid p
  startServer db
  
  -- WS.runServer "localhost" 8080 $ WS.acceptRequest >=> \conn -> forever $ do
  --   x <- WS.receiveData conn
  --   WS.sendTextData conn (x :: Text)
