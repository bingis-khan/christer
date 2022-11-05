{-# LANGUAGE OverloadedLabels, NamedFieldPuns, DeriveGeneric, InstanceSigs #-}
module Chat (chatApp) where

import qualified Table as T

import Network.WebSockets hiding (Message)
import Data.Set (Set)
import qualified Data.Set as S
import Auth (Account (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Servant (Handler, throwError)
import GHC.Conc (TVar)
import Data.Aeson (decode, encode)
import Data.Aeson.Types (FromJSON)
import GHC.Generics (Generic)
import Control.Monad (forever, when, unless)
import GHC.Conc.Sync (atomically)
import Control.Concurrent.STM (modifyTVar)
import Database.Selda.SQLite (withSQLite)
import Database.Selda (def, ID, toId, insert_, fromId, query, select, (.==), (!), literal, restrict, aggregate, (.&&), not_, just, (.||))
import Data.Text (Text)
import Data.Int (Int64)
import Data.Time (getCurrentTime)
import Servant (err500)
import Control.Concurrent.STM (readTVarIO)
import Data.Foldable (find)
import Data.Aeson (ToJSON)
import Data.Maybe (listToMaybe)
import Database.Selda (count)
import Table (relations, Relation(..), Verdict (..))


data Client = Client Account Connection
instance Eq Client where
  (==) :: Client -> Client -> Bool
  Client acc _ == Client acc' _ = acc == acc'

instance Ord Client where
  compare :: Client -> Client -> Ordering
  Client acc _ `compare` Client acc' _ = acc `compare` acc'

type ConnectedClients = Set Client


data IncomingMessage = IncomingMessage 
  { to :: Int64
  , imContent :: Text
  } deriving Generic

instance FromJSON IncomingMessage

data OutgoingMessage = OutgoingMessage
  { from :: Int64
  , ogContent :: Text
  } deriving Generic

instance ToJSON OutgoingMessage


type App = ReaderT FilePath Handler

addClient :: TVar ConnectedClients -> Account -> Connection -> IO ()
addClient clients acc conn =
  let client = Client acc conn
  in atomically $ modifyTVar clients (S.insert client)

addMessageToDB :: FilePath -> Account -> IncomingMessage -> IO ()
addMessageToDB path (Account { person }) (IncomingMessage { to, imContent = content }) = do
  now <- getCurrentTime
  let dbMsg = T.Message { T.msgID = def, T.from = person, T.to = toId to, T.content = content, T.sendDate = now }
  withSQLite path $ insert_ T.messages [dbMsg]


incomingToOutgoing :: Account -> IncomingMessage -> OutgoingMessage
incomingToOutgoing Account { person } IncomingMessage { imContent } = OutgoingMessage { from = fromId person, ogContent = imContent }

sendToRecipient :: TVar ConnectedClients -> Account -> IncomingMessage -> IO ()
sendToRecipient clients acc imsg = do
  clients' <- readTVarIO clients
  let mClient = find (\(Client acc' _) -> acc == acc') $ S.toList clients'
  case mClient of
    Nothing -> return ()
    Just (Client _ conn) -> do
      let omsg = incomingToOutgoing acc imsg
          encoded = encode omsg
      sendTextData conn encoded
  

checkIfMessageCanBeSent :: FilePath -> Account -> IncomingMessage -> IO Bool
checkIfMessageCanBeSent path Account { person } IncomingMessage { to } = withSQLite path $ do
  let id = literal $ toId to
  fmap head $ query $ do 
    c <- aggregate $ do
      r <- select relations
      restrict
          $ (r ! #thisPerson .== literal person .&& r ! #concerning .== id .|| r ! #thisPerson .== id .&& r ! #concerning .== literal person)
        .&& not_ (r ! #blocked)
        .&& r ! #verdict .== just (literal Yay)

      return $ count $ r ! #relationID

    return $ c .== 2


chatApp :: TVar ConnectedClients -> Account -> Connection -> App ()
chatApp clients acc conn = ask >>= \path -> do
  succeeded <- liftIO $ do
    addClient clients acc conn
    withPingThread conn 30 (return ()) $ forever $ do  -- Setup connection.
      mmsg <- decode <$> receiveData conn
      case mmsg of
        Nothing -> return False
        Just msg -> do
          canBeSent <- checkIfMessageCanBeSent path acc msg
          if not canBeSent
            then return False
            else do
              addMessageToDB path acc msg
              sendToRecipient clients acc msg
              return True

  unless succeeded $ throwError err500
