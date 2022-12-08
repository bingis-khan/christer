{-# LANGUAGE DataKinds, TypeOperators, DeriveGeneric #-}
module API where
import Servant
import Types.Types
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT)
import Auth (Auth, RegisterError)
import Types.Suggestion (Suggestion)
import Data.Int (Int64)
import Servant.Multipart
import qualified Data.ByteString.Lazy as LBS
import Table (Verdict)
import Data.List.NonEmpty (NonEmpty)
import Data.Aeson.Types (FromJSON)


-- Monad for tha request context.
type App = ReaderT FilePath Handler




data Message = Message
  { content :: Text
  , outgoing :: Bool
  , sendDate :: UTCTime
  } deriving Generic
instance ToJSON Message
instance FromJSON Message

-- Whole API
type API
  =     FileAPI
  :<|>  MainAPI


type MainAPI
  -- Setup
  =     "register" :> ReqBody '[JSON] RegisterProfile :> Post '[JSON] (Maybe (NonEmpty RegisterError))

  -- Edit data
  :<|>  Auth :> "edit" :> ReqBody '[JSON] PersonalData :> PutNoContent

  -- Matching
  :<|>  Auth :> "suggestions" :> Get '[JSON] [Suggestion]
  
  -- There seems to be a bug with matching custom datatypes.
  :<|>  Auth :> Capture "id" Int64 :> "decide" :> QueryParam' '[Required] "verdict" Verdict :> Put '[JSON] NoContent

  -- Messaging
  :<|>  Auth :> "contacts" :> Get '[JSON] [ContactDigest]
  :<|>  Auth :> "messages" :> Capture "recipient" Int64 :> Get '[JSON] [Message]


-- For some reason, no HasClient for multipart (despite importing these packages...)
-- So...
type FileAPI
  -- Pics (fileserver)
  =     Auth :> "post-pic" :> MultipartForm Mem (MultipartData Mem) :> PostNoContent
  :<|>  "pic" :> Capture "id" Int64 :> Get '[OctetStream] (Headers '[Header "Content-Type" Text] LBS.ByteString)
