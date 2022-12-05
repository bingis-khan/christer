{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Types.Types where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Selda (SqlType)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Int (Int64)



data Race = Caucasian | Black | Asian | Polish 
  deriving (Bounded, Enum, Show, Read, Generic)

instance ToJSON Race
instance SqlType Race
instance FromJSON Race


data Gender = Male | Female | FemaleLeftParenMaleRightParen | Genderlord
  deriving (Bounded, Enum, Show, Read, Generic)

instance ToJSON Gender
instance FromJSON Gender
instance SqlType Gender


data PersonalData' m = PersonalData
  { firstName :: m Text
  , lastName :: m Text
  , gender :: m Gender
  , dateOfBirth :: m UTCTime
  , height :: m Int
  , description :: m Text
  , race :: m Race
  } deriving Generic

type OptionalData = PersonalData' Maybe

instance FromJSON OptionalData


type PersonalData = PersonalData' Identity

instance FromJSON PersonalData
instance ToJSON PersonalData

data RegisterProfile = RegisterProfile
  { email :: Text
  , password :: Text

  , personal :: PersonalData
  } deriving Generic

instance FromJSON RegisterProfile


data DecisionResult
  = AllGood
  | OtherYayd
  | NoBitches
  deriving Generic

instance ToJSON DecisionResult


data ContactDigest = ContactDigest
  { sender :: Int64  -- Person ID
  , senderData :: PersonalData
  , lastMessage :: Text
  , lastMessageDate :: UTCTime
  , block :: Bool
  } deriving Generic

instance ToJSON ContactDigest
