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


data Gender = Male | Female | DeVito
  deriving (Bounded, Enum, Show, Read, Generic)

instance ToJSON Gender
instance FromJSON Gender
instance SqlType Gender


data PersonalData = PersonalData
  { firstName :: Maybe Text
  , lastName :: Maybe Text
  , gender :: Maybe Gender
  , dateOfBirth :: Maybe UTCTime
  , height :: Maybe Int
  , description :: Text
  , race :: Maybe Race
  } deriving Generic

instance FromJSON PersonalData
instance ToJSON PersonalData


data RegisterProfile = RegisterProfile
  { email :: Text
  , password :: Text
  } deriving Generic

instance FromJSON RegisterProfile


data ContactDigest = ContactDigest
  { sender :: Int64  -- Person ID
  , senderData :: PersonalData
  , lastMessage :: Text
  , lastMessageDate :: UTCTime
  } deriving Generic

instance ToJSON ContactDigest
