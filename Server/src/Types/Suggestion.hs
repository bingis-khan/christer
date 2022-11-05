{-# LANGUAGE DeriveGeneric #-}
module Types.Suggestion (Suggestion(..)) where

import Types.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)


-- It's bad. I just want names to be the same. (And I don't wnat to mess with the default instance.)
data Suggestion = Suggestion
  { firstName :: Text
  , lastName :: Text
  , age :: Int
  , height :: Int
  , race :: Race
  , description :: Text
  } deriving Generic

instance ToJSON Suggestion
