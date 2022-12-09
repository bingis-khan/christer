{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, NamedFieldPuns, DeriveGeneric, TypeOperators #-}
module Table where

import Database.Selda
import Database.Selda.SQLite (withSQLite)
import Data.ByteString (ByteString)
import Types.Types (Race, Gender, ContactDigest (ContactDigest), PersonalData(PersonalData))
import Servant
import Text.Read (readMaybe)
import Data.Text (unpack)
import qualified Data.ByteString.Lazy as LBS
import qualified Types.Types as T
import Data.Functor.Identity (runIdentity)
import Data.Int (Int64)
import Data.Maybe (listToMaybe, fromJust, fromMaybe)
import qualified Data.Set as Set
import Types.Suggestion (Suggestion(Suggestion))
import qualified Types.Suggestion as S
import Data.List (find)
import Data.Time (getCurrentTime)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import Control.Monad (void)
import Data.Text (pack)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)

data OMessage = OMessage
  { ocontent :: Text
  , outgoing :: Bool
  , osendDate :: UTCTime
  } deriving Generic
instance ToJSON OMessage
instance FromJSON OMessage


-- Tables
data Person = Person
  { pid :: ID Person

  -- Login data
  , email :: Text
  , password :: ByteString

  -- Personal data
  , firstName :: Maybe Text
  , lastName :: Maybe Text
  , dateOfBirth :: Maybe UTCTime
  , height :: Maybe Int  -- In cm.
  , race :: Maybe Race
  , gender :: Maybe Gender
  , description :: Text
  } deriving Generic

instance SqlRow Person

lambsToTheSlaughter :: Table Person
lambsToTheSlaughter = table "people" [#pid :- autoPrimary, #email :- unique] 


data Message = Message
  { msgID :: ID Message
  , from :: ID Person
  , to :: ID Person

  , content :: Text
  , sendDate :: UTCTime
  } deriving Generic

instance SqlRow Message

messages :: Table Message
messages = table "messages" [#msgID :- autoPrimary]


data Verdict = Yay | Nay 
  deriving (Bounded, Enum, Show, Read, Generic)

instance SqlType Verdict
instance FromHttpApiData Verdict where
  parseQueryParam s = case readMaybe (unpack s) of
    Just v -> Right v
    Nothing -> Left "Invalid verdict value."
instance ToHttpApiData Verdict where
  toQueryParam = pack . show


data Relation = Relation
  { relationID :: ID Relation

  , thisPerson :: ID Person
  , concerning :: ID Person

  , verdict :: Maybe Verdict
  } deriving Generic

instance SqlRow Relation

relations :: Table Relation
relations = table "relations" [#relationID :- autoPrimary]


data Pic = Pic
  { picID :: ID Pic
  , picOf :: ID Person
  , picMIME :: Text
  , picData :: LBS.ByteString
  } deriving Generic

instance SqlRow Pic

pics :: Table Pic
pics = table "pics" [#picID :- autoPrimary]




-- Used for state.
newtype Account = Account
  { person :: ID Person
  } deriving (Eq, Ord)


initDB :: FilePath -> IO ()
initDB path = withSQLite path $ do
  tryCreateTable lambsToTheSlaughter 
  tryCreateTable messages
  tryCreateTable relations
  tryCreateTable pics



personToPersonalData :: Person -> PersonalData
personToPersonalData Person { firstName, lastName, height, gender, dateOfBirth, description, race } = PersonalData
  { T.firstName = firstName
  , T.lastName = lastName
  , T.height = height
  , T.gender = gender
  , T.dateOfBirth = dateOfBirth
  , T.description = Just description
  , T.race = race
  }

personalDataToPerson :: PersonalData -> Text -> ByteString -> Person
personalDataToPerson PersonalData { T.firstName, T.lastName, T.gender, T.dateOfBirth, T.height, T.description, T.race } email pass = Person
  { pid = def
  , email = email
  , password = pass

  , firstName = firstName
  , lastName = lastName
  , height = height
  , gender = gender
  , dateOfBirth = dateOfBirth
  , description = fromMaybe mempty description
  , race = race
  }


diffToAge :: UTCTime -> UTCTime -> Int
diffToAge birth now =
  let greg = toGregorian . utctDay
      (by, bm, bd) = greg birth
      (y, m, d) = greg now 
      age' = y - by
  in if m < bm || m == bm && d < bd
    then fromIntegral age' - 1
    else fromIntegral age'

personToSuggestion :: UTCTime -> Person -> Suggestion
personToSuggestion now Person 
  { pid
  , firstName
  , lastName
  , height
  , race
  , description
  , dateOfBirth
  , gender
  } = Suggestion
    { S.userID = fromId pid
    , S.firstName = firstName
    , S.lastName = lastName
    , S.age = flip diffToAge now <$> dateOfBirth
    , S.height = height
    , S.race = race
    , S.description = description
    , S.gender = gender
    }


candidates :: MonadSelda m => Account -> m [Person]
candidates Account { person } = do
  -- People you *can't* match.
  badPeeps <- fmap Set.fromList $ query $ do
    (c :*: pee) <- aggregate $ do
      p <- select lambsToTheSlaughter
      let otherID = p ! #pid
      pee <- groupBy (p ! #pid)
      r <- select relations
      restrict 
        $   (r ! #thisPerson .== literal person .&& r ! #concerning .== otherID .|| r ! #thisPerson .== otherID .&& r ! #concerning .== literal person)
        .&& 
          (  r ! #verdict .== just (literal Nay)
          )
      return (count (r ! #relationID) :*: pee)

    restrict $ c .> 0  -- I don't think count is displayed when it's 0 (or there's 0 record), but wahtever.
    return pee

  allPeople <- query $ do
    p <- select lambsToTheSlaughter
    restrict $ p ! #pid ./= literal person
    return p

  return $ filter (\p -> pid p `notElem` badPeeps) allPeople

findMatches :: Account -> SeldaM b [Suggestion]
findMatches acc = do
  people <- candidates acc
  
  now <- liftIO getCurrentTime 
  return $ map (personToSuggestion now) people


editProfile :: Account -> PersonalData -> SeldaM b ()
editProfile acc pd = do
  u <- getYourself acc
  let newPerson = merge pd u
  updatePerson (pid u) (const (row newPerson))


merge :: PersonalData -> Person -> Person
merge PersonalData { T.firstName = pdfn, T.lastName = pdln, T.dateOfBirth = pddob, T.height = pdh, T.race = pdr, T.gender = pdg, T.description = pdd } Person { pid, email, password, firstName, lastName, dateOfBirth, height, race, gender, description } =
  let 
    fm :: Maybe a -> Maybe a -> Maybe a
    fm _ (Just x) = Just x 
    fm x Nothing = x
  in Person
    { pid = pid

    -- Login data
    , email = email
    , password = password

    -- Personal data
    , firstName = fm firstName pdfn
    , lastName = fm lastName pdln
    , dateOfBirth = fm dateOfBirth pddob
    , height = fm height pdh
    , race = fm race pdr
    , gender = fm gender pdg
    , description = fromMaybe description pdd
    }

getYourself :: MonadSelda m => Account -> m Person
-- before you check yourself
getYourself Account { person } = findPerson person

findPerson :: MonadSelda m => ID Person -> m Person 
findPerson person = do
  mPerson <- fmap listToMaybe $ query $ limit 0 1 $ do
    p <- select lambsToTheSlaughter
    restrict $ p ! #pid .== literal person
    return p
  return $ fromJust mPerson  -- We assume account is always valid.

-- Returns false if there are no matches.
decideMatch :: Account -> Int64 -> Verdict -> SeldaM b ()
decideMatch acc otheri verdict = do
  let other = toId otheri :: ID Person
  addModifyRelation acc other (\r -> r { verdict = Just verdict }) (`with` [#verdict := just (literal verdict)])

  return ()


updatePerson :: MonadSelda m => ID Person -> (Row (Backend m) Person -> Row (Backend m) Person) -> m ()
updatePerson pid = update_ lambsToTheSlaughter (\p -> p ! #pid .== literal pid)

addModifyRelation :: (MonadSelda m, MonadMask m) => Account -> ID Person -> (Relation -> Relation) -> (Row (Backend m) Relation -> Row (Backend m) Relation) -> m ()
addModifyRelation Account { person } other f f' = 
  void $ upsert relations 
    (\r -> r ! #thisPerson .== literal person .&& r ! #concerning .== literal other) 
    f'
    [f $ Relation { relationID = def, thisPerson = person, concerning = other, verdict = Nothing  }]


lastMessage :: Col s (ID Person) -> Col s (ID Person) -> Query s (Row s Message)
lastMessage p p' = innerJoin (\m -> m ! #from .== p .&& m ! #to .== p' .|| m ! #from .== p' .&& m ! #to .== p) $ limit 0 1 $ do
  m <- select messages
  order (m ! #sendDate) Desc
  return m

mutualLikes :: Account -> Query s (Row s Person)
mutualLikes Account { person } = do
  -- People that you like.
  r <- select relations `suchThat` \r -> r ! #thisPerson .== literal person .&& r ! #verdict .== just (literal Yay)
  
  -- People that like you.
  r' <- select relations `suchThat` \r' -> r' ! #concerning .== literal person .&& r' ! #verdict .== just (literal Yay)
  
  -- Ensure it's mutual.
  restrict $ r ! #concerning .== r' ! #thisPerson
  
  -- Add personal info.
  innerJoin 
    (\p -> p ! #pid .== r ! #concerning)
    (select lambsToTheSlaughter)


findContacts :: MonadSelda m => Account -> m [ContactDigest]
findContacts acc@Account { person = u } = do
  xs <- query $ do
    --p <- mutualLikes acc
    p <- select lambsToTheSlaughter
    restrict $ p ! #pid ./= literal u
    return p
  
  now <- liftIO getCurrentTime
  return $ flip map xs $ \(p) -> ContactDigest
    { T.sender = fromId (pid p)
    , T.lastMessage = mempty
    , T.lastMessageDate = now
    , T.senderData = personToPersonalData p
    }

getMessages :: MonadSelda m => Account -> Int64 -> m [OMessage]
getMessages acc@Account { person = u } other = do
  let lu = literal u
      lo = literal (toId other :: ID Person)
  msgs <- query $ do
    m <- select messages
    restrict $ m ! #from .== lu .&& m ! #to .== lo .|| m ! #from .== lu .&& m ! #to .== lo
    return m
  
  return $ map (\m -> OMessage (content m) (Table.from m == u) (sendDate m) ) msgs

-- Pictures
addPic :: (MonadSelda m, MonadMask m) => Account -> Text -> LBS.ByteString -> m ()
addPic Account { person } mime content = transaction $ do
  -- First, remove existing picture.
  deleteFrom_ pics $ \r -> r ! #picOf .== literal person
  
  -- Then, save dat nigga
  insert_ pics [Pic { picID = def, picOf = person, picMIME = mime, picData = content }]

getPic :: MonadSelda m => Int64 -> m (Maybe (Text, LBS.ByteString))
getPic iPersonID =
  let personId = toId iPersonID
  in fmap (fmap (\(mime :*: content) -> (mime, content)) . listToMaybe) $ query $ limit 0 1 $ do
    pic <- select pics
    restrict $ pic ! #picOf .== literal personId
    return $ pic ! #picMIME :*: pic ! #picData

getOwnPic :: MonadSelda m => Account -> m (Maybe (Text, LBS.ByteString))
getOwnPic acc = do
  u <- getYourself acc
  getPic (fromId $ pid u)
