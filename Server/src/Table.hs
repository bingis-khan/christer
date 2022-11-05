{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, NamedFieldPuns, DeriveGeneric, TypeOperators #-}
module Table where

import Database.Selda
import Database.Selda.SQLite (withSQLite)
import Data.ByteString (ByteString)
import Servant (FromHttpApiData)
import Types.Types (Race, Gender, DecisionResult (..), ContactDigest (ContactDigest), PersonalData, PersonalData' (PersonalData))
import qualified Types.Types as T
import Types.Suggestion ( Suggestion(Suggestion) )
import qualified Types.Suggestion as S
import Data.Maybe (listToMaybe, fromJust)
import Control.Monad (void, when)
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import Data.Time (diffUTCTime)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import Data.Time (getCurrentTime)
import Database.Selda.Nullable ((?!), (?==))
import Control.Monad.Extra (whenJust)
import qualified Data.Set as Set
import Data.List (find)





-- Tables
data Person = Person
  { pid :: ID Person

  -- Login data
  , email :: Text
  , password :: ByteString

  -- Personal data
  , firstName :: Text
  , lastName :: Text
  , dateOfBirth :: UTCTime
  , height :: Int  -- In cm.
  , race :: Race
  , gender :: Gender
  , description :: Text

  -- Maybe move it later to another one-to-one table, like 'Preferences' to make this table rarely mutable.
  , currentMatch :: Maybe (ID Person)
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

data Relation = Relation
  { relationID :: ID Relation

  , thisPerson :: ID Person
  , concerning :: ID Person

  , verdict :: Maybe Verdict
  , blocked :: Bool
  } deriving Generic

instance SqlRow Relation


relations :: Table Relation
relations = table "relations" [#relationID :- autoPrimary]


-- Used for state.
data Account = Account
  { person :: ID Person
  } deriving (Eq, Ord)



initDB :: FilePath -> IO ()
initDB path = withSQLite path $ do
  tryCreateTable lambsToTheSlaughter 
  tryCreateTable messages
  tryCreateTable relations


candidate :: MonadSelda m => Account -> m (Maybe Person)
candidate Account { person } = do
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
          (   r ! #blocked
          .|| r ! #verdict .== just (literal Nay)
          )
      return (count (r ! #relationID) :*: pee)

    restrict $ c .> 0  -- I don't think count is displayed when it's 0 (or there's 0 record), but wahtever.
    return pee

  allPeople <- query $ do
    p <- select lambsToTheSlaughter
    restrict $ p ! #pid ./= literal person
    return p

  return $ find (\p -> pid p `notElem` badPeeps) allPeople

personalDataToPerson :: PersonalData -> Text -> ByteString -> Person
personalDataToPerson PersonalData { T.firstName, T.lastName, T.gender, T.dateOfBirth, T.height, T.description, T.race } email pass = 
  let i = runIdentity
  in Person
  { pid = def
  , email = email
  , password = pass

  , firstName = i firstName
  , lastName = i lastName
  , height = i height
  , gender = i gender
  , dateOfBirth = i dateOfBirth
  , description = i description
  , race = i race

  , currentMatch = Nothing
  }

personToPersonalData :: Person -> PersonalData
personToPersonalData Person { firstName, lastName, height, gender, dateOfBirth, description, race } = 
  let i = pure
  in PersonalData
  { T.firstName = i firstName
  , T.lastName = i lastName
  , T.height = i height
  , T.gender = i gender
  , T.dateOfBirth = i dateOfBirth
  , T.description = i description
  , T.race = i race
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
  { firstName
  , lastName
  , height
  , race
  , description
  , dateOfBirth
  } = Suggestion
    { S.firstName = firstName
    , S.lastName = lastName
    , S.age = diffToAge dateOfBirth now
    , S.height = height
    , S.race = race
    , S.description = description
    }


updatePerson :: MonadSelda m => ID Person -> (Row (Backend m) Person -> Row (Backend m) Person) -> m ()
updatePerson pid = update_ lambsToTheSlaughter (\p -> p ! #pid .== literal pid)

findMatch :: Account -> SeldaM b (Maybe Suggestion)
findMatch acc = do
  u <- getYourself acc

  mPerson <- case currentMatch u of
    Just p -> do
      Just <$> findPerson p
    Nothing -> do
      mPerson <- candidate acc
      
      whenJust mPerson $ \p -> do
          updatePerson (pid u) (`with` [#currentMatch := just (literal (pid p))])
      
      return mPerson

  
  now <- liftIO getCurrentTime 
  return $ fmap (personToSuggestion now) mPerson


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


addModifyRelation :: (MonadSelda m, MonadMask m) => Account -> ID Person -> (Relation -> Relation) -> (Row (Backend m) Relation -> Row (Backend m) Relation) -> m ()
addModifyRelation Account { person } other f f' = 
  void $ upsert relations 
    (\r -> r ! #thisPerson .== literal person .&& r ! #concerning .== literal other) 
    f'
    [f $ Relation { relationID = def, thisPerson = person, concerning = other, blocked = False, verdict = Nothing  }]



otherLikesYou :: MonadSelda m => Account -> ID Person -> m Bool
otherLikesYou Account { person } other = do
  res <- query $ limit 0 1 $ do
    r <- select relations
    restrict $ r ! #thisPerson .== literal other .&& r ! #concerning .== literal person .&& r ! #verdict .== just (literal Yay)
    return $ r ! #relationID

  -- If there's more than zero results, cool. Should not be more than 1, tho, boi.
  return $ not $ null res

-- Returns false if there are no matches.
decideMatch :: Account -> Verdict -> SeldaM b DecisionResult
decideMatch acc verdict = do
  u <- getYourself acc
  case currentMatch u of
    Nothing -> return NoBitches
    Just other -> do
      -- Add relation.
      addModifyRelation acc other (\r -> r { verdict = Just verdict }) (`with` [#verdict := just (literal verdict)])
      
      -- Remove current match.
      updatePerson (pid u) (`with` [#currentMatch := null_])

      -- We can check here if the other person "likes" you too and notify the boi.
      otherLikesYou acc other <&> \case 
        True -> OtherYayd
        False -> AllGood


mutualLikes :: Account -> Query s (Row s Person :*: Col s Bool)
mutualLikes Account { person } = do
  -- People that you like.
  r <- select relations `suchThat` \r -> r ! #thisPerson .== literal person .&& r ! #verdict .== just (literal Yay)
  
  -- People that like you.
  r' <- select relations `suchThat` \r' -> r' ! #concerning .== literal person .&& r' ! #verdict .== just (literal Yay)
  
  -- Ensure it's mutual.
  restrict $ r ! #concerning .== r' ! #thisPerson
  
  -- Add personal info.
  p <- innerJoin 
    (\p -> p ! #pid .== r ! #concerning)
    (select lambsToTheSlaughter)

  let blocked = r ! #blocked .|| r' ! #blocked
  return $ p :*: blocked

lastMessage :: Col s (ID Person) -> Col s (ID Person) -> Query s (Row s Message)
lastMessage p p' = innerJoin (\m -> m ! #from .== p .&& m ! #to .== p' .|| m ! #from .== p' .&& m ! #to .== p) $ limit 0 1 $ do
  m <- select messages
  order (m ! #sendDate) Desc
  return m

findContacts :: MonadSelda m => Account -> m [ContactDigest]
findContacts acc@Account { person = u } = do
  xs <- query $ do
    (p :*: b) <- mutualLikes acc
    m <- lastMessage (literal u) (p ! #pid)
    return $ p :*: b :*: m
  return $ flip map xs $ \(p :*: b :*: m) -> ContactDigest
    { T.sender = fromId (pid p)
    , T.lastMessage = content m
    , T.lastMessageDate = sendDate m
    , T.block = b
    , T.senderData = personToPersonalData p
    }
