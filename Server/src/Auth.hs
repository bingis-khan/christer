{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators, NamedFieldPuns, LambdaCase, OverloadedLabels, DeriveGeneric #-}
module Auth (Auth, serverContext, Account(..), RegisterError(..), register) where

import Servant (BasicAuthResult(Authorized, Unauthorized), Context(EmptyContext), BasicAuth, BasicAuthCheck(..), BasicAuthData (BasicAuthData), Context ((:.)))
import Database.Selda (ID, toId, SeldaM, insert_, def, query, select, limit, (!), type (:*:) ((:*:)), restrict, (.==), literal, suchThat, toLower)
import Table (Person (..), Account(..), lambsToTheSlaughter, personalDataToPerson)
import Data.Text (Text)
import qualified Data.Text as Text
import Types.Types (RegisterProfile(RegisterProfile), PersonalData' (PersonalData))
import qualified Types.Types as T
import Data.List.NonEmpty (NonEmpty)
import Data.Either.Validation (Validation (Failure, Success))
import Data.ByteString (ByteString)
import Data.Functor ((<&>), ($>))
import Control.Applicative ((<**>), liftA2)
import Data.Functor.Identity (runIdentity)
import Crypto.Hash.SHA256 (hash)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Char (isSpace)
import Text.Email.Validate (isValid)
import Database.Selda.SQLite (withSQLite)
import Data.Maybe (listToMaybe)
import Servant.Server (BasicAuthResult(..))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)


type Auth = BasicAuth "bobby-auth" Account 



data RegisterError
  = EmailAlreadyRegistered
  | InvalidEmail
  | EmailTooLong
  | PasswordTooShort
  | PasswordTooLong
  | PasswordContainsInvalidCharacters
  deriving Generic

instance ToJSON RegisterError


type Validate a = Validation (NonEmpty RegisterError) a
newtype UCEmail = UncheckedEmail { fromUCEmail :: Text }
newtype Email = CheckedEmail { fromEmail :: Text }

newtype UCPassword = UncheckedPassword { fromUCPassword :: Text }
newtype Password = HashedPassword { fromPassword :: ByteString }


-- meh, kinda bad, because we're not using the returned email for anything anyway.
checkEmailAvailability :: UCEmail -> SeldaM b (Validate UCEmail)
checkEmailAvailability (UncheckedEmail email) = do
  let ciEmail = toLower (literal email)
  mEmail <- fmap listToMaybe $ query $ limit 0 1 $ select lambsToTheSlaughter `suchThat` \p -> toLower (p ! #email) .== ciEmail
  return $ case mEmail of
    Just _ -> Failure $ pure EmailAlreadyRegistered
    Nothing -> Success $ UncheckedEmail email

validateEmailStructure :: UCEmail -> Validate Email
validateEmailStructure (UncheckedEmail email)
  | isValid (encodeUtf8 email) = Success $ CheckedEmail email
  | otherwise = Failure $ pure InvalidEmail

checkIfEmailIsTooLong :: UCEmail -> Validate UCEmail
checkIfEmailIsTooLong (UncheckedEmail email)
  | Text.length email > 256 = Failure $ pure EmailTooLong
  | otherwise = Success $ UncheckedEmail email

validateEmail :: UCEmail -> SeldaM b (Validate Email)
validateEmail email = fmap ((validateEmailStructure email <*) . (checkIfEmailIsTooLong email <*)) (checkEmailAvailability email) 


validatePasswordLength :: UCPassword -> Validate UCPassword
validatePasswordLength (UncheckedPassword pass)
  | Text.length pass < 8 = Failure $ pure PasswordTooShort
  | Text.length pass > 256 = Failure $ pure PasswordTooLong
  | otherwise = Success $ UncheckedPassword pass

checkForInvalidCharacters :: UCPassword -> Validate UCPassword
checkForInvalidCharacters (UncheckedPassword pass)
  | Text.any isSpace pass = Failure $ pure PasswordContainsInvalidCharacters
  | otherwise = Success $ UncheckedPassword pass

-- No salting, because I don't feel like it c:
hashPassword :: UCPassword -> Password
hashPassword = HashedPassword . hash . encodeUtf8 . fromUCPassword

validatePassword :: UCPassword -> Validate Password
validatePassword pass = validatePasswordLength pass *> checkForInvalidCharacters pass <&> hashPassword


validate :: RegisterProfile -> SeldaM b (Validate Person)
validate RegisterProfile { T.email, T.password, T.personal } = do
  vEmail <- validateEmail $ UncheckedEmail email
  let vPassword = validatePassword $ UncheckedPassword password
  
  return $ liftA2 (personalDataToPerson personal) (fromEmail <$> vEmail) (fromPassword <$> vPassword)


register :: RegisterProfile -> SeldaM b (Maybe (NonEmpty RegisterError))
register rp = validate rp >>= \case
  Failure res -> return $ Just res
  Success p -> insert_ lambsToTheSlaughter [p] >> return Nothing


login :: FilePath -> Email -> Password -> IO (BasicAuthResult Account)
login db (CheckedEmail email) (HashedPassword password) = withSQLite db $ do
  user' <- fmap listToMaybe $ query $ limit 0 1 $ do
    p <- select lambsToTheSlaughter
    restrict $ p ! #email .== literal email
    return $ p ! #pid :*: p ! #password
  return $ case user' of
    Nothing -> NoSuchUser
    Just (personID :*: userPassword)
      | userPassword /= password -> BadPassword
      | otherwise -> Authorized $ Account { person = personID }

authUser :: FilePath -> BasicAuthCheck Account
authUser db = BasicAuthCheck $ \(BasicAuthData email' password') -> 
  let email = CheckedEmail $ decodeUtf8 email'
      password = hashPassword $ UncheckedPassword $ decodeUtf8 password' 
  in login db email password

serverContext :: FilePath -> Context '[BasicAuthCheck Account]
serverContext db = authUser db :. EmptyContext
