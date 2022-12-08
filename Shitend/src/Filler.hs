{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main (main) where

import System.Environment (getArgs)
import Data.Set (Set)
import qualified Data.Set as S
import API (API, MainAPI)
import Data.Data (Proxy(Proxy))
import Servant
import Types.Types (PersonalData(PersonalData), RegisterProfile (..), firstName, Gender (DeVito, Male, Female), Race (..))
import Servant.Client (client, ClientM, runClientM, Scheme (Http))
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager, parseRequest, httpLbs, applyBasicAuth)
import Control.Monad.IO.Class (liftIO)
import Servant.Client.Internal.HttpClient (mkClientEnv)
import Servant.Client.Streaming (BaseUrl(..))
import Control.Monad (void)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileSource)
import System.Random.Stateful
import Data.Time (TimeZone)
import Data.Time.LocalTime
import Data.Time.Clock (NominalDiffTime)
import Data.Bifunctor (first)
import Data.Time (diffUTCTime)
import Data.Time (addUTCTime)
import Data.Time (UTCTime)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import Data.Functor ((<&>))



api :: Proxy MainAPI
api = Proxy


register
  :<|>  edit
  :<|>  current
  :<|>  decide
  :<|>  contacts
  :<|>  messages = client api


names :: [Text]
names =
  [ "bob"
  , "kyle"
  , "tim"
  ]

genders :: [Gender]
genders =
  [ Male
  , Female
  , DeVito
  ]

races :: [Race]
races =
  [ Black
  , Caucasian
  , Asian
  , Polish
  ]


type UID = Int

randomElem :: [a] -> IO a
randomElem xs = do
  let len = length xs
  i <- randomRIO (0, len - 1)
  return $ xs !! i


randomSeconds :: NominalDiffTime -> IO NominalDiffTime
randomSeconds max = realToFrac <$> randomRIO (0, truncate max :: Int)

diff :: TimeZone -> LocalTime -> LocalTime -> NominalDiffTime
diff tz st en = diffUTCTime (localTimeToUTC tz st) (localTimeToUTC tz en)

randomDateBetween :: TimeZone -> LocalTime -> LocalTime -> IO LocalTime
randomDateBetween tz s e =
  fmap
    (utcToLocalTime tz . (`addUTCTime` (localTimeToUTC tz s)))
    (randomSeconds (diff tz s e))

randomBirthday :: IO UTCTime
randomBirthday = do
  tz <- getCurrentTimeZone
  nowUTC <- getCurrentTime
  let yesterday = utcToLocalTime tz $ addUTCTime (-1 * 3600 * 24 * 365 * 30) nowUTC
  
  date <- randomDateBetween tz yesterday (utcToLocalTime tz nowUTC)
  return $ localTimeToUTC tz date

makeProfile :: UID -> IO (RegisterProfile, PersonalData)
makeProfile uid = do
  firstName <- randomElem names <&> (<> T.pack (show uid))
  let lastName = T.reverse firstName 
  gender <- randomElem genders
  birthdate <- randomBirthday
  height <- randomRIO (100, 200 :: Int)
  let description = firstName <> "'s description."
  race <- randomElem races
  

  let email = firstName <> "@bob.bob"
  let password = "bob"
  let rp = RegisterProfile email password
  return (rp, PersonalData (Just firstName) (Just lastName) (Just gender) (Just birthdate) (Just height) (Just description) (Just race))


addSingleUser :: Manager -> UID -> ClientM Text
addSingleUser mgr uid = do
  (rp, pd) <- liftIO $ makeProfile uid
  void $ register rp

  let auth = BasicAuthData (encodeUtf8 $ email rp) (encodeUtf8 $ password rp)
  void $ edit auth pd
  liftIO $ uploadRandomPic mgr auth
  return $ fromJust $ firstName pd
  



uploadRandomPic :: Manager -> BasicAuthData -> IO ()
uploadRandomPic = uploadPic "./img/ted.jpg"

uploadPic :: FilePath -> Manager -> BasicAuthData -> IO ()
uploadPic path mgr BasicAuthData { basicAuthUsername, basicAuthPassword } = do
  let form = [partFileSource "file" path]
  req <- parseRequest "http://localhost:8080/post-pic"
  resp <- flip httpLbs mgr  =<< formDataBody form (applyBasicAuth basicAuthUsername basicAuthPassword req)
  return ()

main :: IO ()
main = do
  [sAmount] <- getArgs
  let amount = read sAmount :: Int
  

  manager' <- newManager defaultManagerSettings
  res <- runClientM (traverse (addSingleUser manager') [1..amount]) (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right names -> print names
