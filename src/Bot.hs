{-# LANGUAGE OverloadedStrings #-}
module Bot (main) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Functor (void)
import Control.Exception
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode, status200)
import Data.Aeson (object, (.=), encode, decode, ToJSON, FromJSON)
import T3.Client
import Control.Monad.Reader

main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  sresp <- startRandom mgr creds
  let mi = srespMatchInfo sresp
  void $ play mgr creds (Loc 0 0) mi
  void $ play mgr creds (Loc 1 1) mi
  presp <- play mgr creds (Loc 2 2) mi
  BL.putStrLn (encode presp)

serverUrl :: String
serverUrl = "http://localhost:3000"

creds :: UserCreds
creds = UserCreds (UserName name) (UserKey key)
  where
    name = "username0"
    key = "SGIomWE7Xj5wl2I0rIiQpU5Blx4-LuCl"

startRandom :: Manager -> UserCreds -> IO StartResponse
startRandom mgr uc = do
  request <- postWithJson url (StartRequest uc)
  jsonResponseOrDie mgr request "Can't parse start response"
  where
    url = serverUrl ++ "/api/random"

play :: Manager -> UserCreds -> Loc -> MatchInfo -> IO PlayResponse
play mgr uc loc mi = do
  request <- postWithJson url (PlayRequest uc loc)
  responseOrDie mgr request "Can't parse play response"
  where
    url = mconcat [serverUrl ++ "/api/play/", T.unpack matchId, "/", T.unpack matchToken]
    (MatchInfo (MatchId matchId) (MatchToken matchToken)) = mi

postWithJson :: ToJSON a => String -> a -> IO Request
postWithJson url body = do
  req <- parseUrl url
  return $ req { method = "POST", requestBody = RequestBodyLBS (encode body) }

jsonResponseOrDie :: (FromJSON a) => Manager -> Request -> String -> IO a
jsonResponseOrDie mgr request err = do
  handle httpOrDie $ do
    response <- httpLbs request mgr
    case decode (responseBody response) of
      Nothing -> error err
      Just resp -> return resp

httpOrDie :: HttpException -> IO a
httpOrDie (StatusCodeException status _ _) = do
  putStrLn $ "Caught http exception with status: "  ++ show (statusCode status)
  error "die"
httpOrDie e = do
  putStrLn "Caught http exception"
  putStrLn (show e) 
  error "die"
