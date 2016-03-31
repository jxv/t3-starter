{-# LANGUAGE OverloadedStrings #-}
module Bot (main) where

import Control.Exception
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode, status200)
import Data.Aeson (object, (.=), encode)
import T3.Client

serverUrl :: String
serverUrl = "http://localhost:3000"

httpDie :: HttpException -> IO ()
httpDie (StatusCodeException status _ _) = do
  putStrLn $ "Caught http exception with status: "  ++ show (statusCode status)
  putStrLn "Don't recover"
httpDie e = do
  putStrLn "Caught http exception"
  putStrLn (show e) 
  putStrLn "Don't recover"

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let regReq = RegisterRequest (UserName "username")
  initialRequest <- parseUrl (serverUrl ++ "/api/register")
  let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode regReq }
  handle httpDie $ do
    response <- httpLbs request manager
    let status =  responseStatus response
    putStrLn $ "The status code was: " ++ show (statusCode status)
    print $ responseBody response
