{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client
import System.Random
import System.IO.Unsafe

-- Generate random string for testing purpose
randomStr :: String
randomStr = take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen

newtype EchoMessage = EchoMessage
  { msg :: String
  } deriving (Show, Generic)

instance FromJSON EchoMessage

type API = "hello" :> QueryParam "message" String :> Get '[JSON] EchoMessage

api :: Proxy API
api = Proxy

hello :: Maybe String
      -> ClientM EchoMessage
hello = client api

queries :: ClientM (EchoMessage)
queries = do
  message <- hello (Just randomStr)
  return (message)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (message) -> do
print message
