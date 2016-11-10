{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module EchoServer
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Char

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show) 

data EchoMessage = EchoMessage
  { msg :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''EchoMessage)

type API = "users" :> Get '[JSON] [User]
            :<|> "hello" :> QueryParam "message" String :> Get '[JSON] EchoMessage

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
        :<|> hello
    where
        hello :: Maybe String -> Handler EchoMessage
        hello mname = return . EchoMessage $ case mname of
          Nothing -> "Hello, Tcd"
          Just n  -> "Hello, " ++ uppercase n

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

uppercase :: String -> String
uppercase = map toUpper
