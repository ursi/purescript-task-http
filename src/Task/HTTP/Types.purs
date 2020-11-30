module Task.HTTP.Types where

import MasonPrelude
import Data.Map (Map)

data Error
  = BadBody String (Response String)
  | BadStatus (Response String)
  | BadURL String

derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show = genericShow

data Method
  = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE

derive instance genericMethod :: Generic Method _

instance showMethod :: Show Method where
  show = genericShow

type Request a
  = { url :: String
    , method :: Method
    , headers :: Array (String /\ String)
    , body :: a
    , timeout :: Maybe Int
    }

type Response a
  = { body :: a
    , status :: Status
    , headers :: Map String String
    }

type Status
  = { code :: Int, message :: String }
