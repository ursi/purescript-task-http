module Task.HTTP
  ( class IsBody
  , toBuffer
  , Method
  , Error
  , Buffer
  , request
  , getJson
  , getString
  , decodeJson
  , string
  ) where

import MasonPrelude
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as A
import Data.Map (Map)
import Data.Map as Map
import Data.Nullable (Nullable, toNullable)
import Foreign.Object (Object)
import Foreign.Object as FO
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as B
import Node.Encoding (Encoding(UTF8))
import Task (Canceler, ForeignCallback, Task, lmap, throwError)
import Task as Task

foreign import data Module :: Type

foreign import http :: Module

foreign import https :: Module

foreign import protocolImpl :: (∀ a. Maybe a) -> (∀ a. a -> Maybe a) -> String -> Maybe String

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

data Error
  = BadBody String
  | BadStatus Status
  | BadURL String

type Buffer
  = ImmutableBuffer

getModule :: String -> Maybe Module
getModule url =
  protocolImpl Nothing Just url
    >>= case _ of
        "https:" -> Just https
        "http:" -> Just http
        _ -> Nothing

type Request a
  = { url :: String
    , method :: Method
    , headers :: Array (String /\ String)
    , body :: a
    , timeout :: Maybe Int
    }

class IsBody a where
  toBuffer :: a -> Buffer

instance isBodyUnit :: IsBody Unit where
  toBuffer _ = B.create 0

instance isBodyString :: IsBody String where
  toBuffer = B.fromString ~$ UTF8

instance isBodyArray :: IsBody (Array Int) where
  toBuffer = B.fromArray

instance isBodyImmutableBuffer :: IsBody ImmutableBuffer where
  toBuffer = identity

foreign import requestImpl ::
  Module ->
  { url :: String
  , method :: String
  , headers :: Object String
  , body :: Buffer
  , timeout :: Nullable Int
  } ->
  ForeignCallback
    { body :: Buffer
    , status :: Status
    , headers :: Object String
    } ->
  ForeignCallback Error ->
  Effect Canceler

type Response a
  = { body :: a
    , status :: Status
    , headers :: Map String String
    }

type Status
  = { code :: Int, message :: String }

request ::
  ∀ a b.
  IsBody a =>
  Request a ->
  (Buffer -> String \/ b) -> Task Error (Response b)
request r fromBuffer = case getModule r.url of
  Just m ->
    Task.fromForeign
      ( requestImpl
          m
          { url: r.url
          , method: show r.method
          , headers: FO.fromFoldable r.headers
          , body: toBuffer r.body
          , timeout: toNullable r.timeout
          }
      )
      >>= \res@{ status, headers } ->
          if badStatus status.code then
            throwError $ BadStatus status
          else case fromBuffer res.body of
            Right b ->
              pure
                $ res
                    { body = b
                    , headers =
                      headers
                        # FO.toArrayWithKey Tuple
                        # Map.fromFoldable
                    }
            Left error -> throwError $ BadBody error
  Nothing -> throwError $ BadURL r.url

badStatus :: Int -> Boolean
badStatus _ = false

decodeJson :: ∀ a. DecodeJson a => Buffer -> String \/ a
decodeJson =
  B.toString UTF8
    .> A.jsonParser
    .> case _ of
        Right json ->
          A.decodeJson json
            # lmap show
        Left error -> Left error

string :: Buffer -> String \/ String
string = Right <. B.toString UTF8

defaultRequest :: Request Unit
defaultRequest =
  { url: ""
  , method: GET
  , headers: []
  , body: unit
  , timeout: Nothing
  }

getJson :: ∀ a. DecodeJson a => String -> Task Error a
getJson url =
  request (defaultRequest { url = url }) decodeJson
    <#> _.body

getString :: String -> Task Error String
getString url =
  request (defaultRequest { url = url }) string
    <#> _.body
