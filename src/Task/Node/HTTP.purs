module Task.Node.HTTP
  ( module Task.HTTP.Types
  , class IsBody
  , toBuffer
  , Buffer
  , defaultRequest
  , request
  , URL
  , getJson
  , getString
  , json
  , string
  ) where

import MasonPrelude
import Data.Argonaut (class DecodeJson, Json, decodeJson, jsonParser)
import Data.Argonaut as Argonaut
import Data.Map as Map
import Data.Undefinable (Undefinable, toUndefinable)
import Foreign.Object (Object)
import Foreign.Object as FO
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as B
import Node.Encoding (Encoding(UTF8))
import Task (Canceler, ForeignCallback, Task, lmap, throwError)
import Task as Task
import Task.HTTP.Types (Error(..), Method(..), Request, Response, Status)

foreign import data Module :: Type

foreign import http :: Module

foreign import https :: Module

foreign import protocolImpl :: (∀ a. Maybe a) -> (∀ a. a -> Maybe a) -> String -> Maybe String

type Buffer
  = ImmutableBuffer

getModule :: String -> Maybe Module
getModule url =
  protocolImpl Nothing Just url
    >>= case _ of
        "https:" -> Just https
        "http:" -> Just http
        _ -> Nothing

class IsBody a where
  toBuffer :: a -> Buffer

instance isBodyUnit :: IsBody Unit where
  toBuffer _ = B.create 0

instance isBodyString :: IsBody String where
  toBuffer = B.fromString ~$ UTF8

instance isBodyJson :: IsBody Json where
  toBuffer = Argonaut.stringify .> (B.fromString ~$ UTF8)

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
  , timeout :: Undefinable Int
  } ->
  ForeignCallback
    { body :: Buffer
    , status :: Status
    , headers :: Object String
    } ->
  ForeignCallback Error ->
  Effect Canceler

request ::
  ∀ a b.
  IsBody a =>
  Request a ->
  (Buffer -> String \/ b) ->
  Task Error (Response b)
request r fromBuffer = case getModule r.url of
  Just m ->
    Task.fromForeign
      ( requestImpl
          m
          { url: r.url
          , method: show r.method
          , headers: FO.fromFoldable r.headers
          , body: toBuffer r.body
          , timeout: toUndefinable r.timeout
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

json :: ∀ a. DecodeJson a => Buffer -> String \/ a
json =
  B.toString UTF8
    .> jsonParser
    .> case _ of
        Right json' ->
          decodeJson json'
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

type URL
  = String

getJson :: ∀ a. DecodeJson a => URL -> Task Error a
getJson url =
  request (defaultRequest { url = url }) json
    <#> _.body

getString :: URL -> Task Error String
getString url =
  request (defaultRequest { url = url }) string
    <#> _.body
