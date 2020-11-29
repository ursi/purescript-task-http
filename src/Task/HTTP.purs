module Task.HTTP
  ( module Task.HTTP.Types
  , class IsBody
  , toBlob
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
import Data.MediaType.Common (applicationJSON, textPlain)
import Data.Undefinable (Undefinable, toUndefinable)
import Foreign.Object (Object)
import Foreign.Object as FO
import Task (Canceler, ForeignCallback, Task, (>>!), lmap, throwError)
import Task as Task
import Task.HTTP.Blob as TaskBlob
import Task.HTTP.Types (Error(..), Method(..), Request, Response, Status)
import Web.File.Blob (Blob)
import Web.File.Blob as Blob

class IsBody a where
  toBlob :: a -> Blob

instance isBodyUnit :: IsBody Unit where
  toBlob _ = Blob.fromString "" textPlain

instance isBodyString :: IsBody String where
  toBlob = Blob.fromString ~$ textPlain

instance isBodyJson :: IsBody Json where
  toBlob = Argonaut.stringify .> (Blob.fromString ~$ applicationJSON)

foreign import requestImpl ::
  { url :: String
  , method :: String
  , headers :: Object String
  , body :: Blob
  , timeout :: Undefinable Int
  } ->
  ForeignCallback
    { body :: Blob
    , status :: Status
    , headers :: Object String
    } ->
  ForeignCallback Error ->
  Effect Canceler

request ::
  ∀ a b.
  IsBody a =>
  Request a ->
  (Blob -> Task String b) ->
  Task Error (Response b)
request r fromBlob =
  Task.fromForeign
    ( requestImpl
        { url: r.url
        , method: show r.method
        , headers: FO.fromFoldable r.headers
        , body: toBlob r.body
        , timeout: toUndefinable r.timeout
        }
    )
    >>= \res@{ status, headers } ->
        if badStatus status.code then
          throwError $ BadStatus status
        else
          fromBlob res.body
            >>= ( \b ->
                  pure
                    $ res
                        { body = b
                        , headers =
                          headers
                            # FO.toArrayWithKey Tuple
                            # Map.fromFoldable
                        }
              )
            >>! throwError
            <. BadBody

badStatus :: Int -> Boolean
badStatus _ = false

json :: ∀ a. DecodeJson a => Blob -> Task String a
json blob = do
  text <- TaskBlob.text blob
  case jsonParser text of
    Right json' -> Task.liftEither (decodeJson json' # lmap show)
    Left error -> throwError error

string :: Blob -> Task String String
string = TaskBlob.text

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
