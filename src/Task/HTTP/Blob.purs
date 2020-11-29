module Task.HTTP.Blob where

import MasonPrelude
import Task (Promise, Task, fromPromise)
import Web.File.Blob (Blob)

foreign import textImpl :: ∀ x. Blob -> Effect (Promise x String)

text :: ∀ x. Blob -> Task x String
text = fromPromise <. textImpl
