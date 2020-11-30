module Task.HTTP.Internal where

import MasonPrelude

badStatus :: Int -> Boolean
badStatus status = status >= 400
