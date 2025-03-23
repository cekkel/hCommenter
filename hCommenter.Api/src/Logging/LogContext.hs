module Logging.LogContext (LogField (..), logFieldToObjectPair) where

import Data.Aeson.Types (Pair, (.=))
import Network.HTTP.Types (Status (statusCode))

data LogField
  = CorrelationID Text
  | ConvoUrl Text
  | CommentId Int64
  | ParentId (Maybe Int64)
  | Username Text
  | Note Text
  | StatusCode Status
  | AppError SomeException
  deriving (Show)

logFieldToObjectPair :: LogField -> Pair
logFieldToObjectPair = \case
  CorrelationID txt -> "CorrelationID" .= txt
  ConvoUrl txt -> "ConvoUrl" .= txt
  CommentId txt -> "CommentId" .= txt
  ParentId txt -> "ParentId" .= txt
  Username txt -> "Username" .= txt
  Note txt -> "Note" .= txt
  StatusCode status -> "StatusCode" .= statusCode status
  AppError e -> "AppError" .= tshow e
