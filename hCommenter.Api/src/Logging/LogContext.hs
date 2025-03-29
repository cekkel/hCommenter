module Logging.LogContext (LogField (..), logFieldToObjectPair) where

import Data.Aeson.Types (Pair, (.=))
import Network.HTTP.Types (Method, Status (statusCode))

data LogField
  = CorrelationID Text
  | ConvoUrl Text
  | CommentId Int64
  | ParentId (Maybe Int64)
  | Username Text
  | Note Text
  | RequestMethod Method
  | RequestPath Text
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
  RequestMethod method -> "Method" .= decodeUtf8 method
  RequestPath path -> "Path" .= path
  StatusCode status -> "StatusCode" .= tshow (statusCode status)
  AppError e -> "AppError" .= tshow e
