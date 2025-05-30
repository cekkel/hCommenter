module Logging.LogContext (LogField (..), logFieldToObjectPairs) where

import Data.Aeson.Types (Pair, (.=))
import Network.HTTP.Types (Method, Status (statusCode))

import Utils.Error (CustomError)

data LogField
  = CorrelationID Text
  | ConvoUrl Text
  | CommentId Int64
  | ParentId (Maybe Int64)
  | Username Text
  | Note Text
  | RequestMethod Method
  | RequestPath Text
  | ResponseBody Text
  | StatusCode Status
  | AppError (CallStack, CustomError)
  deriving (Show)

logFieldToObjectPairs :: LogField -> [Pair]
logFieldToObjectPairs = \case
  CorrelationID txt -> ["CorrelationID" .= txt]
  ConvoUrl txt -> ["ConvoUrl" .= txt]
  CommentId txt -> ["CommentId" .= txt]
  ParentId txt -> ["ParentId" .= txt]
  Username txt -> ["Username" .= txt]
  Note txt -> ["Note" .= txt]
  RequestMethod method -> ["Method" .= decodeUtf8 @Text method]
  RequestPath path -> ["Path" .= path]
  ResponseBody content -> ["ResponseBody" .= content]
  StatusCode status -> ["StatusCode" .= tshow (statusCode status)]
  AppError (stack, err) -> ["CallStack" .= tshow stack, "Error" .= tshow err]
