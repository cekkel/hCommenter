module Logging.LogContext (LogField (..), logFieldToObjectPair) where

import Data.Aeson.Types (Pair, (.=))

data LogField
  = CorrelationID Text
  | ConvoUrl Text
  | CommentId (Maybe Int64)
  | ParentId (Maybe Int64)
  | Username Text
  deriving (Show)

logFieldToObjectPair :: LogField -> Pair
logFieldToObjectPair = \case
  CorrelationID txt -> "CorrelationID" .= txt
  ConvoUrl txt -> "ConvoUrl" .= txt
  CommentId txt -> "CommentId" .= txt
  ParentId txt -> "ParentId" .= txt
  Username txt -> "Username" .= txt
