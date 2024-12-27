{-# LANGUAGE TemplateHaskell #-}

module Mapping.ExternalTypes where

import ClassyPrelude
import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)

data ViewComment = ViewComment
  { _id :: Int64
  , _created :: UTCTime
  , _message :: Text
  , _score :: Int
  , _children :: [ViewComment]
  , _authorName :: Text
  , _conversationUrl :: Text
  }
  deriving (Eq, Show, Generic)

makeLenses ''ViewComment

instance ToSchema ViewComment

instance ToJSON ViewComment
