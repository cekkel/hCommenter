module Mapping.Comments where

import ClassyPrelude
import Database.Persist.Sql (fromSqlKey)
import Database.StorageTypes
import Mapping.ExternalTypes (ViewComment (..))
import Optics

storageToViewComment :: Key Comment -> Comment -> ViewComment
storageToViewComment commentId comment =
  ViewComment
    { _id = fromSqlKey commentId
    , _created = comment ^. #dateCreated
    , _message = comment ^. #message
    , _score = calculateScore (comment ^. #upvotes) (comment ^. #downvotes)
    , _children = []
    , _authorName = comment ^. #author
    , _conversationUrl = comment ^. #convoUrl
    }

calculateScore :: Int -> Int -> Int
calculateScore upvotes downvotes = upvotes - downvotes
