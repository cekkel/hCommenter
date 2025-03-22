module Mapping.Comments (storageToViewComment) where

import Database.Persist.Sql (fromSqlKey)
import Optics

import Database.StorageTypes
import Mapping.ExternalTypes
  ( ViewComment (..)
  )

storageToViewComment :: Key Comment -> Comment -> ViewComment
storageToViewComment commentId comment =
  ViewComment
    { id = fromSqlKey commentId
    , created = comment ^. #dateCreated
    , message = comment ^. #message
    , score = calculateScore (comment ^. #upvotes) (comment ^. #downvotes)
    , replies = Nothing
    , authorName = comment ^. #author
    , conversationUrl = comment ^. #convoUrl
    }

calculateScore :: Int -> Int -> Int
calculateScore upvotes downvotes = upvotes - downvotes
