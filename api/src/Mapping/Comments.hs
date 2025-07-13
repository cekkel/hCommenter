module Mapping.Comments (storageToViewComment) where

import Database.Persist.Sql (fromSqlKey)
import Optics

import Database.Schema
import Mapping.ExternalTypes
  ( ViewComment (..)
  )

storageToViewComment :: Key Comment -> Comment -> ViewComment
storageToViewComment commentId comment =
  ViewComment
    { id = fromSqlKey commentId
    , created = comment ^. #commentCreatedAt
    , message = comment ^. #commentText
    , score = calculateScore (comment ^. #commentUpvotes) (comment ^. #commentDownvotes)
    , replies = Nothing
    , authorName = comment ^. #commentUserId
    , conversationUrl = comment ^. #commentConversationId
    }

calculateScore :: Int -> Int -> Int
calculateScore upvotes downvotes = upvotes - downvotes
