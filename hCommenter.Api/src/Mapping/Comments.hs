module Mapping.Comments where

import ClassyPrelude
import Control.Lens ((^.))
import Database.Persist.Sql (Key, fromSqlKey)
import Database.StorageTypes qualified as S
import Mapping.ExternalTypes (ViewComment (..))

storageToViewComment :: Key S.Comment -> S.Comment -> ViewComment
storageToViewComment commentId comment =
  ViewComment
    { _id = fromSqlKey commentId
    , _created = comment ^. S.dateCreated
    , _message = comment ^. S.message
    , _score = calculateScore (comment ^. S.upvotes) (comment ^. S.downvotes)
    , _children = []
    , _authorName = comment ^. S.author
    , _conversationUrl = comment ^. S.convoUrl
    }

calculateScore :: Int -> Int -> Int
calculateScore upvotes downvotes = upvotes - downvotes
