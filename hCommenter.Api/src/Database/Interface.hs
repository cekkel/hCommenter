{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Interface
  ( getCommentsForConvo
  , getCommentsForUser
  , getReplies
  , insertComment
  , editComment
  , deleteComment
  , CommentStorage (..)
  )
where

import ClassyPrelude
import Database.Persist.Sql (Key)
import Database.StorageTypes
  ( Comment
  , NewComment
  , SortBy (..)
  )
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.TH (makeEffect)
import Mapping.ExternalTypes

type ConvoUrl = Text

type Username = Text

data CommentStorage :: Effect where
  GetCommentsForConvo :: ConvoUrl -> SortBy -> CommentStorage m [ViewComment]
  GetCommentsForUser :: Username -> SortBy -> CommentStorage m [ViewComment]
  GetReplies :: Key Comment -> SortBy -> CommentStorage m [ViewComment]
  InsertComment :: NewComment -> CommentStorage m (Key Comment)
  EditComment :: Key Comment -> (Comment -> Comment) -> CommentStorage m ViewComment
  DeleteComment :: Key Comment -> CommentStorage m ()

type instance DispatchOf CommentStorage = 'Dynamic

makeEffect ''CommentStorage
