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
  , CommentUpdate (..)
  )
where

import Database.Persist.Sql (Key)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.TH (makeEffect)

import Database.StorageTypes
  ( Comment
  , NewComment
  , SortBy (..)
  )
import Mapping.ExternalTypes

type ConvoUrl = Text

type Username = Text

data CommentUpdate
  = SendNewContent Text
  | SendUpvote
  | SendDownvote
  deriving stock (Generic, Show)

data CommentStorage :: Effect where
  GetCommentsForConvo :: ConvoUrl -> SortBy -> CommentStorage m [ViewComment]
  GetCommentsForUser :: Username -> SortBy -> CommentStorage m [ViewComment]
  GetReplies :: Key Comment -> SortBy -> CommentStorage m [ViewComment]
  InsertComment :: NewComment -> CommentStorage m (Key Comment)
  EditComment :: Key Comment -> [CommentUpdate] -> CommentStorage m ViewComment
  DeleteComment :: Key Comment -> CommentStorage m ()

type instance DispatchOf CommentStorage = 'Dynamic

makeEffect ''CommentStorage
