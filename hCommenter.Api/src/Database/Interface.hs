{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Database.Interface (getCommentsForConvo, getCommentsForUser, getReplies, newComment, editComment, deleteComment, CommentStorage(..)) where

import           ClassyPrelude
import           Database.StorageTypes (Comment, CommentId, SortBy (..))
import           Effectful             (Dispatch (Dynamic), DispatchOf, Effect)
import           Effectful.TH          (makeEffect)

type ConvoUrl = Text
type Username = Text
type ParentId = CommentId

data CommentStorage :: Effect where
  GetCommentsForConvo :: ConvoUrl -> SortBy -> CommentStorage m [(CommentId, Comment)]
  GetCommentsForUser :: Username -> SortBy -> CommentStorage m [(CommentId, Comment)]
  GetReplies :: ParentId -> SortBy -> CommentStorage m [(CommentId, Comment)]
  NewComment :: Comment -> CommentStorage m (CommentId, Comment)
  EditComment :: CommentId -> (Comment -> Comment) -> CommentStorage m Comment
  DeleteComment :: CommentId -> CommentStorage m ()

type instance DispatchOf CommentStorage = 'Dynamic
makeEffect ''CommentStorage
