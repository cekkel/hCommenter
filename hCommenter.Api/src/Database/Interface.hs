{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Database.Interface (getManyComments, getComment, newComment, editComment, deleteComment, CommentStorage(..)) where

import           Database.StorageTypes (Comment, CommentId, SortBy (..))
import           Effectful             (Dispatch (Dynamic), DispatchOf, Effect)
import           Effectful.TH          (makeEffect)

data CommentStorage :: Effect where
  GetManyComments :: CommentId -> CommentId -> SortBy -> CommentStorage m [Comment]
  GetComment :: CommentId -> CommentStorage m Comment
  NewComment :: Comment -> CommentStorage m CommentId
  EditComment :: CommentId -> (Comment -> Comment) -> CommentStorage m Comment
  DeleteComment :: CommentId -> CommentStorage m ()

type instance DispatchOf CommentStorage = 'Dynamic
makeEffect ''CommentStorage
