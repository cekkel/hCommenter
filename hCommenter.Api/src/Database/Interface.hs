{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Database.Interface (getManyComments, getComment, newComment, editComment, deleteComment, CommentStorage(..)) where

import           ClassyPrelude
import           Database.StorageTypes (Comment, ID, SortBy (..))
import           Effectful             (Dispatch (Dynamic), DispatchOf, Effect)
import           Effectful.TH          (makeEffect)

data CommentStorage :: Effect where
  GetManyComments :: ID -> ID -> SortBy -> CommentStorage m [Comment]
  GetComment :: ID -> CommentStorage m Comment
  NewComment :: Comment -> CommentStorage m ID
  EditComment :: ID -> (Comment -> Comment) -> CommentStorage m Comment
  DeleteComment :: ID -> CommentStorage m ()

type instance DispatchOf CommentStorage = 'Dynamic
makeEffect ''CommentStorage
