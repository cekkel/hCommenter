{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Database.Interface (getManyComments, getComment, newComment, editComment, deleteComment, runCommentStoragePure, CommentStorage, PureStorage(..)) where

import           ClassyPrelude
import           Control.Lens               (makeLenses, view, (%~), (&), (+~))
import qualified Data.Map                   as M
import           Database.StorageTypes      (Comment, ID, SortBy (..),
                                             StorageError (..))
import           Effectful                  (Dispatch (Dynamic), DispatchOf,
                                             Eff, Effect, type (:>))
import           Effectful.Dispatch.Dynamic (reinterpret)
import           Effectful.Error.Static     (Error, throwError)
import           Effectful.State.Dynamic    (evalStateLocal, gets, modify)
import           Effectful.TH               (makeEffect)

data CommentStorage :: Effect where
  GetManyComments :: Int -> Int -> SortBy -> CommentStorage m [Comment]
  GetComment :: ID -> CommentStorage m Comment
  NewComment :: Comment -> CommentStorage m ID
  EditComment :: ID -> (Comment -> Comment) -> CommentStorage m ()
  DeleteComment :: ID -> CommentStorage m ()

type instance DispatchOf CommentStorage = 'Dynamic
makeEffect ''CommentStorage

data PureStorage = PureStorage {
    _store  :: M.Map ID Comment,
    _nextID :: Int
  }

makeLenses ''PureStorage

runCommentStoragePure
  :: Error StorageError :> es
  => PureStorage
  -> Eff (CommentStorage : es) a
  -> Eff es a
runCommentStoragePure storage = reinterpret (evalStateLocal storage) $ \_ -> \case
  GetManyComments start end sortMethod ->
    gets $ toList . M.take (sortedRange sortMethod start end) . view store
  GetComment cID -> do
    comment <- gets $ M.lookup cID . view store
    maybe
      (throwError CommentNotFound)
      pure
      comment
  NewComment comment -> do
    newID <- gets $ view nextID
    modify $ \ps ->
      ps & store %~ M.insert newID comment
         -- This is the only time that a comment is added, so just increment the ID!
         -- Security-wise it's fine since obviously this should not be used in production.
         & nextID +~ 1
    pure newID
  EditComment cID f ->
    modify $ store %~ M.adjust f cID
  DeleteComment cID ->
    modify $ store %~ M.delete cID


sortedRange :: SortBy -> Int -> Int -> Int
sortedRange sortMethod start end = case sortMethod of
  Popular       -> end - start
  Controversial -> end - start
  Old           -> end - start
  New           -> end - start
