{-# LANGUAGE GADTs #-}

module Database.PureStorage where
import           ClassyPrelude
import           Control.Lens               (view, (%~), (&), (+~))
import qualified Data.Map                   as M
import           Database.Interface         (CommentStorage (..))
import           Database.StorageTypes      (Comment, ID (ID), PureStorage,
                                             SortBy (..), StorageError (..),
                                             nextID, store)
import           Effectful                  (Eff, (:>))
import           Effectful.Dispatch.Dynamic (reinterpret)
import           Effectful.Error.Static     (Error, throwError)
import           Effectful.State.Dynamic    (State, evalStateLocal, gets,
                                             modify)
runCommentStoragePure
  :: Error StorageError :> es
  => PureStorage
  -> Eff (CommentStorage : es) a
  -> Eff es a
runCommentStoragePure storage = reinterpret (evalStateLocal storage) $ \_ -> \case
  GetManyComments (ID start) (ID end) sortMethod ->
    gets $ toList . M.take (sortedRange sortMethod start end) . view store

  GetComment cID -> getCommentIfExists cID

  NewComment comment -> do
    newID <- gets $ view nextID
    modify $ \ps ->
      ps & store %~ M.insert newID comment
         -- This is the only time that a comment is added, so just increment the ID!
         -- Security-wise it's fine since obviously this should not be used in production.
         & nextID +~ 1
    pure newID

  EditComment cID f -> do
    modify $ store %~ M.adjust f cID
    getCommentIfExists cID

  DeleteComment cID -> do
    modify $ store %~ M.delete cID

getCommentIfExists ::
  (State PureStorage :> es,
  Error StorageError :> es)
  => ID -> Eff es Comment
getCommentIfExists cID = do
  comment <- gets $ M.lookup cID . view store
  case comment of
    Just c  -> pure c
    Nothing -> throwError CommentNotFound

sortedRange :: SortBy -> Int -> Int -> Int
sortedRange sortMethod start end = case sortMethod of
  Popular       -> end - start
  Controversial -> end - start
  Old           -> end - start
  New           -> end - start
