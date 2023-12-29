{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.LocalStorage where

import           ClassyPrelude
import           Control.Lens               (view, (%~), (&), (+~), (^.))
import           Data.Binary                (decodeFile, encodeFile)
import qualified Data.Map                   as M
import           Database.Interface         (CommentStorage (..))
import           Database.StorageTypes      (Comment, ID (ID), PureStorage,
                                             SortBy (..), StorageError (..),
                                             nextID, store)
import           Effectful                  (Eff, IOE, (:>))
import           Effectful.Dispatch.Dynamic (interpret)
import           Effectful.Error.Static     (Error, throwError)

runCommentStorageIO
  :: ( Error StorageError :> es
     , IOE :> es
     )
  => FilePath
  -> Eff (CommentStorage : es) a
  -> Eff es a
runCommentStorageIO filePath =
  interpret $ \_ -> \case
    GetManyComments (ID start) (ID end) sortMethod -> do
      toList . M.take (sortedRange sortMethod start end) . view store
        <$> liftIO (decodeFile filePath)

    GetComment cID -> do
      storage <- liftIO (decodeFile filePath)
      getCommentIfExists cID storage

    NewComment comment -> do
      storage <- liftIO $ decodeFile filePath

      let newID = storage ^. nextID
          updatedStorage =
            storage
              & store %~ M.insert newID comment
                -- This is the only time that a comment is added, so just increment the ID!
                -- Security-wise it's fine since obviously this should not be used in production.
              & nextID +~ 1

      liftIO $ encodeFile filePath updatedStorage
      pure newID

    EditComment cID f -> do
      storage <- liftIO $ decodeFile filePath
      let updatedStorage = storage & store %~ M.adjust f cID
      liftIO $ encodeFile filePath updatedStorage
      getCommentIfExists cID updatedStorage

    DeleteComment cID -> do
      storage <- liftIO $ decodeFile filePath
      let updatedStorage = storage & store %~ M.delete cID
      liftIO $ encodeFile filePath updatedStorage

getCommentIfExists
  :: ( Error StorageError :> es
     , IOE :> es
     )
  => ID -> PureStorage -> Eff es Comment
getCommentIfExists cID storage = do
  case M.lookup cID . view store $ storage of
    Just c  -> pure c
    Nothing -> throwError CommentNotFound

sortedRange :: SortBy -> Int -> Int -> Int
sortedRange sortMethod start end = case sortMethod of
  Popular       -> end - start
  Controversial -> end - start
  Old           -> end - start
  New           -> end - start
