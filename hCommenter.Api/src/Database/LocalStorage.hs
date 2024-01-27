{-# LANGUAGE GADTs #-}

module Database.LocalStorage where

import           ClassyPrelude
import           Control.Lens               ((%~), (&), (^.))
import           Data.Binary                (decodeFile, encodeFile)
import qualified Data.Map                   as M
import           Database.Interface         (CommentStorage (..))

import           Database.Persist.Sql       (fromSqlKey, toSqlKey)
import           Database.StorageTypes      (Comment, CommentId, SortBy (..),
                                             StorageError (..), dateCreated,
                                             downvotes, nextID, postedBy,
                                             postedTo, store, upvotes)
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
    GetCommentsForConvo convoUrlQ sortMethod ->
      sortedComments sortMethod
        <$> findComments filePath (\_ comment -> comment ^. postedTo == toSqlKey 1)


    GetCommentsForUser userNameQ sortMethod -> do
      sortedComments sortMethod
        <$> findComments filePath (\_ comment -> comment ^. postedBy == toSqlKey 1)

    GetReplies cID sortMethod -> do
      sortedComments sortMethod
        <$> findComments filePath (\otherCID _ -> cID == otherCID)

    NewComment comment -> do
      storage <- liftIO $ decodeFile filePath

      let newID = storage ^. nextID
          updatedStorage =
            storage
              & store %~ M.insert newID comment
                -- This is the only time that a comment is added, so just increment the ID!
                -- Security-wise it's fine since obviously this should not be used in production.
              & nextID %~ toSqlKey . (+ 1) . fromSqlKey

      liftIO $ encodeFile filePath updatedStorage
      pure (newID, comment)

    EditComment cID f -> do
      storage <- liftIO $ decodeFile filePath
      case storage ^. store & M.lookup cID of
        Nothing -> throwError CommentNotFound
        Just comment -> do
          let updatedStorage = storage & store %~ M.adjust f cID
          liftIO $ encodeFile filePath updatedStorage
          pure comment

    DeleteComment cID -> do
      storage <- liftIO $ decodeFile filePath
      case storage ^. store & M.lookup cID of
        Nothing -> throwError CommentNotFound
        Just _ -> do
          let updatedStorage = storage & store %~ M.delete cID
          liftIO $ encodeFile filePath updatedStorage

findComments :: (Error StorageError :> es, IOE :> es) => FilePath -> (CommentId -> Comment -> Bool) -> Eff es [(CommentId, Comment)]
findComments filePath condition = do
  storage <- liftIO (decodeFile filePath)
  pure $ filter (uncurry condition) $ M.assocs (storage ^. store)

-- | TODO: Needs revisiting
sortedComments :: SortBy -> [(CommentId, Comment)] -> [(CommentId, Comment)]
sortedComments sortMethod = sortBy $ \(_, c1) (_, c2) -> case sortMethod of
  Popular       -> compare (c1 ^. upvotes) (c2 ^. upvotes)
  Controversial -> compare (c1 ^. downvotes) (c2 ^. downvotes)
  Old           -> compare (c1 ^. dateCreated) (c2 ^. dateCreated)
  New           -> flip compare (c1 ^. dateCreated) (c2 ^. dateCreated)
