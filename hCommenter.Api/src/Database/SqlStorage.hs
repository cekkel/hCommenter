{-# LANGUAGE GADTs #-}

module Database.SqlStorage (runCommentStorageSQL) where

import           ClassyPrelude              hiding (Reader)
import           Control.Lens               ((^.))
import           Database.Interface         (CommentStorage (..))
import           Database.Persist           (Entity (Entity),
                                             PersistEntity (Key),
                                             PersistStoreWrite (insert, update),
                                             PersistUniqueRead (getBy),
                                             SelectOpt (Asc, Desc), selectList,
                                             (=.), (==.))
import qualified Database.Persist           as P
import           Database.Persist.Sqlite    (PersistStoreRead (get))
import           Database.SqlPool           (SqlPool, runSqlPool, withConn)
import           Database.StorageTypes      (Comment, EntityField (..),
                                             SortBy (..), StorageError (..),
                                             Unique (UniqueUrl, UniqueUsername),
                                             downvotes, message, upvotes)
import           Effectful                  (Eff, IOE, (:>))
import           Effectful.Dispatch.Dynamic (interpret)
import           Effectful.Error.Static     (Error, throwError)
import           Logging                    (Log)
import           Server.ServerTypes         (Backend)

entityToTuple :: Entity record -> (Key record, record)
entityToTuple (Entity key value) = (key, value)

runCommentStorageSQL
  :: ( Log :> es
     , Error StorageError :> es
     , IOE :> es
     )
  => Backend
  -> Eff (CommentStorage : SqlPool : es) a
  -> Eff es a
runCommentStorageSQL backend = runSqlPool backend . interpret (\_ action ->
  withConn $ case action of
    GetCommentsForConvo convoUrlQ sortMethod -> do
      convo <- getBy $ UniqueUrl convoUrlQ
      case convo of
        Nothing -> lift $ throwError ConvoNotFound
        Just (Entity convoId _) -> do
          map entityToTuple <$> selectList [PostedTo ==. convoId] (generateSort sortMethod)

    GetCommentsForUser userNameQ sortMethod -> do
      user <- getBy $ UniqueUsername userNameQ
      case user of
        Nothing -> lift $ throwError UserNotFound
        Just (Entity userId _) -> do
          map entityToTuple <$> selectList [PostedBy ==. userId] (generateSort sortMethod)

    GetReplies cID sortMethod -> do
      map entityToTuple <$> selectList [Parent ==. Just cID] (generateSort sortMethod)

    NewComment comment -> do
      cID <- insert comment
      pure (cID, comment)

    EditComment cID f -> do
      comment <- get cID
      case comment of
        Nothing -> lift $ throwError CommentNotFound
        Just val -> do
          let upComment = f val
          update cID [
              Message =. (upComment ^. message)
            , Upvotes =. (upComment ^. upvotes)
            , Downvotes =. (upComment ^. downvotes)
            ]
          pure upComment

    DeleteComment cID -> P.delete cID
  )

-- | TODO: Needs revisiting since sorts in persistent are basic.
generateSort :: SortBy -> [SelectOpt Comment]
generateSort sortMethod = case sortMethod of
  Popular       -> [Desc Upvotes]
  Controversial -> [Desc Downvotes]
  Old           -> [Asc DateCreated]
  New           -> [Desc DateCreated]
