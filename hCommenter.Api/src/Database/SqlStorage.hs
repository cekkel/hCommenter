{-# LANGUAGE GADTs #-}

module Database.SqlStorage (runCommentStorageSQL) where

import           ClassyPrelude              hiding (Reader)
import           Control.Lens               ((^.))
import           Database.Interface         (CommentStorage (..))
import           Database.Persist           (Entity (Entity),
                                             PersistEntity (Key),
                                             PersistStoreWrite (insert, update),
                                             SelectOpt (Asc, Desc), selectList,
                                             (=.), (==.))
import qualified Database.Persist           as P
import           Database.Persist.Sqlite    (PersistStoreRead (get))
import           Database.SqlPool           (SqlPool, runSqlPool, withConn)
import           Database.StorageTypes      (Comment,
                                             Conversation (Conversation),
                                             EntityField (..), SortBy (..),
                                             StorageError (..), User (User),
                                             commentDownvotes, commentMessage,
                                             commentUpvotes)
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
      map entityToTuple <$> selectList [CommentConvoUrl ==. convoUrlQ] (generateSort sortMethod)

    GetCommentsForUser userNameQ sortMethod -> do
      map entityToTuple <$> selectList [CommentUsername ==. userNameQ] (generateSort sortMethod)

    GetReplies cID sortMethod -> do
      map entityToTuple <$> selectList [CommentParent ==. Just cID] (generateSort sortMethod)

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
              CommentMessage =. (upComment ^. commentMessage)
            , CommentUpvotes =. (upComment ^. commentUpvotes)
            , CommentDownvotes =. (upComment ^. commentDownvotes)
            ]
          pure upComment

    DeleteComment cID -> P.delete cID
  )

-- | TODO: Needs revisiting since sorts in persistent are basic.
generateSort :: SortBy -> [SelectOpt Comment]
generateSort sortMethod = case sortMethod of
  Popular       -> [Desc CommentUpvotes]
  Controversial -> [Desc CommentDownvotes]
  Old           -> [Asc CommentDateCreated]
  New           -> [Desc CommentDateCreated]
