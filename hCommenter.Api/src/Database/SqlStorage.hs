{-# LANGUAGE GADTs #-}

module Database.SqlStorage (runCommentStorageSQL) where

import ClassyPrelude hiding (Reader)
import Control.Lens ((^.))
import Database.Interface (CommentStorage (..))
import Database.Persist
  ( Entity (Entity)
  , PersistStoreWrite (insert, update)
  , SelectOpt (Asc, Desc)
  , selectList
  , (=.)
  , (==.)
  )
import Database.Persist qualified as P
import Database.Persist.Sqlite (PersistStoreRead (get))
import Database.SqlPool (SqlPool, runSqlPool, withConn)
import Database.StorageTypes
  ( Comment (..)
  , EntityField (..)
  , SortBy (..)
  , StorageError (..)
  , downvotes
  , fromNewComment
  , message
  , upvotes
  )
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Logging (Log)
import Mapping.Typeclass (MapsFrom (mapFrom))
import Server.ServerTypes (Backend)

runCommentStorageSQL ::
  ( Log :> es
  , Error StorageError :> es
  , IOE :> es
  ) =>
  Backend ->
  Eff (CommentStorage : SqlPool : es) a ->
  Eff es a
runCommentStorageSQL backend =
  runSqlPool backend
    . interpret
      ( \_ action ->
          withConn $ case action of
            GetCommentsForConvo convoUrlQ sortMethod -> do
              map mapFrom <$> selectList [CommentConvoUrl ==. convoUrlQ] (generateSort sortMethod)
            GetCommentsForUser userNameQ sortMethod -> do
              map mapFrom <$> selectList [CommentAuthor ==. userNameQ] (generateSort sortMethod)
            GetReplies cID sortMethod -> do
              replies <- map mapFrom <$> selectList [CommentParent ==. Just cID] (generateSort sortMethod)
              pure replies
            InsertComment comment -> do
              fullComment <- liftIO $ fromNewComment comment

              cID <- insert fullComment
              pure cID
            EditComment cID f -> do
              comment <- get cID
              case comment of
                Nothing -> lift $ throwError CommentNotFound
                Just val -> do
                  let
                    upComment = f val
                  update
                    cID
                    [ CommentMessage =. (upComment ^. message)
                    , CommentUpvotes =. (upComment ^. upvotes)
                    , CommentDownvotes =. (upComment ^. downvotes)
                    ]
                  pure $ mapFrom (Entity cID upComment)
            DeleteComment cID -> P.delete cID
      )

-- | TODO: Needs revisiting since sorts in persistent are basic.
generateSort :: SortBy -> [SelectOpt Comment]
generateSort sortMethod = case sortMethod of
  Popular -> [Desc CommentUpvotes]
  Controversial -> [Desc CommentDownvotes]
  Old -> [Asc CommentDateCreated]
  New -> [Desc CommentDateCreated]
