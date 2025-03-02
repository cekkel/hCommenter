{-# LANGUAGE GADTs #-}

module Database.SqlStorage (runCommentStorageSQL) where

import Database.Persist ((=.), (==.))
import Database.Persist.Sqlite (PersistStoreRead (get))
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Optics

import Database.Persist qualified as P

import Database.Interface (CommentStorage (..))
import Database.SqlPool (SqlPool, runSqlPool, withConn)
import Database.StorageTypes
  ( Comment (..)
  , EntityField (..)
  , SortBy (..)
  , StorageError (..)
  , fromNewComment
  )
import Logging.LogEffect (Log)
import Mapping.Typeclass (MapsFrom (mapFrom))
import Server.ServerTypes (Backend)

runCommentStorageSQL
  :: ( Error StorageError :> es
     , IOE :> es
     , Log :> es
     )
  => Backend
  -> Eff (CommentStorage : SqlPool : es) a
  -> Eff es a
runCommentStorageSQL backend =
  runSqlPool backend
    . interpret
      ( \_ action ->
          withConn $ case action of
            GetCommentsForConvo convoUrlQ sortMethod -> do
              map mapFrom <$> P.selectList [CommentConvoUrl ==. convoUrlQ] (generateSort sortMethod)
            GetCommentsForUser userNameQ sortMethod -> do
              map mapFrom <$> P.selectList [CommentAuthor ==. userNameQ] (generateSort sortMethod)
            GetReplies cID sortMethod -> do
              replies <- map mapFrom <$> P.selectList [CommentParent ==. Just cID] (generateSort sortMethod)
              pure replies
            InsertComment comment -> do
              fullComment <- liftIO $ fromNewComment comment

              cID <- P.insert fullComment
              pure cID
            EditComment cID f -> do
              comment <- get cID
              case comment of
                Nothing -> lift $ throwError CommentNotFound
                Just val -> do
                  let
                    upComment = f val
                  P.update
                    cID
                    [ CommentMessage =. (upComment ^. #message)
                    , CommentUpvotes =. (upComment ^. #upvotes)
                    , CommentDownvotes =. (upComment ^. #downvotes)
                    ]
                  pure $ mapFrom (P.Entity cID upComment)
            DeleteComment cID -> P.delete cID
      )

-- | TODO: Needs revisiting since sorts in persistent are basic.
generateSort :: SortBy -> [P.SelectOpt Comment]
generateSort sortMethod = case sortMethod of
  Popular -> [P.Desc CommentUpvotes]
  Controversial -> [P.Desc CommentDownvotes]
  Old -> [P.Asc CommentDateCreated]
  New -> [P.Desc CommentDateCreated]
