{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.SqlStorage (runCommentStorageSQL) where

import Database.Persist ((=.), (==.))
import Database.Persist.Sqlite (SqlBackend, fromSqlKey)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Optics
import PyF (fmt)

import Database.Persist qualified as P
import Effectful.Reader.Static qualified as ES

import Database.Interface (CommentStorage (..))
import Database.SqlPool (SqlPool, withConn)
import Database.StorageTypes
  ( Comment (..)
  , EntityField (..)
  , SortBy (..)
  , StorageError (..)
  , fromNewComment
  )
import Logging.LogEffect (Log)
import Logging.Utilities (logError)
import Mapping.Typeclass (MapsFrom (mapFrom))
import Utils.RequestContext (RequestContext)

runCommentStorageSQL
  :: ( ES.Reader RequestContext :> es
     , Error StorageError :> es
     , IOE :> es
     , Log :> es
     , SqlPool :> es
     )
  => Eff (CommentStorage : es) a
  -> Eff es a
runCommentStorageSQL = do
  interpret
    ( \_ action ->
        withConn $ handleAny throwStorageError $ case action of
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
            comment <- P.get cID
            case comment of
              Nothing -> lift $ do
                logError [fmt|No comment found with ID: {fromSqlKey cID}|]
                throwError $ CommentNotFound "Comment not found"
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

throwStorageError
  :: ( Error StorageError :> es
     , Log :> es
     )
  => SomeException
  -> ReaderT SqlBackend (Eff es) a
throwStorageError e = lift $ do
  logError [fmt|Unhandled storage error: {show e}|]
  throwError $ UnhandledStorageError $ tshow e
