{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Comments.Effect (runCommentStorageSQL) where

import Database.Persist ((+=.), (=.), (==.))
import Database.Persist.Sqlite (fromSqlKey)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import PyF (fmt)

import Database.Persist qualified as P
import Effectful.Reader.Static qualified as ES

import Database.Comments.Interface (CommentStorage (..), CommentUpdate (..))
import Database.Schema
  ( Comment (..)
  , EntityField (..)
  , SortBy (..)
  , StorageError (..)
  , fromNewComment
  )
import Database.SqlPool (SqlPool, withConn)
import Logging.LogEffect (Log)
import Logging.Utilities (logDebug)
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
        withConn $ case action of
          GetCommentsForConvo convoUrlQ sortMethod -> do
            map mapFrom <$> P.selectList [CommentConvoUrl ==. convoUrlQ] (generateSort sortMethod)
          GetCommentsForUser userNameQ sortMethod -> do
            map mapFrom <$> P.selectList [CommentAuthor ==. userNameQ] (generateSort sortMethod)
          GetReplies cID sortMethod -> do
            map mapFrom <$> P.selectList [CommentParent ==. Just cID] (generateSort sortMethod)
          InsertComment comment -> do
            -- TODO: Add a time effect to get the current time, instead of using raw IO
            fullComment <- liftIO $ fromNewComment comment

            catchAny (P.insert fullComment) $ \e -> lift $ do
              throwError $
                UserOrConvoNotFound [fmt|Failed to insert comment with error: {tshow e}|]
          EditComment cID edits -> do
            let
              sqlEdits =
                edits <&> \case
                  SendNewContent newContent -> CommentMessage =. newContent
                  SendUpvote -> CommentUpvotes +=. 1
                  SendDownvote -> CommentDownvotes +=. 1

            lift $ logDebug [fmt|Performing edits: {tshow edits}|]

            updatedComment <-
              catch (P.updateGet cID sqlEdits) $ \(e :: IOError) -> lift $ do
                throwError $
                  CommentNotFound [fmt|Failed to update comment {fromSqlKey cID} with error: {tshow e}|]

            lift $ logDebug [fmt|Updated comment to: {tshow updatedComment}|]

            pure $ mapFrom $ P.Entity cID updatedComment
          DeleteComment cID -> do
            comment <- P.get cID -- just to check if it exists
            when (isNothing comment) $ lift $ do
              throwError $
                CommentNotFound [fmt|Comment {fromSqlKey cID} not found, so it cannot be deleted|]

            P.delete cID
    )

-- PERF: Needs revisiting since sorts in persistent are basic.
generateSort :: SortBy -> [P.SelectOpt Comment]
generateSort sortMethod = case sortMethod of
  Popular -> [P.Desc CommentUpvotes]
  Controversial -> [P.Desc CommentDownvotes]
  Old -> [P.Asc CommentDateCreated]
  New -> [P.Desc CommentDateCreated]
