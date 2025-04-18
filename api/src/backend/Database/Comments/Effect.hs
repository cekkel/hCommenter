{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Comments.Effect (runCommentStorageSQL) where

import Database.Esqueleto.Experimental
  ( OrderBy
  , SqlBackend
  , SqlExpr
  , Value (..)
  , asc
  , desc
  , from
  , fromSqlKey
  , just
  , orderBy
  , select
  , table
  , val
  , where_
  , (-.)
  , (==.)
  , (^.)
  )
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import PyF (fmt)

import Database.Persist qualified as P

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
import Mapping.ExternalTypes (ViewComment)
import Mapping.Typeclass (MapsFrom (mapFrom))

runCommentStorageSQL
  :: ( Error StorageError :> es
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
          GetCommentsForConvo convoUrlQ sortMethod -> getCommentsWhere sortMethod $
            \comments -> comments ^. CommentConvoUrl ==. val convoUrlQ
          GetCommentsForUser userNameQ sortMethod -> getCommentsWhere sortMethod $
            \comments -> comments ^. CommentAuthor ==. val userNameQ
          GetReplies cID sortMethod -> getCommentsWhere sortMethod $
            \comments -> comments ^. CommentParent ==. just (val cID)
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
                  SendNewContent newContent -> CommentMessage P.=. newContent
                  SendUpvote -> CommentUpvotes P.+=. 1
                  SendDownvote -> CommentDownvotes P.+=. 1

            lift $ logDebug [fmt|Performing edits: {tshow edits}|]

            updatedComment <-
              -- TODO: Update to use more specific error type when possible.
              catchAny (P.updateGet cID sqlEdits) $ \e -> lift $ do
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

getCommentsWhere
  :: (MonadIO m)
  => SortBy
  -> (SqlExpr (P.Entity Comment) -> SqlExpr (Value Bool))
  -> ReaderT SqlBackend m [ViewComment]
getCommentsWhere sortMethod condition = (map . map) mapFrom $
  select $
    do
      comments <- from $ table @Comment
      where_ $ condition comments
      orderBy $ makeSort sortMethod comments
      pure comments

makeSort :: SortBy -> SqlExpr (P.Entity Comment) -> [SqlExpr OrderBy]
makeSort sortMethod comments = case sortMethod of
  Popular -> [desc $ (comments ^. CommentUpvotes) -. (comments ^. CommentDownvotes)]
  Controversial -> [desc $ comments ^. CommentDownvotes]
  Old -> [asc $ comments ^. CommentDateCreated]
  New -> [desc $ comments ^. CommentDateCreated]
