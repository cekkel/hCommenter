{-# LANGUAGE GADTs #-}

module Database.LocalStorage where

-- import Data.Map qualified as M
-- import Database.Interface (CommentStorage (..))
import Database.Persist.Sql (Entity (Entity))
-- import Database.SqlPool (SqlPool)

--   ( Comment
--   , CommentId
--   , SortBy (..)
--   , StorageError (..)
--   , commentStore
--   , fromNewComment
--   , nextID
--   )
-- import Effectful (Eff, IOE, (:>))
-- import Effectful.Dispatch.Dynamic (interpret)
-- import Effectful.Error.Static (Error, throwError)
-- import Mapping.Typeclass (MapsFrom (mapFrom))
import Optics

import Database.StorageTypes

-- runCommentStorageIO ::
--   ( Error StorageError :> es
--   , IOE :> es
--   ) =>
--   FilePath ->
--   Eff (CommentStorage : SqlPool : es) a ->
--   Eff es a
-- runCommentStorageIO filePath =
--   interpret (\_ _ -> error "SQLPool is not implemented for 'LocalFile' backend (this should not be reached)")
--     . interpret
--       ( \_ -> \case
--           GetCommentsForConvo convoUrlQ sortMethod ->
--             map mapFrom
--               <$> sortedComments sortMethod
--               <$> findComments filePath (\_ comment -> comment ^. #convoUrl == convoUrlQ)
--           GetCommentsForUser userNameQ sortMethod -> do
--             map mapFrom
--               <$> sortedComments sortMethod
--               <$> findComments filePath (\_ comment -> comment ^. #author == userNameQ)
--           GetReplies cID sortMethod -> do
--             map mapFrom
--               <$> sortedComments sortMethod
--               <$> findComments filePath (\otherCID _ -> cID == otherCID)
--           InsertComment comment -> do
--             storage <- liftIO $ decodeFile filePath
--             fullComment <- liftIO $ fromNewComment comment
--
--             let
--               newID = storage ^. nextID
--               updatedStorage =
--                 storage
--                   & commentStore
--                   %~ M.insert newID fullComment
--                   -- This is the only time that a comment is added, so just increment the ID!
--                   -- Security-wise it's fine since obviously this should not be used in production.
--                   & nextID
--                   %~ toSqlKey
--                   . (+ 1)
--                   . fromSqlKey
--
--             liftIO $ encodeFile filePath updatedStorage
--             pure newID
--           EditComment cID f -> do
--             storage <- liftIO $ decodeFile filePath
--             case storage ^. commentStore & M.lookup cID of
--               Nothing -> throwError CommentNotFound
--               Just comment -> do
--                 let
--                   updatedStorage = storage & commentStore %~ M.adjust f cID
--                 liftIO $ encodeFile filePath updatedStorage
--                 pure $ mapFrom $ Entity cID comment
--           DeleteComment cID -> do
--             storage <- liftIO $ decodeFile filePath
--             case storage ^. commentStore & M.lookup cID of
--               Nothing -> throwError CommentNotFound
--               Just _ -> do
--                 let
--                   updatedStorage = storage & commentStore %~ M.delete cID
--                 liftIO $ encodeFile filePath updatedStorage
--       )
--
-- findComments :: (Error StorageError :> es, IOE :> es) => FilePath -> (CommentId -> Comment -> Bool) -> Eff es [Entity Comment]
-- findComments filePath condition = do
--   storage <- liftIO (decodeFile filePath)
--
--   let
--     allComments = M.assocs (storage ^. commentStore)
--
--   pure $ uncurry Entity <$> filter (uncurry condition) allComments

-- | TODO: Needs revisiting
sortedComments :: SortBy -> [Entity Comment] -> [Entity Comment]
sortedComments sortMethod = sortBy $ \(Entity _ c1) (Entity _ c2) -> case sortMethod of
  Popular -> compare (c1 ^. #upvotes) (c2 ^. #upvotes)
  Controversial -> compare (c1 ^. #downvotes) (c2 ^. #downvotes)
  Old -> compare (c1 ^. #dateCreated) (c2 ^. #dateCreated)
  New -> compare (c2 ^. #dateCreated) (c1 ^. #dateCreated)
