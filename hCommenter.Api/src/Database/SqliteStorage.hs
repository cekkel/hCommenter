{-# LANGUAGE GADTs #-}

module Database.SqliteStorage where

import           ClassyPrelude              hiding (Reader)
import           Control.Lens               (view, (%~), (&), (+~), (^.))
import           Data.Binary                (decodeFile, encodeFile)
import qualified Data.Map                   as M
import           Database.Interface         (CommentStorage (..))
import           Database.Persist           (Entity (Entity), Filter,
                                             PersistStoreWrite (insert, update),
                                             selectList, (==.))
import           Database.Persist.Sqlite    (PersistQueryRead (selectFirst),
                                             PersistStoreRead (get), SqlBackend,
                                             runSqlConn, runSqlite,
                                             withSqliteConn)
import           Database.SqlPool           (SqlPool, withConn)
import           Database.StorageTypes      (Comment, CommentId,
                                             EntityField (Message), PureStorage,
                                             SortBy (..), StorageError (..),
                                             nextID, store)
import           Effectful                  (Eff, IOE, (:>))
import           Effectful.Dispatch.Dynamic (interpret, reinterpret)
import           Effectful.Error.Static     (Error, throwError)

runCommentStorageSQLite
  :: ( SqlPool :> es
     , Error StorageError :> es
     , IOE :> es
     )
  => FilePath
  -> Eff (CommentStorage : es) a
  -> Eff es a
runCommentStorageSQLite filePath = interpret $ \_ command -> do
  case command of
    GetCommentsForConvo convoUrlQ sortMethod -> do
      findComments []

    GetCommentsForUser userNameQ sortMethod -> do
      -- storage <- liftIO (decodeFile filePath)
      findComments []
      -- pure undefined

    GetReplies cID -> do
      -- storage <- liftIO (decodeFile filePath)
      findComments []
      -- pure undefined

    NewComment comment -> withConn $ do
      cID <- insert comment
      pure (cID, comment)

    EditComment cID f -> do
      -- update cID []
      pure undefined

    DeleteComment cID -> do
      storage <- liftIO $ decodeFile filePath
      let updatedStorage = storage & store %~ M.delete cID
      liftIO $ encodeFile filePath updatedStorage

-- useSqlLite context = runReader . (runNoLoggingT . withSqliteConn ":memory:" . runSqlConn $ context)
-- initSqlBackendPool
--   :: ( Log :> es
--      , IOE :> es
--      )
--   => Eff (Reader (Pool

-- getCommentIfExists
--   :: ( Error StorageError :> es
--      , IOE :> es
--      , SqlPool :> es
--      )
--   => Eff es Comment
-- getCommentIfExists _ = withConn $ do
--   maybeComment <- selectFirst [Message ==. "First"] []
--   lift $ case maybeComment of
--     Just (Entity _ c) -> pure c
--     Nothing           -> throwError CommentNotFound

findComments
  :: ( Error StorageError :> es
     , IOE :> es
     , SqlPool :> es
     )
  => [Filter Comment]
  -> Eff es [(CommentId, Comment)]
findComments condition = withConn $ do
  comments <- selectList condition []
  lift $ case comments of
    [] -> throwError CommentNotFound
    xs -> pure $ entityToTuple <$> xs

  where
    entityToTuple (Entity cID comment) = (cID, comment)

sortedComments :: SortBy -> [(CommentId, Comment)] -> [(CommentId, Comment)]
sortedComments sortMethod = case sortMethod of
  Popular       -> sortBy $ \x y -> compare x y
  Controversial -> sortBy $ \x y -> compare x y
  Old           -> sortBy $ \x y -> compare x y
  New           -> sortBy $ \x y -> compare x y
