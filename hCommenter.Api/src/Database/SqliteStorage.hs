{-# LANGUAGE GADTs #-}

module Database.SqliteStorage where

import           ClassyPrelude              hiding (Reader)
import           Control.Lens               (view, (%~), (&), (+~), (^.))
import           Data.Binary                (decodeFile, encodeFile)
import qualified Data.Map                   as M
import           Database.Interface         (CommentStorage (..))
import           Database.Persist           (Entity (Entity), (==.))
import           Database.Persist.Sqlite    (PersistQueryRead (selectFirst),
                                             PersistStoreRead (get), SqlBackend,
                                             runSqlConn, runSqlite,
                                             withSqliteConn)
import           Database.SqlPool           (SqlPool, withConn)
import           Database.StorageTypes      (Comment, EntityField (Message),
                                             ID (ID), PureStorage, SortBy (..),
                                             StorageError (..), nextID, store)
import           Effectful                  (Eff, IOE, (:>))
import           Effectful.Dispatch.Dynamic (interpret, reinterpret)
import           Effectful.Error.Static     (Error, throwError)
import           Effectful.Reader.Dynamic   (Reader, runReader)

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
    GetManyComments (ID start) (ID end) sortMethod -> withConn $ do
      toList . M.take (sortedRange sortMethod start end) . view store
        <$> liftIO (decodeFile filePath)

    GetComment cID -> do
      -- storage <- liftIO (decodeFile filePath)
      getCommentIfExists cID
      -- pure undefined

    NewComment comment -> do
      storage <- liftIO $ decodeFile filePath

      let newID = storage ^. nextID
          updatedStorage =
            storage
              & store %~ M.insert newID comment
                -- This is the only time that a comment is added, so just increment the ID!
                -- Security-wise it's fine since obviously this should not be used in production.
              & nextID +~ 1

      liftIO $ encodeFile filePath updatedStorage
      pure newID

    EditComment cID f -> do
      storage <- liftIO $ decodeFile filePath
      let updatedStorage = storage & store %~ M.adjust f cID
      liftIO $ encodeFile filePath updatedStorage
      -- getCommentIfExists cID updatedStorage
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

getCommentIfExists
  :: ( Error StorageError :> es
     , IOE :> es
     , SqlPool :> es
     )
  => ID -> Eff es Comment
getCommentIfExists _ = withConn $ do
  maybeComment <- selectFirst [Message ==. "First"] []
  lift $ case maybeComment of
    Just (Entity _ c) -> pure c
    Nothing           -> throwError CommentNotFound

sortedRange :: SortBy -> Int -> Int -> Int
sortedRange sortMethod start end = case sortMethod of
  Popular       -> end - start
  Controversial -> end - start
  Old           -> end - start
  New           -> end - start
