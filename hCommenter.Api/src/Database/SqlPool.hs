{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database.SqlPool (SqlPool, withConn, runSqlPool) where

import Prelude hiding (Reader, ask)
import Control.Monad.Logger (LoggingT (runLoggingT))
import Data.Pool (Pool, withResource)
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (withSqlitePool)
import Database.StorageTypes (migrateAll)
import Effectful
  ( Eff
  , Effect
  , IOE
  , Limit (Unlimited)
  , Persistence (Ephemeral)
  , UnliftStrategy (ConcUnlift)
  , withEffToIO
  , (:>)
  )
import Effectful.Dispatch.Dynamic (localSeqUnliftIO, reinterpret)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.TH (makeEffect)
import Logging.LogEffect (Log, askForMonadLoggerIO)
import Server.ServerTypes (Backend (LocalFile, SQLite, ToBeDeterminedProd))

data SqlPool :: Effect where
  WithConn :: ReaderT SqlBackend m a -> SqlPool m a

makeEffect ''SqlPool

runSqlPool ::
  ( Log :> es
  , IOE :> es
  ) =>
  Backend ->
  Eff (SqlPool : es) a ->
  Eff es a
runSqlPool backend = reinterpret initSqlPool $ \env -> \case
  WithConn action -> do
    pool <- ask
    localSeqUnliftIO env $ \unlift -> do
      withResource pool $ unlift . runReaderT action
  where
    initSqlPool = case backend of
      LocalFile -> error "SqlPool is not implemented for 'LocalFile' backend (this error should never be shown)"
      SQLite -> initSqlitePool
      ToBeDeterminedProd -> initSqlitePool

initSqlitePool ::
  ( Log :> es
  , IOE :> es
  ) =>
  Eff (Reader (Pool SqlBackend) : es) a ->
  Eff es a
initSqlitePool action = do
  logIO <- askForMonadLoggerIO
  flip runLoggingT logIO $
    withSqlitePool "sqliteStorage.db" 2 $ \pool -> do
      lift $ withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
        liftIO $ withResource pool $ runReaderT $ runMigration migrateAll
        unlift $ runReader pool action
