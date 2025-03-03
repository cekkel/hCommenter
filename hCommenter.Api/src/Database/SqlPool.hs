{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database.SqlPool (SqlPool, withConn, runSqlPool) where

import Control.Monad.Logger (LoggingT (runLoggingT))
import Data.Pool (Pool, withResource)
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (withSqlitePool)
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
import Effectful.TH (makeEffect)
import Optics
import Prelude hiding (Reader, ask)

import Effectful.Reader.Static qualified as ES

import Database.StorageTypes (migrateAll)
import Logging.LogContext (LogField (CorrelationID, Note))
import Logging.LogEffect (Log, addLogContext, addLogNamespace, askForMonadLoggerIO)
import Server.ServerTypes (Backend (LocalFile, SQLite, ToBeDeterminedProd))
import Utils.AppContext (AppContext (correlationId))

data SqlPool :: Effect where
  WithConn :: ReaderT SqlBackend m a -> SqlPool m a

makeEffect ''SqlPool

runSqlPool
  :: ( ES.Reader AppContext :> es
     , IOE :> es
     , Log :> es
     )
  => Backend
  -> Eff (SqlPool : es) a
  -> Eff es a
runSqlPool backend = reinterpret initSqlPool $ \env -> \case
  WithConn action -> do
    pool <- ES.ask
    localSeqUnliftIO env $ \unlift -> do
      withResource pool $ unlift . runReaderT action
 where
  initSqlPool = case backend of
    LocalFile -> error "SqlPool is not implemented for 'LocalFile' backend (this error should never be shown)"
    SQLite -> initSqlitePool
    ToBeDeterminedProd -> initSqlitePool

-- | TODO: Might need to be moved to startup, if there are performance problems.
initSqlitePool
  :: ( ES.Reader AppContext :> es
     , IOE :> es
     , Log :> es
     )
  => Eff (ES.Reader (Pool SqlBackend) : es) a
  -> Eff es a
initSqlitePool action = do
  corrId <- ES.asks $ view #correlationId

  -- Need to add correlation id here because otherwise sometimes a monad-logger log escapes the context later.
  addLogContext [CorrelationID corrId] $ do
    logIO <- askForMonadLoggerIO
    flip runLoggingT logIO $
      withSqlitePool "sqliteStorage.db" 2 $ \pool -> do
        lift $ withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
          liftIO $ withResource pool $ runReaderT $ runMigration migrateAll
          unlift $ ES.runReader pool action
