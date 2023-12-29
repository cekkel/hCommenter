{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Database.SqlPool where
import           ClassyPrelude              hiding (Reader, ask)
import           Control.Monad.Logger       (LoggingT (runLoggingT))
import           Data.Pool                  (Pool, withResource)
import           Database.Persist.Sql       (SqlBackend, runMigration)
import           Database.Persist.Sqlite    (withSqlitePool)
import           Database.StorageTypes      (migrateAll)
import           Effectful                  (Eff, Effect, IOE,
                                             Limit (Unlimited),
                                             Persistence (Ephemeral),
                                             UnliftStrategy (ConcUnlift),
                                             withEffToIO, (:>))
import           Effectful.Dispatch.Dynamic (localSeqUnliftIO, reinterpret)
import           Effectful.Reader.Static    (Reader, ask, runReader)
import           Effectful.TH               (makeEffect)
import           Logging                    (Log, askForMonadLoggerIO)

data SqlPool :: Effect where
  WithConn :: ReaderT SqlBackend m a -> SqlPool m a

makeEffect ''SqlPool

runSqlPool
  :: ( Log :> es
     , IOE :> es
     )
  => Eff (SqlPool : es) a
  -> Eff es a
runSqlPool = reinterpret initSqlPool $ \env -> \case
  WithConn action -> do
    pool <- ask
    localSeqUnliftIO env $ \unlift -> do
      withResource pool $ unlift . runReaderT action

initSqlPool
  :: ( Log :> es
     , IOE :> es
     )
  => Eff (Reader (Pool SqlBackend) : es) a
  -> Eff es a
initSqlPool action = do
  logIO <- askForMonadLoggerIO
  flip runLoggingT logIO $
    withSqlitePool "" 2 $ \pool -> do
      lift $ withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
        liftIO $ withResource pool $ runReaderT $ runMigration migrateAll
        unlift $ runReader pool action
