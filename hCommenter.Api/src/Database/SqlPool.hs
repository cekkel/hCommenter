{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database.SqlPool (SqlPool, withConn, runSqlPool) where

import Control.Monad.Logger (LoggingT (runLoggingT))
import Data.Pool (withResource)
import Database.Persist.Sql (SqlBackend)
import Effectful
  ( Eff
  , Effect
  , IOE
  , (:>)
  )
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import Effectful.TH (makeEffect)
import Optics
import Prelude hiding (Reader, ask)

import Effectful.Reader.Static qualified as ES

import Logging.LogEffect (Log, askForMonadLoggerIO)
import Utils.RequestContext (RequestContext)

data SqlPool :: Effect where
  WithConn :: ReaderT SqlBackend m a -> SqlPool m a

makeEffect ''SqlPool

runSqlPool
  :: ( ES.Reader RequestContext :> es
     , IOE :> es
     , Log :> es
     )
  => Eff (SqlPool : es) a
  -> Eff es a
runSqlPool =
  interpret $ \effEnv -> \case
    WithConn action -> do
      wrappedPool <- ES.asks $ view (#env % #pool)
      logIO <- askForMonadLoggerIO
      pool <- liftIO $ runLoggingT wrappedPool logIO

      localSeqUnliftIO effEnv $ \unlift -> do
        withResource pool $ unlift . runReaderT action
