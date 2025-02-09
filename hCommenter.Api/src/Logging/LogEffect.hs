{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging.LogEffect where

import Control.Monad.Logger
  ( Loc
  , LogLevel (..)
  , LogSource
  , ToLogStr (toLogStr)
  , fromLogStr
  , toLogStr
  )
import Data.Aeson (Object, Value, object)
import Effectful
  ( Dispatch (Static)
  , DispatchOf
  , Eff
  , Effect
  , IOE
  , (:>)
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects)
  , StaticRep
  , evalStaticRep
  , getStaticRep
  , localStaticRep
  , unsafeEff_
  )
import Katip
import Optics
import Prelude hiding (log, singleton)

import Logging.LogContext (LogField, logFieldToObjectPair)
import Server.ServerTypes
  ( Env
  , appName
  , envName
  , scribe
  , scribeName
  )

data LogConfig = LogConfig
  { _logNamespace :: !Namespace
  , _logContext :: !LogContexts
  , _logEnv :: !LogEnv
  }

makeLenses ''LogConfig

data Log :: Effect

type instance DispatchOf Log = Static WithSideEffects

newtype instance StaticRep Log = Log LogConfig

-- | TODO: Verify performance.
runLog
  :: (IOE :> es)
  => Env
  -> Eff (Log : es) a
  -> Eff es a
runLog env logEff = do
  envWithScribe <- liftIO $ initLogEnvWithScribe env

  flip evalStaticRep logEff $
    Log
      LogConfig
        { _logNamespace = mempty
        , _logContext = mempty
        , _logEnv = envWithScribe
        }

initLogEnvWithScribe :: Env -> IO LogEnv
initLogEnvWithScribe env = do
  let
    component' = Namespace [env ^. appName]
    environment' = Environment $ env ^. envName

  initialEnv <- liftIO $ initLogEnv component' environment'
  liftIO $
    registerScribe
      (env ^. scribeName)
      (env ^. scribe)
      defaultScribeSettings
      initialEnv

getConsoleScribe :: IO Scribe
getConsoleScribe = mkHandleScribe (ColorLog True) stdout (const (pure True)) V0

getFileScribe :: IO Scribe
getFileScribe = mkFileScribe "logs.txt" (const $ pure True) V0

log :: (Log :> es) => Severity -> LogStr -> Eff es ()
log level msg = do
  f <- askForLoggerIO
  unsafeEff_ $ f level msg

logInfo :: (Log :> es) => LogStr -> Eff es ()
logInfo = log InfoS

logWarn :: (Log :> es) => LogStr -> Eff es ()
logWarn = log WarningS

logError :: (Log :> es) => LogStr -> Eff es ()
logError = log ErrorS

logExceptions :: (IOE :> es, Log :> es) => Eff es a -> Eff es a
logExceptions action = action `catchAny` \e -> logErr e >> throwIO e
 where
  logErr e = logError ("An exception has occurred: " <> showLS e)

addLogNamespace :: (Log :> es) => Namespace -> Eff es a -> Eff es a
addLogNamespace ns = localKatipNamespace' (<> ns)

addLogContext :: forall es a. (Log :> es) => [LogField] -> Eff es a -> Eff es a
addLogContext fields = localKatipContext' (<> liftPayload context)
 where
  context = object $ logFieldToObjectPair <$> fields

instance LogItem Object where
  payloadKeys _ _ = AllKeys

instance ToObject Value

instance LogItem Value where
  payloadKeys _ _ = AllKeys

-- logIO :: Env -> IO ()
-- logIO env = do
--   ctx <- getKatipContext'
--   ns <- getKatipNamespace'
--   env <- getLogEnv'
--   pure (\sev msg -> runKatipT env $ logF ctx ns sev msg)

askForLoggerIO :: (Log :> es) => Eff es (Severity -> LogStr -> IO ())
askForLoggerIO = do
  ctx <- getKatipContext'
  ns <- getKatipNamespace'
  env <- getLogEnv'
  pure (\sev msg -> runKatipT env $ logF ctx ns sev msg)

{-| This is useful for when there is a need to work with a library that uses the
  'LoggingT' transformer from the monad-logger library.
-}
askForMonadLoggerIO :: (Log :> es, ToLogStr a) => Eff es (Loc -> LogSource -> LogLevel -> a -> IO ())
askForMonadLoggerIO = do
  logIO <- askForLoggerIO
  pure (\_loc _src lvl msg -> logIO (mapLvl lvl) (mapMsg msg))
 where
  mapLvl = \case
    LevelInfo -> InfoS
    LevelWarn -> WarningS
    LevelDebug -> DebugS
    LevelError -> ErrorS
    LevelOther _ -> WarningS

  mapMsg = logStr . fromLogStr . toLogStr

instance (IOE :> es, Log :> es) => Katip (Eff es) where
  getLogEnv = getLogEnv'
  localLogEnv = localLogEnv'

instance (Katip (Eff es), Log :> es) => KatipContext (Eff es) where
  getKatipContext = getKatipContext'
  localKatipContext = localKatipContext'
  getKatipNamespace = getKatipNamespace'
  localKatipNamespace = localKatipNamespace'

getKatipContext' :: (Log :> es) => Eff es LogContexts
getKatipContext' = view logContext <$> getConfig

localKatipContext' :: (Log :> es) => (LogContexts -> LogContexts) -> Eff es a -> Eff es a
localKatipContext' f = localConfig (logContext %~ f)

getKatipNamespace' :: (Log :> es) => Eff es Namespace
getKatipNamespace' = view logNamespace <$> getConfig

localKatipNamespace' :: (Log :> es) => (Namespace -> Namespace) -> Eff es a -> Eff es a
localKatipNamespace' f = localConfig (logNamespace %~ f)

getLogEnv' :: (Log :> es) => Eff es LogEnv
getLogEnv' = view logEnv <$> getConfig

localLogEnv' :: (Log :> es) => (LogEnv -> LogEnv) -> Eff es a -> Eff es a
localLogEnv' f = localConfig (logEnv %~ f)

getConfig :: (Log :> es) => Eff es LogConfig
getConfig = do
  Log conf <- getStaticRep
  pure conf

localConfig :: (Log :> es) => (LogConfig -> LogConfig) -> Eff es a -> Eff es a
localConfig f = localStaticRep $ \(Log config) -> Log (f config)
