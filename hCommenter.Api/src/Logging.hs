{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging where

import           ClassyPrelude             hiding (log, singleton)
import           Control.Lens              (makeLenses, view, (%~))
import           Data.Aeson                (Key, Object, ToJSON (toJSON), Value)
import           Data.Aeson.KeyMap         (singleton)
import           Effectful                 (Dispatch (Static), DispatchOf, Eff,
                                            Effect, IOE, (:>))
import           Effectful.Dispatch.Static (SideEffects (WithSideEffects),
                                            StaticRep, evalStaticRep,
                                            getStaticRep, localStaticRep,
                                            unsafeEff_)
import           Katip

data LogConfig = LogConfig {
  _logNamespace :: !Namespace,
  _logContext   :: !LogContexts,
  _logEnv       :: !LogEnv
}

makeLenses ''LogConfig

data Log :: Effect where

type instance DispatchOf Log = Static WithSideEffects
newtype instance StaticRep Log = Log LogConfig

runLog
  :: (IOE :> es)
  => Text
  -- ^ Application component name
  -> Text
  -- ^ Environment (e.g. prod vs test)
  -> Text
  -- ^ Name of backend
  -> Scribe
  -- ^ Backend to use, e.g. console, file, newrelic
  -> Eff (Log : es) a
  -> Eff es a
runLog component environment backendName backend logEff = do
  let component' = Namespace [component]
      environment' = Environment environment

  initialEnv <- liftIO $ initLogEnv component' environment'
  envWithScribe <- liftIO $ registerScribe backendName backend defaultScribeSettings initialEnv

  flip evalStaticRep logEff $ Log LogConfig {
    _logNamespace = mempty
  , _logContext = mempty
  , _logEnv = envWithScribe
  }

getConsoleScribe :: Verbosity -> IO Scribe
getConsoleScribe = mkHandleScribe (ColorLog True) stdout (const (pure True))

getFileScribe :: Verbosity -> IO Scribe
getFileScribe = mkFileScribe "logs.txt" (const $ pure True)

log :: Log :> es => Severity -> LogStr -> Eff es ()
log level msg = do
  f <- askForLoggerIO
  unsafeEff_ $ f level msg

logInfo :: Log :> es => LogStr -> Eff es ()
logInfo = log InfoS

logWarn :: Log :> es => LogStr -> Eff es ()
logWarn = log WarningS

logError :: Log :> es => LogStr -> Eff es ()
logError = log ErrorS

logExceptions :: (IOE :> es, Log :> es) => Eff es a -> Eff es a
logExceptions action = action `catchAny` \e -> logErr e >> throwIO e
  where
    logErr e = logFM ErrorS ("An exception has occurred: " <> showLS e)

addLogNamespace :: Log :> es => Namespace -> Eff es a -> Eff es a
addLogNamespace ns = localKatipNamespace' (<> ns)

addLogContext :: (Log :> es, LogItem i) => i -> Eff es a -> Eff es a
addLogContext item = localKatipContext' (<> liftPayload item)

toObj :: (ToJSON a) => Key -> a -> Object
toObj name = singleton name . toJSON

instance LogItem Object where
  payloadKeys _ _ = AllKeys

instance ToObject Value
instance LogItem Value where
  payloadKeys _ _ = AllKeys

askForLoggerIO :: (Log :> es) => Eff es (Severity -> LogStr -> IO ())
askForLoggerIO = do
  ctx <- getKatipContext'
  ns <- getKatipNamespace'
  env <- getLogEnv'
  pure (\sev msg -> runKatipT env $ logF ctx ns sev msg)

instance (IOE :> es, Log :> es) => Katip (Eff es) where
  getLogEnv = getLogEnv'
  localLogEnv = localLogEnv'

instance (Log :> es, Katip (Eff es)) => KatipContext (Eff es) where
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
