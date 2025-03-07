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
  ( HasCallStack
  , SideEffects (WithSideEffects)
  , StaticRep
  , evalStaticRep
  , getStaticRep
  , localStaticRep
  , unsafeEff_
  )
import Katip
import Optics
import PyF (PyFCategory (PyFString), PyFClassify)
import Prelude hiding (log, singleton)

import Logging.LogContext (LogField, logFieldToObjectPair)
import Utils.Environment

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
  flip evalStaticRep logEff $
    Log
      LogConfig
        { _logNamespace = mempty
        , _logContext = mempty
        , _logEnv = env ^. #logging % #katipLogEnv
        }

getFileScribe :: IO Scribe
getFileScribe = mkFileScribe "logs.txt" (const $ pure True) V0

type instance PyFClassify LogStr = 'PyFString

log :: (Log :> es) => Severity -> LogStr -> Eff es ()
log level msg = do
  f <- askForLoggerIO
  unsafeEff_ $ f Nothing level msg

logInfo :: (Log :> es) => LogStr -> Eff es ()
logInfo = log InfoS

logWarn :: (Log :> es) => LogStr -> Eff es ()
logWarn = log WarningS

logError :: (Log :> es) => LogStr -> Eff es ()
logError = log ErrorS

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
--

askForLoggerIO :: (HasCallStack, Log :> es) => Eff es (Maybe Loc -> Severity -> LogStr -> IO ())
askForLoggerIO = do
  ctx <- getKatipContext'
  ns <- getKatipNamespace'
  env <- getLogEnv'
  -- Not using location-based logging for now, as katip's support wrapper funcs with it is limited.
  pure (\maybe_loc sev msg -> runKatipT env $ logF ctx ns sev msg)

{-| This is useful for when there is a need to work with a library that uses the
  'LoggingT' transformer from the monad-logger library.
-}
askForMonadLoggerIO :: (Log :> es, ToLogStr a) => Eff es (Loc -> LogSource -> LogLevel -> a -> IO ())
askForMonadLoggerIO = do
  logIO <- askForLoggerIO
  pure (\_loc _src lvl msg -> logIO (Just _loc) (mapLvl lvl) (mapMsg msg))
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

-- instance (KatipContext m) => MonadLogger m where
--  monadLoggerLog = defaultMonadLoggerLog katipContextLogItem

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

-- | The type of 'monadLoggerLog'.
type MonadLoggerLog m = forall msg. (ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> m ()

{-| Default implementation for 'monadLoggerLog'.


The first argument is a handler for generated log messages. In general,
you can simply use 'katipLogItem' or 'katipContextLogItem', depending on
the level of functionality your monad provides.
-}

-- defaultMonadLoggerLog
--  :: (LogItem a, MonadIO m)
--  => a
--  -- ^ Handler for generated log messages
--  -> MonadLoggerLog m
-- defaultMonadLoggerLog logItem' loc src level msg = case mapLogLevel level of
--  Right level' -> logItem' src' loc' level' msg'
--  Left level' -> logLevelOther logItem' src' loc' level' msg'
-- where
--  src'
--    | Text.null src = Nothing -- Short-circuit common case
--    | otherwise = Just $ Namespace $ Text.splitOn dot src
--  -- Don't pass loc if it's defaultLoc (i.e. not really passed to monadLoggerLog)
--  -- Common case is to have a location (when using the TH splices)
--  loc' = if loc /= defaultLoc then Just loc else Nothing
--  msg' = logStr $ fromLogStr $ toLogStr msg
--  mapLogLevel :: LogLevel -> Either Text Severity
--  mapLogLevel l = case l of
--    LevelDebug -> Right DebugS
--    LevelInfo -> Right InfoS
--    LevelWarn -> Right WarningS
--    LevelError -> Right ErrorS
--    LevelOther lvl -> Left lvl
--  dot = Text.singleton '.'
-- {-# INLINE defaultMonadLoggerLog #-}
--
---- | Handler for 'defaultMonadLoggerLog' which logs messages in a 'KatipContext' environment using 'logItemM'.
-- katipContextLogItem :: (KatipContext m, LogItem a) => Maybe a
-- katipContextLogItem n = case n of
--  Nothing -> logItemM
--  Just n' -> \l s m -> katipAddNamespace n' $ logItemM l s m
-- {-# INLINE katipContextLogItem #-}
