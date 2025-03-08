module Logging.Utilities where

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
  ( Eff
  , (:>)
  )
import Effectful.Dispatch.Static
  ( HasCallStack
  , unsafeEff_
  )
import Katip
import Prelude hiding (log, singleton)

import Logging.LogContext (LogField, logFieldToObjectPair)
import Logging.LogEffect
  ( Log
  , getKatipContext'
  , getKatipNamespace'
  , getLogEnv'
  , localKatipContext'
  , localKatipNamespace'
  )

{- FYI
 - These functions are due to an issue (see a note in Logging.LogEffect for details)
 - which prevents us from using built-in katip functions. Instead, we just import
 - these functions instead.
 -}

askForLoggerIO :: (HasCallStack, Log :> es) => Eff es (Maybe Loc -> Severity -> LogStr -> IO ())
askForLoggerIO = do
  ctx <- getKatipContext'
  ns <- getKatipNamespace'
  env <- getLogEnv'
  -- Not using location-based logging for now, as katip's support for wrapper funcs with it is limited.
  pure (\maybe_loc sev msg -> runKatipT env $ logF ctx ns sev msg)

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
