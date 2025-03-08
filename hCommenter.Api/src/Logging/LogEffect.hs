{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging.LogEffect where

import Control.Monad.Logger
  ( Loc
  , LogLevel (..)
  , LogSource
  , ToLogStr
  )
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
  )
import Katip
import Optics
import PyF (PyFCategory (PyFString), PyFClassify)
import Prelude hiding (log, singleton)

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

-- Need the IOE constraint due to an unnecessary MonadIO constraint in the katip library.
-- See this PR: https://github.com/Soostone/katip/pull/119
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
