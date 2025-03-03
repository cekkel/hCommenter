{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Environment (LoggingConf (..), Env (..), readEnv) where

import Katip (ColorStrategy (ColorIfTerminal, ColorLog), Scribe, Severity (DebugS, InfoS), Verbosity (V0), mkHandleScribe, permitItem)
import Optics (makeFieldLabelsNoPrefix, makeLenses)
import System.Environment (getEnv)

import Server.ServerTypes (Backend (SQLite))

data LoggingConf = LoggingConf
  { grafanaAccountNum :: !Text
  , grafanaToken :: !Text
  , grafanaUrl :: !Text
  }

makeFieldLabelsNoPrefix ''LoggingConf

readLoggingConf :: IO LoggingConf
readLoggingConf = do
  grafanaAccountNum <- pack <$> getEnv "LOGGING__GRAFANA_ACC"
  grafanaToken <- pack <$> getEnv "LOGGING__GRAFANA_TOKEN"
  grafanaUrl <- pack <$> getEnv "LOGGING__GRAFANA_URL"

  pure $
    LoggingConf
      { grafanaToken
      , grafanaAccountNum
      , grafanaUrl
      }

data Env = Env
  { backend :: !Backend
  , appName :: !Text
  , envName :: !Text
  , logging :: !LoggingConf
  , scribeName :: !Text
  , scribe :: !Scribe
  }

makeFieldLabelsNoPrefix ''Env

getConsoleScribe :: IO Scribe
getConsoleScribe = mkHandleScribe (ColorIfTerminal) stdout (permitItem DebugS) V0

readEnv :: IO Env
readEnv = do
  scribe <- getConsoleScribe
  appEnv <- pack <$> getEnv "APP_ENVIRONMENT"

  loggingConf <- readLoggingConf
  -- sentryDSN <- getEnv "SENTRY_DSN"
  -- sentryService <- initRaven sentryDSN id sendRecord stderrFallback

  -- ravenScribe <- mkRavenScribe sentryService (const $ pure True) V3

  pure $
    Env
      { backend = SQLite
      , appName = "hCommenter.Api"
      , envName = appEnv
      , logging = loggingConf
      , scribeName = "Grafana"
      , scribe = scribe
      }
