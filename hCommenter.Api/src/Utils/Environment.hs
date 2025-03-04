{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Environment (LoggingConf (..), Env (..), readEnv) where

import Control.Exception (throw)
import Data.Maybe (fromJust)
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
  , port :: !Int
  , appName :: !Text
  , envName :: !Text
  , logging :: !LoggingConf
  , scribeName :: !Text
  , scribe :: !Scribe
  }

makeFieldLabelsNoPrefix ''Env

getConsoleScribe :: IO Scribe
getConsoleScribe = mkHandleScribe (ColorIfTerminal) stdout (permitItem DebugS) V0

{-| Read in all environment variables to be used throughout the execution of the application.
Includes defaults where appropriate, and anything else that should be initialised once.
-}
readEnv :: IO Env
readEnv = do
  scribe <- getConsoleScribe
  appName <- pack <$> getEnv "APP__NAME"
  envName <- pack <$> getEnv "APP__ENVIRONMENT"
  port <- readMay <$> getEnv "APP__PORT"
  backend <- readMay <$> getEnv "APP__BACKEND"

  -- TODO: Replace these with 'warn' logs
  when (isNothing backend) $
    error "Backend provided in 'APP__BACKEND' is missing or invalid"

  when (isNothing port) $
    error "Port provided in 'APP__PORT' is missing or invalid"

  loggingConf <- readLoggingConf

  pure $
    Env
      { backend = fromMaybe SQLite backend
      , port = fromMaybe 8080 port
      , appName
      , envName
      , logging = loggingConf
      , scribeName = "Grafana"
      , scribe
      }
