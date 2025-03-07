{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Environment (LoggingConf (..), Env (..), readEnv) where

import Katip
  ( ColorStrategy (ColorIfTerminal)
  , Environment (Environment)
  , LogEnv
  , Namespace (Namespace)
  , Scribe
  , Severity (DebugS)
  , Verbosity (V0)
  , defaultScribeSettings
  , initLogEnv
  , mkHandleScribe
  , permitItem
  , registerScribe
  )
import Optics
import System.Environment (getEnv)

import Server.ServerTypes (Backend (SQLite))

data LoggingConf = LoggingConf
  { grafanaAccountNum :: !Text
  , grafanaToken :: !Text
  , grafanaUrl :: !Text
  , scribe :: !Scribe
  , katipLogEnv :: !LogEnv
  }

makeFieldLabelsNoPrefix ''LoggingConf

getConsoleScribe :: IO Scribe
getConsoleScribe = mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V0

readLoggingConf :: Text -> Text -> IO LoggingConf
readLoggingConf appName envName = do
  grafanaAccountNum <- pack <$> getEnv "LOGGING__GRAFANA_ACC"
  grafanaToken <- pack <$> getEnv "LOGGING__GRAFANA_TOKEN"
  grafanaUrl <- pack <$> getEnv "LOGGING__GRAFANA_URL"

  let
    component' = Namespace [appName]
    environment' = Environment $ envName
    scribeName = "GrafanaScribe"

  scribe <- getConsoleScribe
  initialEnv <- initLogEnv component' environment'
  katipLogEnv <- registerScribe scribeName scribe defaultScribeSettings initialEnv

  pure $
    LoggingConf
      { grafanaToken
      , grafanaAccountNum
      , grafanaUrl
      , scribe
      , katipLogEnv
      }

data Env = Env
  { backend :: !Backend
  , port :: !Int
  , appName :: !Text
  , envName :: !Text
  , logging :: !LoggingConf
  }

makeFieldLabelsNoPrefix ''Env

{-| Read in all environment variables to be used throughout the execution of the application.
Includes defaults where appropriate, and anything else that should be initialised once.
-}
readEnv :: IO Env
readEnv = do
  appName <- pack <$> getEnv "APP__NAME"
  envName <- pack <$> getEnv "APP__ENVIRONMENT"
  port <- readMay <$> getEnv "APP__PORT"
  backend <- readMay <$> getEnv "APP__BACKEND"

  -- TODO: Replace these with 'warn' logs
  when (isNothing backend) $
    error "Backend provided in 'APP__BACKEND' is missing or invalid"

  when (isNothing port) $
    error "Port provided in 'APP__PORT' is missing or invalid"

  loggingConf <- readLoggingConf appName envName

  pure $
    Env
      { backend = fromMaybe SQLite backend
      , port = fromMaybe 8080 port
      , appName
      , envName
      , logging = loggingConf
      }
