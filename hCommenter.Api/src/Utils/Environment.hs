{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Utils.Environment (LoggingConf (..), Env (..), readEnv) where

import Control.Monad.Logger (LoggingT, MonadLoggerIO)
import Data.Pool (Pool)
import Database.Persist.Sqlite (SqlBackend, createSqlitePool)
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
  , pool :: LoggingT IO (Pool SqlBackend)
  -- ^ Keep the LoggingT wrapper so that logging can be injected with context as needed.
  , appName :: !Text
  , envName :: !Text
  , logging :: !LoggingConf
  }

makeFieldLabelsNoPrefix ''Env

{-| Read in all environment variables to be used throughout the execution of the application.
Includes defaults where appropriate, and anything else that should be initialised only once.
-}
readEnv :: IO Env
readEnv = do
  appName <- pack <$> getEnv "APP__NAME"
  envName <- pack <$> getEnv "APP__ENVIRONMENT"
  mPort <- readMay <$> getEnv "APP__PORT"
  mBackend <- readMay <$> getEnv "APP__BACKEND"

  let
    backend = fromMaybe (error "Backend provided in 'APP__BACKEND' is missing or invalid") mBackend
    port = fromMaybe (error "Port provided in 'APP__PORT' is missing or invalid") mPort

  loggingConf <- readLoggingConf appName envName

  pure $
    Env
      { backend
      , port
      , pool = createCustomSqlPool backend
      , appName
      , envName
      , logging = loggingConf
      }

createCustomSqlPool :: (MonadLoggerIO m, MonadUnliftIO m) => Backend -> m (Pool SqlBackend)
createCustomSqlPool = \case
  SQLite -> createSqlitePool "sqliteStorage.db" 2
  _ -> error "Only the SQLite backend is supported currently."
