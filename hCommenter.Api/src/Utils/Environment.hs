{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Environment (Keys (..), Env (..), readEnv) where

import Katip (ColorStrategy (ColorLog), Scribe, Verbosity (V0), mkHandleScribe)
import Optics (makeFieldLabelsNoPrefix, makeLenses)
import System.Environment (getEnv)

import Server.ServerTypes (Backend (SQLite))

data Keys = Keys
  { grafanaToken :: !Text
  }

makeFieldLabelsNoPrefix ''Keys

readKeys :: IO Keys
readKeys = do
  grafanaToken <- pack <$> getEnv "GRAFANA_TOKEN"

  pure $ Keys grafanaToken

data Env = Env
  { backend :: !Backend
  , appName :: !Text
  , envName :: !Text
  , keys :: !Keys
  , scribeName :: !Text
  , scribe :: !Scribe
  }

makeFieldLabelsNoPrefix ''Env

getConsoleScribe :: IO Scribe
getConsoleScribe = mkHandleScribe (ColorLog True) stdout (const (pure True)) V0

readEnv :: IO Env
readEnv = do
  scribe <- getConsoleScribe
  appEnv <- pack <$> getEnv "APP_ENVIRONMENT"

  envKeys <- readKeys
  -- sentryDSN <- getEnv "SENTRY_DSN"
  -- sentryService <- initRaven sentryDSN id sendRecord stderrFallback

  -- ravenScribe <- mkRavenScribe sentryService (const $ pure True) V3

  pure $ Env SQLite "hCommenter.Api" appEnv envKeys "Raven" scribe
