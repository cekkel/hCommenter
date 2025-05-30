{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging.Config where

import Katip
  ( ColorStrategy (ColorIfTerminal)
  , Environment (Environment)
  , LogEnv
  , Namespace (Namespace)
  , Scribe
  , Severity (InfoS)
  , Verbosity (V3)
  , defaultScribeSettings
  , initLogEnv
  , mkHandleScribe
  , permitItem
  , registerScribe
  )
import Optics
import System.Environment (getEnv)

import Katip qualified as K

import Logging.Scribes.Grafana (GrafanaConf, mkGrafanaScribe, readGrafanaConf)

data LoggingConf = LoggingConf
  { grafanaConf :: !GrafanaConf
  , katipLogEnv :: !LogEnv
  , scribe :: !Scribe
  }

makeFieldLabelsNoPrefix ''LoggingConf

getConsoleScribe :: Severity -> Verbosity -> IO Scribe
getConsoleScribe severity = mkHandleScribe ColorIfTerminal stdout (permitItem severity)

readLoggingConf :: Text -> Text -> IO LoggingConf
readLoggingConf appName envName = do
  severity <- fromMaybe InfoS . K.textToSeverity . pack <$> getEnv "LOGGING__SEVERITY"
  verbosity <- fromMaybe V3 . readMaybe <$> getEnv "LOGGING__VERBOSITY"

  let
    component' = Namespace [appName]
    environment' = Environment envName
    scribeName = "GrafanaScribe"

  grafanaConf <- readGrafanaConf
  scribe <- case envName of
    "Development" -> getConsoleScribe severity verbosity
    _ -> mkGrafanaScribe grafanaConf (permitItem severity) verbosity

  initialEnv <- initLogEnv component' environment'
  katipLogEnv <- registerScribe scribeName scribe defaultScribeSettings initialEnv

  pure LoggingConf {..}
