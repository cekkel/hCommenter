{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging.Config where

import Katip
  ( ColorStrategy (ColorIfTerminal)
  , Environment (Environment)
  , LogEnv
  , Namespace (Namespace)
  , Scribe
  , Severity (DebugS, InfoS)
  , Verbosity (V0)
  , defaultScribeSettings
  , initLogEnv
  , mkHandleScribe
  , permitItem
  , registerScribe
  )
import Optics
import System.Environment (getEnv)

import Logging.Grafana (GrafanaConf, mkGrafanaScribe, readGrafanaConf)

data LoggingConf = LoggingConf
  { grafanaConf :: !GrafanaConf
  , katipLogEnv :: !LogEnv
  , scribe :: !Scribe
  }

makeFieldLabelsNoPrefix ''LoggingConf

getConsoleScribe :: IO Scribe
getConsoleScribe = mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V0

readLoggingConf :: Text -> Text -> IO LoggingConf
readLoggingConf appName envName = do
  severity <- fromMaybe InfoS . readMay <$> getEnv "LOGGING__SEVERITY"

  let
    component' = Namespace [appName]
    environment' = Environment $ envName
    scribeName = "GrafanaScribe"

  grafanaConf <- readGrafanaConf
  scribe <- case envName of
    "Development" -> getConsoleScribe
    _ -> mkGrafanaScribe grafanaConf (permitItem severity) V0

  initialEnv <- initLogEnv component' environment'
  katipLogEnv <- registerScribe scribeName scribe defaultScribeSettings initialEnv

  pure LoggingConf {..}
