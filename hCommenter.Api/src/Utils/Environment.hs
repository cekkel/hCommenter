module Utils.Environment where

import System.Environment (getEnv)

getSentryDSN :: IO String
getSentryDSN = getEnv "SENTRY_DSN"

getAppEnv :: IO Text
getAppEnv = pack <$> getEnv "APP_ENVIRONMENT"
