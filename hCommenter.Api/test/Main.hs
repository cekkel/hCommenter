module Main (main) where

import System.Environment (setEnv)

import ApiRules.HttpClient (runInstanceRuleTests)
import ApiRules.Wai

main :: IO ()
main = do
  setEnv "LOGGING__SEVERITY" "Critical" -- So that almost all logs are hidden, except critical (unexpected) failures
  setEnv "API_ENVIRONMENT" "Development" -- To ensure that logs are written to the console
  runWaiRuleTests
  runInstanceRuleTests
