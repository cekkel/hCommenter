{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Options.Commander (command_, optDef, raw, toplevel)
import Text.Read (readMaybe)

import Server (Backend (..), Env (Env), app, getConsoleScribe, initDevSqliteDB, mkEnv)

main :: IO ()
main = command_
  . toplevel @"hCommenter CLI"
  . optDef @"p" @"port" "8080"
  $ \(portOpt :: String) ->
    optDef @"m" @"mode" "sqlite" $ \(modeOpt :: String) -> do
      raw $ case readMaybe portOpt of
        Nothing -> putStrLn "\nInvalid port value."
        Just port -> case modeOpt of
          "binary" -> messageConsoleAndRun port LocalFile
          "sqlite" -> messageConsoleAndRun port SQLite
          "prod" -> messageConsoleAndRun port ToBeDeterminedProd
          other -> putStrLn $ "\nInvalid Mode: " <> pack other

messageConsoleAndRun :: Int -> Backend -> IO ()
messageConsoleAndRun port backend = do
  env <- mkEnv

  case backend of
    SQLite -> initDevSqliteDB backend env
    LocalFile -> pure ()
    ToBeDeterminedProd -> pure ()

  putStrLn $ "\nListening in " <> tshow backend <> " mode, on port " <> tshow port <> "...\n"
  run port $ app env
