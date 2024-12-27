{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Options.Commander (command_, optDef, raw, toplevel)
import Server (Backend (..), Env (Env), app, getConsoleScribe, initDevSqliteDB, initialiseLocalFile)
import Text.Read (readMaybe)

main :: IO ()
main = command_
  . toplevel @"hCommenter CLI"
  . optDef @"p" @"port" "5001"
  $ \(portOpt :: String) ->
    optDef @"m" @"mode" "binary" $ \(modeOpt :: String) -> do
      raw $ case readMaybe portOpt of
        Nothing -> putStrLn "\nInvalid port value."
        Just port -> case modeOpt of
          "binary" -> initialiseLocalFile >> messageConsoleAndRun port LocalFile
          "sqlite" -> messageConsoleAndRun port SQLite
          "prod" -> messageConsoleAndRun port ToBeDeterminedProd
          other -> putStrLn $ "\nInvalid Mode: " <> other

messageConsoleAndRun :: Int -> Backend -> IO ()
messageConsoleAndRun port backend = do
  scribe <- getConsoleScribe

  let env = Env backend "hCommenter.Api" "Dev" "Console" scribe

  case backend of
    SQLite -> initDevSqliteDB backend env
    LocalFile -> initialiseLocalFile
    ToBeDeterminedProd -> pure ()

  putStrLn $ "\nListening in " <> show backend <> " mode, on port " <> show port <> "...\n"
  run port $ app env
