{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL8 (writeFile)
import           Network.Wai.Handler.Warp   (run)
import           Options.Commander          (command_, raw, toplevel, optDef)
import           Server                     (app, swaggerDefinition)
import Text.Read (readMaybe)

main :: IO ()
main = command_ . toplevel @"hCommenter CLI"
  . optDef @"p" @"port" "8080" $ \(portOpt :: String) ->
    optDef @"m" @"mode" "mock" $ \(modeOpt :: String) -> do
    raw $ case readMaybe portOpt of
      Nothing -> putStrLn "\nInvalid port value."
      Just port -> case modeOpt of
        "swagger" -> BL8.writeFile "swagger.json" swaggerDefinition
        "mock"    -> messageConsoleAndRun port
        "prod"    -> messageConsoleAndRun port
        other     -> putStrLn $ "\nInvalid Mode: " <> other

messageConsoleAndRun :: Int -> IO ()
messageConsoleAndRun port = do
  putStrLn $ "\nListening on port " <> show port <> "...\n"
  run port app
