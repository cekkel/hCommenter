{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL8 (writeFile)
import           Network.Wai.Handler.Warp   (run)
import           Options.Commander          (command_, raw, toplevel, optDef)
import           Server                     (app, swaggerDefinition, initialiseLocalFile, Backend (..))
import           Text.Read                  (readMaybe)

main :: IO ()
main = command_ . toplevel @"hCommenter CLI"
  . optDef @"p" @"port" "8080" $ \(portOpt :: String) ->
    optDef @"m" @"mode" "local" $ \(modeOpt :: String) -> do
    raw $ case readMaybe portOpt of
      Nothing -> putStrLn "\nInvalid port value."
      Just port -> case modeOpt of
        "swagger" -> BL8.writeFile "swagger.json" swaggerDefinition
        "static"  -> messageConsoleAndRun port Static
        "local"   -> initialiseLocalFile >> messageConsoleAndRun port LocalFile
        "prod"    -> messageConsoleAndRun port ToBeDeterminedProd
        other     -> putStrLn $ "\nInvalid Mode: " <> other

messageConsoleAndRun :: Int -> Backend -> IO ()
messageConsoleAndRun port backend = do
  putStrLn $ "\nListening in " <> show backend <> " mode, on port " <> show port <> "...\n"
  run port $ app backend
