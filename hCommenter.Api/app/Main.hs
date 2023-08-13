{-# LANGUAGE DataKinds #-}
module Main where
import qualified Data.ByteString.Lazy.Char8 as BL8 (writeFile)
import           Network.Wai.Handler.Warp   (run)
import           Options.Commander          (command_, flag, raw, toplevel)
import           Server                     (app, swaggerDefinition)

main :: IO ()
main = command_
  . toplevel @"hCommenter CLI"
  $ flag @"generateSwagger" $ \wantsSwagger ->
    raw $ if wantsSwagger
        then BL8.writeFile "swagger.json" swaggerDefinition
        else run 8080 app
