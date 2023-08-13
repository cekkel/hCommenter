{-# LANGUAGE DataKinds #-}
module Main where
import           Control.Monad              (when)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           MyLib                      (swaggerDefinition)
import           Options.Commander          (command_, flag, raw, toplevel)

main :: IO ()
main = command_
  . toplevel @"hCommenter CLI"
  $ flag @"generateSwagger" $ \wantsSwagger ->
    raw $ when wantsSwagger
        $ BL8.writeFile "swagger.json" swaggerDefinition
