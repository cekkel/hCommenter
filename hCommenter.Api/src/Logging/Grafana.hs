{-# LANGUAGE QuasiQuotes #-}

module Logging.Grafana where

import Data.Aeson
import Network.HTTP.Req
import Network.HTTP.Req (POST (POST))
import Optics
import PyF (fmt)

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as Builder
import Katip qualified as K

import Utils.Environment (Env (logging), LoggingConf (grafanaAccountNum, grafanaUrl))

-- curl -X POST -H "Content-Type: application/json" -H "Authorization: Bearer <LOGS_USERID>:<API_KEY>" -d '{"streams": [{"stream": {"Language": "Curl", "source": "Shell"},"values": [["'"$(($(date +%s)*1000000000))"'", "This is my log line"]]}]}' <URL>

mkGrafanaScribe :: Env -> K.PermitFunc -> K.Verbosity -> IO K.Scribe
mkGrafanaScribe env permitF verbosity = do
  pure $ K.Scribe {scribePermitItem = permitF, scribeFinalizer = pure (), liPush = pushLog env}

pushLog :: forall a. (K.LogItem a) => Env -> K.Item a -> IO ()
pushLog env item = runReq defaultHttpConfig $ do
  let
    url = env ^. #logging % #grafanaUrl
    accountNum = env ^. #logging % #grafanaAccountNum
    apiToken = env ^. #logging % #grafanaToken

    options =
      mconcat
        [ header "Authorization" [fmt|Bearer {accountNum}:{apiToken}|]
        , header "Content-Type" "application/json"
        ]

  r <-
    req
      POST
      (https url)
      (ReqBodyJson $ encodeLoki item)
      (jsonResponse)
      options

  liftIO $ print (responseBody r :: Value)

encodeLoki :: (K.LogItem a) => K.Item a -> Value
encodeLoki item =
  let
    msg = toStrict $ Builder.toLazyText $ K.unLogStr $ K._itemMessage item
  in
    object $
      [ "streams"
          .= [ object $
                 [ "stream"
                     .= ( object $
                            [ "Language" .= ("Haskell" :: Text)
                            , "source" .= ("Application" :: Text)
                            ]
                        )
                 , "values"
                     .= [
                          [ "$(($(date +%s)*1000000000))" :: Text
                          , msg
                          ]
                        ]
                 ]
             ]
      ]
