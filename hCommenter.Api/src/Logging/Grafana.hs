module Logging.Grafana where

import Data.Aeson
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as Builder
import Katip qualified as K
import Network.HTTP.Req
import Network.HTTP.Req (POST (POST))

-- curl -X POST -H "Content-Type: application/json" -H "Authorization: Bearer <LOGS_USERID>:<API_KEY>" -d '{"streams": [{"stream": {"Language": "Curl", "source": "Shell"},"values": [["'"$(($(date +%s)*1000000000))"'", "This is my log line"]]}]}' <URL>

mkGrafanaScribe :: K.PermitFunc -> K.Verbosity -> IO K.Scribe
mkGrafanaScribe permitF verbosity = do
  pure $ K.Scribe{scribePermitItem = permitF, scribeFinalizer = pure (), liPush = pushLog}

pushLog :: forall a. (K.LogItem a) => K.Item a -> IO ()
pushLog item = runReq defaultHttpConfig $ do
  let
    options =
      mconcat
        [ header "Authorization" $ "Bearer 1094195:" <> apiToken
        , header "Content-Type" "application/json"
        ]

  r <-
    req
      POST
      (https "logs-prod-026.grafana.net")
      (ReqBodyJson $ encodeLoki item)
      (jsonResponse)
      options

  liftIO $ print (responseBody r :: Value)

apiToken :: (IsString a) => a
apiToken =
  ""

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
