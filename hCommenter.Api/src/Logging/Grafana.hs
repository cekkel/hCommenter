{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging.Grafana where

import Data.Aeson
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Katip (Verbosity (V0))
import Network.HTTP.Req
import Optics
import PyF (fmt)
import System.Environment (getEnv)

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as AK
import Data.Text.Lazy.Builder qualified as Builder
import Katip qualified as K

data GrafanaConf = GrafanaConf
  { grafanaAccountNum :: !Text
  , grafanaToken :: !Text
  , grafanaUrl :: !Text
  }

readGrafanaConf :: IO GrafanaConf
readGrafanaConf = do
  grafanaAccountNum <- pack <$> getEnv "LOGGING__GRAFANA_ACC"
  grafanaToken <- pack <$> getEnv "LOGGING__GRAFANA_TOKEN"
  grafanaUrl <- pack <$> getEnv "LOGGING__GRAFANA_URL"

  pure $ GrafanaConf {..}

makeFieldLabelsNoPrefix ''GrafanaConf

mkGrafanaScribe :: GrafanaConf -> K.PermitFunc -> K.Verbosity -> IO K.Scribe
mkGrafanaScribe conf permitF verbosity = do
  pure $ K.Scribe {scribePermitItem = permitF, scribeFinalizer = pure (), liPush = pushLog conf}

pushLog :: forall a. (K.LogItem a) => GrafanaConf -> K.Item a -> IO ()
pushLog conf item = runReq defaultHttpConfig $ do
  let
    options =
      mconcat
        [ header "Authorization" [fmt|Bearer {conf ^. #grafanaAccountNum}:{conf ^. #grafanaToken}|]
        , header "Content-Type" "application/json"
        ]

  r <-
    req
      POST
      (https (conf ^. #grafanaUrl) /: "loki" /: "api" /: "v1" /: "push")
      (ReqBodyJson $ encodeLoki item)
      ignoreResponse
      options
      `catchAny` \e -> do
        putStrLn "Failed to push log to Grafana:"
        print e
        throwIO e

  when (responseStatusCode r /= 204) $ do
    putStrLn "Status code indicates failure to push log to Grafana:"
    print r

utcTimeToEpochTime :: UTCTime -> Int
utcTimeToEpochTime = round . (* 1e9) . utcTimeToPOSIXSeconds

encodeLoki :: (K.LogItem a) => K.Item a -> Value
encodeLoki item =
  let
    msg = toStrict $ Builder.toLazyText $ K.unLogStr $ K._itemMessage item
    logTime = tshow $ utcTimeToEpochTime $ K._itemTime item
    environment = K.getEnvironment $ K._itemEnv item
    jsonMsg =
      allFieldsText $
        foldl'
          AK.union
          mempty
          [ K.payloadObject V0 $ K._itemPayload item
          , AK.singleton "severity" $ A.String $ K.renderSeverity $ K._itemSeverity item
          ]
  in
    toJSON $
      Loki
        { streams =
            [ Stream
                { stream =
                    ( StreamInfo
                        { language = "Haskell"
                        , source = "API"
                        , environment
                        , service_name = "hCommenter"
                        }
                    )
                , values =
                    [(logTime, msg, jsonMsg)]
                }
            ]
        }

data Loki = Loki
  { streams :: [Stream]
  }
  deriving (Generic, Show, ToJSON)

data Stream = Stream
  { stream :: StreamInfo
  , values :: [(Text, Text, Object)]
  }
  deriving (Generic, Show, ToJSON)

data StreamInfo = StreamInfo
  { language :: Text
  , source :: Text
  , service_name :: Text
  , environment :: Text
  }
  deriving (Generic, Show, ToJSON)

-- | Convert all fields in an object to text. Needed because Loki only accepts string values for fields.
allFieldsText :: Object -> Object
allFieldsText = AK.map toText
 where
  toText :: Value -> Value
  toText (A.String t) = A.String t
  toText (A.Number n) = A.String $ tshow n
  toText (A.Bool b) = A.String $ tshow b
  toText _ = A.String "unknown"
