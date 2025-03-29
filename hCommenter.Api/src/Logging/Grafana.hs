{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging.Grafana where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusIsSuccessful)
import Optics
import PyF (fmt)
import Servant (Header, JSON, NoContent (..), Post, Proxy (Proxy), ReqBody, (:>))
import Servant.Client
  ( BaseUrl (..)
  , ClientError
  , ClientM
  , ResponseF (responseStatusCode)
  , Scheme (Https)
  , client
  , mkClientEnv
  , runClientM
  )
import System.Environment (getEnv)

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as AK
import Data.Text.Lazy.Builder qualified as Builder
import Katip qualified as K
import Katip.Core qualified as KC

data GrafanaConf = GrafanaConf
  { grafanaAccountNum :: !Text
  , grafanaToken :: !Text
  , grafanaUrl :: !Text
  , httpManager :: !Manager
  , debugLogs :: !Bool
  }

readGrafanaConf :: IO GrafanaConf
readGrafanaConf = do
  grafanaAccountNum <- pack <$> getEnv "LOGGING__GRAFANA_ACC"
  grafanaToken <- pack <$> getEnv "LOGGING__GRAFANA_TOKEN"
  grafanaUrl <- pack <$> getEnv "LOGGING__GRAFANA_URL"
  debugLogs <- (== "true") . toLower <$> getEnv "LOGGING__ENABLE_LOG_DEBUGGING_IN_CONSOLE"

  httpManager <- newManager tlsManagerSettings

  pure $ GrafanaConf {..}

makeFieldLabelsNoPrefix ''GrafanaConf

makePrisms ''ClientError

mkGrafanaScribe :: GrafanaConf -> K.PermitFunc -> K.Verbosity -> IO K.Scribe
mkGrafanaScribe conf permitF verbosity = do
  pure $ K.Scribe {scribePermitItem = permitF, scribeFinalizer = pure (), liPush = pushLog verbosity conf}

type GrafanaAPI =
  "loki"
    :> "api"
    :> "v1"
    :> "push"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] NoContent

api :: Proxy GrafanaAPI
api = Proxy

sendLog :: Maybe Text -> Maybe Text -> Value -> ClientM NoContent
sendLog = client api

pushLog :: forall a. (K.LogItem a) => K.Verbosity -> GrafanaConf -> K.Item a -> IO ()
pushLog verbosity conf item = do
  let
    clientEnv =
      mkClientEnv
        (conf ^. #httpManager)
        ( BaseUrl
            { baseUrlScheme = Https
            , baseUrlHost = unpack $ conf ^. #grafanaUrl
            , baseUrlPath = ""
            , baseUrlPort = 443
            }
        )

  let
    token = Just [fmt|Bearer {conf ^. #grafanaAccountNum}:{conf ^. #grafanaToken}|]
    contentType = Just "application/json"
    payload = encodeLoki verbosity item

  response <- runClientM (sendLog token contentType payload) clientEnv

  when (conf ^. #debugLogs) $ do
    putStrLn [fmt|Pushed log to Grafana: {encodePretty payload}|]
    putStrLn [fmt|Got response from Grafana: {tshow response}|]

  let
    non200StatusCode = response ^? _Left % _FailureResponse % _2 % to responseStatusCode

  unless (maybe True statusIsSuccessful non200StatusCode) $ do
    putStrLn [fmt|Error occurred while pushing log to Grafana: {tshow response}|]

utcTimeToEpochTime :: UTCTime -> Int
utcTimeToEpochTime = round . (* 1e9) . utcTimeToPOSIXSeconds

encodeLoki :: (K.LogItem a) => K.Verbosity -> K.Item a -> Value
encodeLoki verbosity item =
  let
    msg = toStrict $ Builder.toLazyText $ K.unLogStr $ K._itemMessage item
    -- FIXME: This is currently the same time for all logs in a request. Need to verify if correct.
    logTime = tshow $ utcTimeToEpochTime $ K._itemTime item
    environment = K.getEnvironment $ K._itemEnv item
    jsonMsg =
      allFieldsText $
        K.payloadObject verbosity (K._itemPayload item)
          & AK.insert "severity" (A.String $ K.renderSeverity $ K._itemSeverity item)
          & AK.insert "namespace" (A.String $ mconcat $ KC.intercalateNs $ K._itemNamespace item)
          & AK.insert "host" (A.String $ pack $ K._itemHost item)
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

newtype Loki = Loki
  { streams :: [Stream]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

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
