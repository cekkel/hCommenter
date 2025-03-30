{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging.Scribes.Grafana where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusIsSuccessful)
import Optics
import PyF (fmt)
import Servant (Header, JSON, NoContent (..), Post, Proxy (Proxy), ReqBody, (:>))
import Servant.Client
  ( BaseUrl (..)
  , ClientEnv
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
  { httpClientEnv :: !ClientEnv
  , authToken :: !Text
  , enableDebugLogs :: !Bool
  }

readGrafanaConf :: IO GrafanaConf
readGrafanaConf = do
  grafanaAccountNum <- getEnv "LOGGING__GRAFANA_ACC"
  grafanaToken <- getEnv "LOGGING__GRAFANA_TOKEN"
  grafanaUrl <- getEnv "LOGGING__GRAFANA_URL"
  enableDebugLogs' <- getEnv "LOGGING__ENABLE_LOG_DEBUGGING_IN_CONSOLE"
  httpManager <- newManager tlsManagerSettings

  let
    enableDebugLogs = (== "true") . toLower $ enableDebugLogs'
    authToken = [fmt|Bearer {grafanaAccountNum}:{grafanaToken}|]
    httpClientEnv =
      mkClientEnv
        httpManager
        ( BaseUrl
            { baseUrlScheme = Https
            , baseUrlHost = grafanaUrl
            , baseUrlPath = ""
            , baseUrlPort = 443
            }
        )

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
    :> ReqBody '[JSON] Loki
    :> Post '[JSON] NoContent

api :: Proxy GrafanaAPI
api = Proxy

sendLog :: Maybe Text -> Maybe Text -> Loki -> ClientM NoContent
sendLog = client api

pushLog :: forall a. (K.LogItem a) => K.Verbosity -> GrafanaConf -> K.Item a -> IO ()
pushLog verbosity conf item = do
  let
    clientEnv = conf ^. #httpClientEnv
    token = Just $ conf ^. #authToken
    contentType = Just "application/json"

  payload <- encodeLoki verbosity item

  response <- runClientM (sendLog token contentType payload) clientEnv

  when (conf ^. #enableDebugLogs) $ do
    putStrLn [fmt|Pushed log to Grafana: {encodePretty payload}|]
    putStrLn [fmt|Got response from Grafana: {tshow response}|]

  let
    non200StatusCode = response ^? _Left % _FailureResponse % _2 % to responseStatusCode

  unless (maybe True statusIsSuccessful non200StatusCode) $ do
    putStrLn [fmt|Error occurred while pushing log to Grafana: {tshow response}|]

utcTimeToEpochTime :: UTCTime -> Int
utcTimeToEpochTime = round . (* 1e9) . utcTimeToPOSIXSeconds

encodeLoki :: (K.LogItem a) => K.Verbosity -> K.Item a -> IO Loki
encodeLoki verbosity item =
  let
    msg = toStrict $ Builder.toLazyText $ K.unLogStr $ K._itemMessage item
    -- NOTE: Would use this, but katip doesn't capture unique times very well
    -- logTime = tshow $ utcTimeToEpochTime $ K._itemTime item
    environment = K.getEnvironment $ K._itemEnv item
    jsonMsg =
      allFieldsText $
        K.payloadObject verbosity (K._itemPayload item)
          & AK.insert "severity" (A.String $ K.renderSeverity $ K._itemSeverity item)
          & AK.insert "namespace" (A.String $ mconcat $ KC.intercalateNs $ K._itemNamespace item)
          & AK.insert "host" (A.String $ pack $ K._itemHost item)
  in
    do
      logTime <- tshow . utcTimeToEpochTime <$> getCurrentTime
      pure $
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
