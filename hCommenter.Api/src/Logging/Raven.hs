module Logging.Raven where

import Data.Aeson.KeyMap (fromHashMapText, toHashMapText)
import Data.String.Conv (toS)

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as Builder
import Katip qualified
import Katip.Core qualified
import System.Log.Raven qualified as Raven
import System.Log.Raven.Types qualified as Raven

mkRavenScribe :: Raven.SentryService -> Katip.PermitFunc -> Katip.Verbosity -> IO Katip.Scribe
mkRavenScribe sentryService permitItem verbosity =
  return $
    Katip.Scribe
      { Katip.liPush = push
      , Katip.scribeFinalizer = return ()
      , Katip.scribePermitItem = permitItem
      }
 where
  push :: (Katip.LogItem a) => Katip.Item a -> IO ()
  push item = Raven.register sentryService (toS name) level msg updateRecord
   where
    name = sentryName $ Katip._itemNamespace item
    level = sentryLevel $ Katip._itemSeverity item
    msg = TL.unpack $ Builder.toLazyText $ Katip.Core.unLogStr $ Katip._itemMessage item
    katipAttrs =
      foldMap (\loc -> HM.singleton "loc" $ Aeson.toJSON $ Katip.Core.LocJs loc) (Katip._itemLoc item)
    context = HM.mapKeys T.unpack $ toHashMapText $ Katip.payloadObject verbosity (Katip._itemPayload item) <> fromHashMapText katipAttrs
    updateRecord record =
      record
        { Raven.srEnvironment = Just $ toS $ Katip.getEnvironment $ Katip._itemEnv item
        , Raven.srTimestamp = Katip._itemTime item
        , -- add string / bool katip context values as tags
          Raven.srTags = T.unpack <$> HM.mapMaybe onlyText context
        , -- add remaining katip context as raven extras
          Raven.srExtra = context
        }

  onlyText :: Aeson.Value -> Maybe Text
  onlyText = \case
    (Aeson.String str) -> Just str
    (Aeson.Bool b) -> Just (tshow b)
    _ -> Nothing

  -- onlyTextHashmap ::

  sentryLevel :: Katip.Severity -> Raven.SentryLevel
  sentryLevel Katip.DebugS = Raven.Debug
  sentryLevel Katip.InfoS = Raven.Info
  sentryLevel Katip.NoticeS = Raven.Custom "Notice"
  sentryLevel Katip.WarningS = Raven.Warning
  sentryLevel Katip.ErrorS = Raven.Error
  sentryLevel Katip.CriticalS = Raven.Fatal
  sentryLevel Katip.AlertS = Raven.Fatal
  sentryLevel Katip.EmergencyS = Raven.Fatal

  sentryName :: Katip.Namespace -> T.Text
  sentryName (Katip.Namespace xs) = T.intercalate "." xs
