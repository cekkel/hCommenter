module HspecWaiHedgehog (waiProperty, waiLift, WaiTestable (..), WaiProperty (..)) where

import Hedgehog
import Network.Wai (Application)
import Test.Hspec.Wai.Internal

waiProperty :: (WaiTestable a) => a -> (State a, Application) -> Property
waiProperty = unWaiProperty . toProperty

data WaiProperty st = WaiProperty {unWaiProperty :: (st, Application) -> Property}

class WaiTestable a where
  type State a
  toProperty :: a -> WaiProperty (State a)

instance WaiTestable (WaiProperty st) where
  type State (WaiProperty st) = st
  toProperty = id

instance WaiTestable (WaiExpectation st) where
  type State (WaiExpectation st) = st
  toProperty action = WaiProperty (Hedgehog.property . liftIO . runWithState action)

waiLift :: (WaiTestable prop) => (a -> Property -> Property) -> a -> prop -> WaiProperty (State prop)
waiLift f a prop = WaiProperty $ \app -> f a (unWaiProperty (toProperty prop) app)
