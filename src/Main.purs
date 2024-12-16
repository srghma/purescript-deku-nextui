module Main where

import Prelude
import Prelude
import Deku.Core (Hook, Nut, fixed)
import FRP.Event (fold, mapAccum, folded, keepLatest, makeEvent, subscribe)

import Data.Array as Array
import Data.Foldable (for_, oneOf, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Deku.Control (text_)
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useDynAtEnd, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import FRP.Poll (Poll)
import Partial.Unsafe (unsafePartial)
import Web.HTML (window)
import Web.HTML.Window (confirm)

main :: Effect Unit
main = pure unit
