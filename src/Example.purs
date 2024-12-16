module Example where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Ref (new, read, write)
import Data.Array (drop, take, dropEnd, zipWith)
import Data.Array as Array
import Data.Compactable (compact)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Number (floor, sign)
import Data.Number.Format (fixed, toStringWith)
import Data.Op (Op(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut, useRant)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.SVG as DS
import Deku.DOM.SVG.Attributes as DSA
import Deku.Do as Deku
import Deku.Hooks (guardWith, useDynAtEnd, useHot, useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Random (randomBool, randomRange)
import Effect.Timer (clearTimeout)
import FRP.Event (Event, fold, mapAccum, sampleOnRight)
import FRP.Event.AnimationFrame (animationFrame')
import FRP.Event.Time (interval', withDelay, withTime)
import FRP.Poll (sham)
import Record (union)

buttonClass :: String -> String
buttonClass color =
  """ml-4 inline-flex items-center rounded-md
border border-transparent bg-COLOR-600 px-3 py-2
text-sm font-medium leading-4 text-white shadow-sm
hover:bg-COLOR-700 focus:outline-none focus:ring-2
focus:ring-COLOR-500 focus:ring-offset-2"""

mySimpleComponent :: Nut
mySimpleComponent =
  D.div_
    [ D.span__ "I exist"
    , D.ul_ $ map D.li__ [ "A", "B", "C" ]
    , D.div_
        [ D.h3__ "foo"
        , D.i__ "bar"
        , text_ " "
        , D.b__ "baz"
        ]
    ]

main :: Effect Unit
main = void $ runInBody mySimpleComponent
