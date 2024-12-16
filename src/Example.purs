module Example where

import Prelude

import Data.Filterable (filter)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks
import Deku.Toplevel (runInBody', runInBody)
import Effect (Effect)
import FRP.Poll (Poll)
import Unsafe.Coerce (unsafeCoerce)
import Debug
import Data.Foldable (oneOf)

buttonClass :: String -> String
buttonClass color =
  replaceAll (Pattern "COLOR") (Replacement color)
    """ml-4 inline-flex items-center rounded-md
border border-transparent bg-COLOR-600 px-3 py-2
text-sm font-medium leading-4 text-white shadow-sm
hover:bg-COLOR-700 focus:outline-none focus:ring-2
focus:ring-COLOR-500 focus:ring-offset-2"""

-- f ∷ ∀ (t9 ∷ Row Type). Boolean → APoll Event (Attribute @(Row Type) ( style ∷ String | t9 ) )
f = case _ of
  true -> DA.style $ pure "color:magenta;"
  false -> DA.unset DA.style $ pure ""

myf :: forall a x. Poll a -> (a -> Poll x) -> Poll x
myf = unsafeCoerce unit

main :: Effect Unit
main = runInBody Deku.do
  setStyleSwitch /\ styleSwitch <- useState'
  D.div_
  [ D.button (oneOf [ D.Id := "my-button", D.Class := "p-3" ])
      [ text "press me" ]
  , D.span (D.Class := "text-slate:700") [ text "heo world!" ]
  ]

-- app :: ExampleSignatures
-- app runExample = runExample Deku.do
--   setStyleSwitch /\ styleSwitch <- useState false
--   D.div_
--     [ D.a
--         [ DA.target_ "_blank"
--         -- , myf styleSwitch f
--         , DA.style $ filter identity styleSwitch $> "color:magenta;"
--         , DA.unset DA.style $ filter not styleSwitch $> ""
--         ]
--         [ text_ "Click me" ]
--     , D.button
--         [ DA.klass_ $ buttonClass "pink"
--         , DL.runOn DL.click $ styleSwitch <#> not >>> setStyleSwitch
--         ]
--         [ text_ "Switch style" ]
--     ]
