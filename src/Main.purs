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
import Deku.Toplevel (runInBody')
import Deku.Toplevel (runInBody', runInBody)
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
main = runInBody deeplyNestedPreservesOrder
  where
  -- `Nut` is the type of Deku components
  mySimpleComponent :: Nut
  mySimpleComponent =
    (D.input [ DA.xtypeRange, DL.numberOn_ DL.input logShow ] [])

  deeplyNestedPreservesOrder :: Nut
  deeplyNestedPreservesOrder = Deku.do
    let
      counter :: forall a. Poll a -> Poll Int
      counter event = fold (\a _ -> a + 1) (-1) event
    setItem /\ item <- useState'
    let
      mydyn n = do
        let sn = show n
        fixed
          [ Deku.do
              { value: i } <- useDynAtEnd (counter item)
              if i == 1 then mydyn (n + 1)
              else D.span [ DA.id_ ("dyn" <> sn <> "-" <> show i) ]
                [ text_ (sn <> "-" <> show i) ]
          , D.button [ DA.id_ $ "incr-" <> sn, DL.click_ \_ -> setItem unit ]
              [ text_ $ "incr-" <> sn ]
          ]
    D.div [ DA.id_ "div0" ]
      [ mydyn 0
      ]

-- | type Rec = { code :: Int, desc :: String }
-- | type Recs = Array Rec

-- | main :: Effect Unit
-- | main = do
-- |   { push, event } <- create
-- |   let
-- |     init =
-- |       [ { code: 1, desc: "Desc for 1" }
-- |       , { code: 2, desc: "Desc for 2" }
-- |       , { code: -1, desc: "Desc for -1" }
-- |       ]
-- |   { event: recs } <- burning init event
-- |   runInBody (testFunc recs push)

-- | data ItemLifecycle = Old Rec | New Rec

-- | testFunc :: forall lock payload. Event Recs -> (Recs -> Effect Unit) -> Domable lock payload
-- | testFunc recs pushRecs = Deku.do
-- |   setUpdating /\ updating <- useHot Nothing
-- |   setItems /\ items <- useHot'

-- |   onWillMount
-- |     ( launchAff_ do
-- |         delay $ Milliseconds 0.0
-- |         liftEffect do
-- |           uns <- subscribe recs setItems
-- |           uns
-- |     ) $ Deku.do
-- |     let
-- |       disabled = (_ <#> if _ then D.Disabled := "true" else D.Disabled := unit)
-- |       disabled_ = disabled <<< pure
-- |       remove idx is = setItems $ unsafePartial fromJust $ Array.deleteAt idx is
-- |       insert item is = setItems $ item Array.: is
-- |       update idx item is = setItems $ unsafePartial fromJust $ Array.updateAt idx item is

-- |     D.table_
-- |       [ D.thead_
-- |           [ D.tr_
-- |               [ D.th_
-- |                   [ D.button
-- |                       ( oneOf
-- |                           [ disabled $ updating <#> isJust
-- |                           , click $ items <#> \is -> cb \_ -> do
-- |                               insert { code: 0, desc: "" } is
-- |                               setUpdating $ Just 0
-- |                           ]
-- |                       )
-- |                       [ text_ "Add" ]
-- |                   ]
-- |               , D.th__ "Code"
-- |               , D.th__ "Description"
-- |               ]
-- |           ]
-- |       , D.tbody_
-- |           [ Deku.do
-- |               items <#~> \is' -> fixed $ flip Array.mapWithIndex is' \idx item -> Deku.do
-- |                 let new = item.code == 0

-- |                 setTemp /\ temp <- useHot item

-- |                 updating <#~> \mbUpdIdx -> Deku.do
-- |                   let
-- |                     thisUpdating = (Just idx == mbUpdIdx)
-- |                     otherUpdating = isJust mbUpdIdx && not thisUpdating
-- |                     unchanged = temp <#> (_ == item)

-- |                   D.tr_
-- |                     [ D.td_
-- |                         if thisUpdating then
-- |                           [ D.button
-- |                               ( oneOf
-- |                                   [ disabled unchanged
-- |                                   , click $ (\t its -> update idx t its *> setUpdating Nothing) <$> temp <*> items
-- |                                   ]
-- |                               )
-- |                               [ text_ "Update" ]
-- |                           , D.button
-- |                               ( click $ items <#> \is -> cb \_ -> do
-- |                                   if new then remove idx is else pure unit
-- |                                   setUpdating Nothing
-- |                               )
-- |                               [ text_ "Cancel" ]
-- |                           ]
-- |                         else if item.code /= -1 then
-- |                           [ D.button
-- |                               ( oneOf
-- |                                   [ disabled_ otherUpdating
-- |                                   , click_ $ cb \_ -> setUpdating $ Just idx
-- |                                   ]
-- |                               )
-- |                               [ text_ "Edit" ]
-- |                           , D.button
-- |                               ( oneOf
-- |                                   [ disabled_ otherUpdating
-- |                                   , click $ items <#> \is -> cb \_ -> window
-- |                                       >>= confirm ("Do you want to remove item with code \"" <> show item.code <> "\"")
-- |                                       >>= if _ then remove idx is else pure unit
-- |                                   ]
-- |                               )
-- |                               [ text_ "Remove" ]
-- |                           ]
-- |                         else [ blank ]
-- |                     , if thisUpdating then
-- |                         D.input
-- |                           ( oneOf
-- |                               [ D.Value !:= show item.code
-- |                               , numeric $ temp <#> \t val -> setTemp $ t { code = unsafePartial $ fromJust $ Int.fromNumber val }
-- |                               ]
-- |                           )
-- |                           []
-- |                       else D.td__ $ show item.code
-- |                     , D.td__ item.desc
-- |                     ]
-- |           ]
-- |       ]
