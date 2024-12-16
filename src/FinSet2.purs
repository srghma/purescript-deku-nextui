module FinSet2 where

import Prelude
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Bounded (class Bounded)
import Topology

data FinSet2 = A2 | B2

derive instance genericFinSet2 :: Generic FinSet2 _
derive instance eqFinSet2 :: Eq FinSet2
derive instance ordFinSet2 :: Ord FinSet2

instance showFinSet2 :: Show FinSet2 where
  show = genericShow

instance boundedFinSet2 :: Bounded FinSet2 where
  bottom = A2
  top = B2

instance enumFinSet2 :: Enum FinSet2 where
  succ A2 = Just B2
  succ B2 = Nothing
  pred B2 = Just A2
  pred A2 = Nothing

instance boundedEnumFinSet2 :: BoundedEnum FinSet2 where
  cardinality = Cardinality 2
  fromEnum A2 = 0
  fromEnum B2 = 1
  toEnum 0 = Just A2
  toEnum 1 = Just B2
  toEnum _ = Nothing

instance hasUniverseFinSet2 :: HasUniverse FinSet2 where
  universe = Set.fromFoldable [ A2, B2 ]

instance ShowTopologicalSpaceElement FinSet2 where
  showTopologicalSpaceElement A2 = "a"
  showTopologicalSpaceElement B2 = "b"
