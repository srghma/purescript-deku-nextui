module FinSet3 where

import Prelude
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericPred, genericSucc)
import Topology (class HasUniverse, class Pretty)

data FinSet3 = A3 | B3 | C3

derive instance genericFinSet3 :: Generic FinSet3 _
derive instance eqFinSet3 :: Eq FinSet3
derive instance ordFinSet3 :: Ord FinSet3

instance showFinSet3 :: Show FinSet3 where
  show = genericShow

instance boundedFinSet3 :: Bounded FinSet3 where
  bottom = genericBottom
  top = genericTop

instance enumFinSet3 :: Enum FinSet3 where
  succ = genericSucc
  pred = genericPred

instance boundedEnumFinSet3 :: BoundedEnum FinSet3 where
  cardinality = Cardinality 3
  fromEnum A3 = 0
  fromEnum B3 = 1
  fromEnum C3 = 2
  toEnum 0 = Just A3
  toEnum 1 = Just B3
  toEnum 2 = Just C3
  toEnum _ = Nothing

instance hasUniverseFinSet3 :: HasUniverse FinSet3 where
  universe = Set.fromFoldable [ A3, B3, C3 ]

instance Pretty FinSet3 where
  pretty A3 = "a"
  pretty B3 = "b"
  pretty C3 = "c"
