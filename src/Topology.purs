module Topology where

import Prelude
import Data.Set (Set)
import Data.Set as Set
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Foldable (all, any)
import Data.Tuple (Tuple(..))

class HasUniverse a where
  universe :: Set a

type TopologySpace a = Set (Set a)

containsEmptySet :: forall a. Ord a => TopologySpace a -> Boolean
containsEmptySet = Set.member Set.empty

containsParentSet :: forall a. Ord a => HasUniverse a => TopologySpace a -> Boolean
containsParentSet = Set.member universe

closedUnderUnions :: forall a. Ord a => HasUniverse a => TopologySpace a -> Boolean
closedUnderUnions topology =
  if Set.isEmpty topology then false
  else containsParentSet topology && allUnionsInTopology
  where
  allUnionsInTopology = all (\union -> Set.member union topology) allPossibleUnions
  allPossibleUnions = Set.map (\(Tuple s1 s2) -> Set.union s1 s2) pairs
  pairs = cartesianProduct topology topology
  cartesianProduct s1 s2 = Set.fromFoldable do
    x <- Array.fromFoldable s1
    y <- Array.fromFoldable s2
    pure $ Tuple x y

closedUnderIntersections :: forall a. Ord a => HasUniverse a => TopologySpace a -> Boolean
closedUnderIntersections topology =
  if Set.isEmpty topology then false
  else containsParentSet topology && allIntersectionsInTopology
  where
  allIntersectionsInTopology = all checkIntersection (powerset topology)
  checkIntersection subset =
    if Set.isEmpty subset then true
    else Set.member intersection topology
    where
    intersection = Array.foldl Set.intersection universe (Array.fromFoldable subset)

isValidTopologySpace :: forall a. Ord a => HasUniverse a => TopologySpace a -> Boolean
isValidTopologySpace topology =
  containsEmptySet topology
    && containsParentSet topology
    && closedUnderUnions topology
    &&
      closedUnderIntersections topology

isHausdorff :: forall a. Ord a => HasUniverse a => TopologySpace a -> Boolean
isHausdorff topology =
  if not (containsParentSet topology && containsEmptySet topology) then false
  else all checkPointPair pointPairs
  where
  pointPairs = combinations 2 (Array.fromFoldable universe)
  checkPointPair pair = case pair of
    [ x, y ] -> hasDisjointNeighborhoods x y
    _ -> false

  hasDisjointNeighborhoods x y = any
    ( \u -> any (\v -> u /= v && Set.isEmpty (Set.intersection u v))
        (openSetsContaining y)
    )
    (openSetsContaining x)

  openSetsContaining point = Set.filter
    (\s -> any (_ == point) s)
    topology

powerset :: forall a. Ord a => Set a -> Set (Set a)
powerset s = Set.fromFoldable $ map Set.fromFoldable $ combinations' (Set.size s) (Array.fromFoldable s)

combinations :: forall a. Int -> Array a -> Array (Array a)
combinations = combinations'

combinations' :: forall a. Int -> Array a -> Array (Array a)
combinations' n xs
  | n <= 0 = [ [] ]
  | Array.length xs < n = []
  | n == Array.length xs = [ xs ]
  | otherwise = case Array.uncons xs of
      Nothing -> []
      Just { head, tail } ->
        Array.concat
          [ map (Array.cons head) (combinations' (n - 1) tail)
          , combinations' n tail
          ]
