module Topology where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, all, any, foldl, foldr)
import Data.List (List(..), mapMaybe, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Debug as Debug

class HasUniverse a where
  universe :: Set a

data TopologySpace a = TopologySpace (Set (Set a))

class ShowTopologySpaceElement a where
  showTopologicalSpaceElement :: a -> String

instance Newtype (ShowTopologicalSpace a) (TopologySpace a)
derive newtype instance (Eq a) => Eq (ShowTopologicalSpace a)
derive newtype instance (Ord a) => Ord (ShowTopologicalSpace a)
-- Show instance for ShowTopologicalSpace
instance (Ord a, ShowTopologySpaceElement a) => Show (ShowTopologicalSpace a) where
  show (ShowTopologicalSpace topology) =
    let
      showSet :: Set a -> String
      showSet subset =
        if Set.isEmpty subset then "∅"
        else "{" <> (subset # Set.toUnfoldable # Array.sort # map showTopologicalSpaceElement # String.joinWith ",") <> "}"

      showTopology :: TopologySpace a -> String
      showTopology space =
        "{" <> (space # Set.toUnfoldable # Array.sort # map showSet # String.joinWith ", ") <> "}"
    in
      showTopology topology

containsEmptySet :: forall a. Ord a => TopologySpace a -> Boolean
containsEmptySet = Set.member Set.empty

containsParentSet :: forall a. Ord a => HasUniverse a => TopologySpace a -> Boolean
containsParentSet = Set.member universe

-- | Form the union of a collection of sets
intersections :: forall f a. Foldable f => Ord a => f (Set a) -> Set a
intersections = foldl Set.intersection Set.empty

-- structure topological_space (X : Type) :=
-- (is_open : set X → Prop)
-- (is_open_univ : is_open set.univ)
-- (is_open_empty : is_open ∅)
-- (is_open_inter : ∀ (s t : set X), is_open s → is_open t → is_open (s ∩ t))

-- | Check if a topology is closed under unions
-- (is_open_union : ∀ (s : set (set X)), (∀ u ∈ s, is_open u) → is_open (⋃₀ s))
closedUnderUnions :: forall a. Show a => Ord a => TopologySpace a -> Boolean
closedUnderUnions topology =
  case subsetsOfSize2 topology of
    Nothing -> false
    Just subsets2 -> all (\(TwoElementSet x y) -> Set.member (Set.union x y) topology) (Debug.trace (show subsets2) (\_ -> subsets2))

-- | Check if a topology is closed under intersections
closedUnderIntersections :: forall a. Ord a => Set (Set a) -> Boolean
closedUnderIntersections topology = Set.member (Set.unions topology) topology

isValidTopologySpace :: forall a. Show a => Ord a => HasUniverse a => TopologySpace a -> Boolean
isValidTopologySpace topology =
  containsEmptySet topology
    && containsParentSet topology
    && closedUnderUnions topology
    && closedUnderIntersections topology

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

-- Custom type for a TwoElementSet
data TwoElementSet a = TwoElementSet a a

derive instance Eq a => Eq (TwoElementSet a)
derive instance Ord a => Ord (TwoElementSet a)
instance Show a => Show (TwoElementSet a) where
  show (TwoElementSet x y) = "(" <> show x <> ", " <> show y <> ")"

-- Function to create a TwoElementSet, ensuring distinct elements
makeTwoElementSet :: forall a. Ord a => a -> a -> Maybe (TwoElementSet a)
makeTwoElementSet x y =
  case compare x y of
    EQ -> Nothing
    LT -> Just (TwoElementSet x y)
    GT -> Just (TwoElementSet y x)

-- Function to compute all subsets of size 2, returning Maybe
subsetsOfSize2 :: forall a. Ord a => Set a -> Maybe (Set (TwoElementSet a))
subsetsOfSize2 xs =
  let
    arr = Set.toUnfoldable xs :: List a
  in
    if List.length arr < 2 then Nothing
    else Just (go arr mempty)
  where
  go :: List a -> Set (TwoElementSet a) -> Set (TwoElementSet a)
  go List.Nil acc = acc
  go (x : rest) acc =
    let
      pairs = List.mapMaybe (\y -> makeTwoElementSet x y) rest
    in
      go rest (foldr (\pair s -> Set.insert pair s) acc pairs)

powersetOrSize :: forall a. Ord a => Int -> Set a -> Set (Set a)
powersetOrSize size set = Set.filter (\s -> Set.size s == size) (powerset set)

powerset :: forall a. Ord a => Set a -> Set (Set a)
powerset set =
  if Set.isEmpty set then
    Set.singleton Set.empty
  else
    case Set.findMin set of
      Nothing -> Set.singleton Set.empty -- Fallback for safety (should not occur)
      Just elem ->
        let
          rest = Set.delete elem set
          subsets = powerset rest
        in
          Set.union subsets (Set.map (Set.insert elem) subsets)

combinations :: forall a. Int -> Array a -> Array (Array a)
combinations n xs
  | n <= 0 = [ [] ]
  | Array.length xs < n = []
  | n == Array.length xs = [ xs ]
  | otherwise = case Array.uncons xs of
      Nothing -> []
      Just { head, tail } ->
        Array.concat
          [ map (Array.cons head) (combinations (n - 1) tail)
          , combinations n tail
          ]
