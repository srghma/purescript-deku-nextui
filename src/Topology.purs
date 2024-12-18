module Topology where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, all, any, foldl, foldr)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Debug as Debug

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty = show

instance Pretty Boolean where
  pretty = show

instance Pretty Char where
  pretty = show

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "Nothing"
  pretty (Just a) = "(Just " <> pretty a <> ")"

instance (Ord a, Pretty a) => Pretty (Set a) where
  pretty subset =
    if Set.isEmpty subset then "∅"
    else "{" <> (subset # Set.toUnfoldable # Array.sort # map pretty # Array.sort # String.joinWith ",") <> "}"

newtype ShowUsePretty a = ShowUsePretty a

derive newtype instance (Eq a) => Eq (ShowUsePretty a)
derive newtype instance (Ord a) => Ord (ShowUsePretty a)

instance Pretty a => Show (ShowUsePretty a) where
  show (ShowUsePretty x) = pretty x

class HasUniverse a where
  universe :: Set a

type TopologySpace a = Set (Set a)

-- instance (Ord a, Pretty a) => Pretty (TopologySpace a) where
--   pretty (TopologySpace subset) = pretty subset
-- instance Newtype (TopologySpace a) (Set (Set a))
-- derive newtype instance (Eq a) => Eq (TopologySpace a)
-- derive newtype instance (Ord a) => Ord (TopologySpace a)

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

tracePrettyId s x = x

-- Debug.trace (s <> ": " <> pretty x) (\_ -> x)

-- | Check if a topology is closed under unions
-- (is_open_union : ∀ (s : set (set X)), (∀ u ∈ s, is_open u) → is_open (⋃₀ s))
closedUnderUnions :: forall a. Pretty a => Ord a => Pretty a => TopologySpace a -> Boolean
closedUnderUnions topology = all (\x -> Set.member x topology) (Set.map Set.unions $ powerset topology)

-- | Check if a topology is closed under intersections
-- (is_open_inter : ∀ (s t : set X), is_open s → is_open t → is_open (s ∩ t))
-- closedUnderIntersections topology = all (\x -> Set.member x topology) (Set.map intersections $ powerset topology)
closedUnderIntersections :: forall a. Pretty a => Ord a => TopologySpace a -> Boolean
closedUnderIntersections topology =
  case subsetsOfSize2 (tracePrettyId "topology" topology) of
    Nothing -> false
    Just subsets2 -> all
      ( \(TwoElementSet x y) ->
          let
            union = Set.intersection x y
            isMember = Set.member union topology
          in
            tracePrettyId ("union " <> pretty union <> " is a member: " <> show isMember) isMember
      )
      (tracePrettyId "subsets2" subsets2)

isValidTopologySpace :: forall a. Pretty a => Ord a => HasUniverse a => TopologySpace a -> Boolean
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
    (topology)

-- Custom type for a TwoElementSet
data TwoElementSet a = TwoElementSet a a

derive instance Eq a => Eq (TwoElementSet a)
derive instance Ord a => Ord (TwoElementSet a)

instance Pretty a => Pretty (TwoElementSet a) where
  pretty (TwoElementSet x y) = "(" <> pretty x <> ", " <> pretty y <> ")"

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
