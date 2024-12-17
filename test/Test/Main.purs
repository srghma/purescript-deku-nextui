module Test.Main where

import FinSet2
import FinSet3
import Prelude
import Topology

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Data.Either (Either(..))
import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (class Foldable, elem, for_, notElem)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error, error)
import Test.Spec (Spec, describe, describeOnly, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

itTopologySpaceSatisfies :: forall a. Pretty a => Ord a => (TopologySpace a -> Boolean) -> TopologySpace a -> Boolean -> Spec Unit
itTopologySpaceSatisfies property topology expected = do
  let topologyString = pretty topology
  it topologyString do
    let actual = property topology
    unless (actual == expected)
      $ fail
      $ "actual = " <> show actual <> ", expected = " <> show expected

itClosedUnderUnions :: forall a. Show a => Pretty a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itClosedUnderUnions = itTopologySpaceSatisfies closedUnderUnions

itClosedUnderIntersections :: forall a. Pretty a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itClosedUnderIntersections = itTopologySpaceSatisfies closedUnderIntersections

itContainsEmptySet :: forall a. Pretty a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itContainsEmptySet = itTopologySpaceSatisfies containsEmptySet

itContainsParentSet :: forall a. Pretty a => HasUniverse a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itContainsParentSet = itTopologySpaceSatisfies containsParentSet

itIsHausdorff :: forall a. Pretty a => HasUniverse a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itIsHausdorff = itTopologySpaceSatisfies isHausdorff

itIsValidTopologySpace :: forall a. Show a => Pretty a => HasUniverse a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itIsValidTopologySpace = itTopologySpaceSatisfies isValidTopologySpace

spec :: Spec Unit
spec = do
  describe "Topology" do
    describe "containsEmptySet" do
      itContainsEmptySet (TopologySpace $ Set.singleton Set.empty :: TopologySpace FinSet2) true
      itContainsEmptySet (TopologySpace $ Set.singleton (Set.singleton A2)) false
      itContainsEmptySet (TopologySpace $ Set.fromFoldable [ Set.empty, Set.empty ] :: TopologySpace FinSet2) true

    describe "containsParentSet" do
      itContainsParentSet (TopologySpace $ Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2) true
      itContainsParentSet (TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2 ]) false
      itContainsParentSet (TopologySpace $ Set.fromFoldable [ Set.singleton A2, Set.singleton B2 ]) false

    describe "closedUnderUnions" do
      itClosedUnderUnions (TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]) true
      itClosedUnderUnions (TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2 ]) false
      itClosedUnderUnions (TopologySpace $ Set.empty :: TopologySpace FinSet2) false
      itClosedUnderUnions (TopologySpace $ Set.singleton universe :: TopologySpace FinSet2) true
      itClosedUnderUnions (TopologySpace $ Set.fromFoldable [ Set.empty, Set.fromFoldable [ A2 ], Set.fromFoldable [ A2, B2 ] ]) true
      itClosedUnderUnions (TopologySpace $ Set.fromFoldable [ Set.empty, (universe :: Set FinSet2) ]) true

    describe "closedUnderIntersections" do
      itClosedUnderIntersections (TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]) true
      itClosedUnderIntersections (TopologySpace $ Set.fromFoldable [ Set.singleton A2, Set.singleton B2 ]) false
      itClosedUnderIntersections (TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, universe ]) true

    describe "isValidTopologySpace" do
      let discreteTopology = TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
      let indiscreteTopology = TopologySpace $ Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
      let invalidTopology = TopologySpace $ Set.fromFoldable [ Set.singleton A2 ]
      let emptyTopology = TopologySpace $ Set.empty :: TopologySpace FinSet2
      let missingEmptyTopology = TopologySpace $ Set.fromFoldable [ Set.singleton A2, universe ]

      itIsValidTopologySpace discreteTopology true
      itIsValidTopologySpace indiscreteTopology true
      itIsValidTopologySpace invalidTopology false
      itIsValidTopologySpace emptyTopology false
      itIsValidTopologySpace missingEmptyTopology false

    describe "isHausdorff" do
      let discreteTopology = TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
      let indiscreteTopology = TopologySpace $ Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
      let
        hausdorffTopology =
          TopologySpace $ Set.fromFoldable
            [ Set.empty
            , Set.singleton A3
            , Set.singleton B3
            , Set.singleton C3
            , Set.fromFoldable [ A3, B3 ]
            , Set.fromFoldable [ B3, C3 ]
            , Set.fromFoldable [ A3, C3 ]
            , universe
            ] :: TopologySpace FinSet3
      let insufficientSepTopology = TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, universe ] :: TopologySpace FinSet3

      itIsHausdorff discreteTopology true
      itIsHausdorff indiscreteTopology false
      itIsHausdorff hausdorffTopology true
      itIsHausdorff insufficientSepTopology false

    describe "powerset" do
      it "should return the correct power set of an empty set" do
        let set = Set.empty :: Set Int
        let expected = Set.singleton Set.empty
        powerset set `shouldEqual` expected
      it "should return the correct power set of a set with one element" do
        let set = Set.singleton 1
        let expected = Set.fromFoldable [ Set.empty, Set.singleton 1 ]
        powerset set `shouldEqual` expected
      it "should return the correct power set of a set with two elements" do
        let set = Set.fromFoldable [ 1, 2 ]
        let expected = Set.fromFoldable [ Set.empty, Set.singleton 1, Set.singleton 2, Set.fromFoldable [ 1, 2 ] ]
        powerset set `shouldEqual` expected
      it "should return the correct power set of a set with three elements" do
        let set = Set.fromFoldable [ 1, 2, 3 ]
        let
          expected = Set.fromFoldable
            [ Set.empty
            , Set.singleton 1
            , Set.singleton 2
            , Set.singleton 3
            , Set.fromFoldable [ 1, 2 ]
            , Set.fromFoldable [ 1, 3 ]
            , Set.fromFoldable [ 2, 3 ]
            , Set.fromFoldable [ 1, 2, 3 ]
            ]
        powerset set `shouldEqual` expected
      it "should return the correct power set of a set with characters" do
        let set = Set.fromFoldable [ 'a', 'b' ]
        let expected = Set.fromFoldable [ Set.empty, Set.singleton 'a', Set.singleton 'b', Set.fromFoldable [ 'a', 'b' ] ]
        powerset set `shouldEqual` expected

  it "Powerset of universe" do
    let
      expectedPowerset = Set.fromFoldable
        [ Set.empty
        , Set.singleton A2
        , Set.singleton B2
        , universe
        ]
    powerset universe `shouldEqual` expectedPowerset

  it "Powerset of powerset of universe" do
    let powersetUniverse = powerset universe
    let powersetUniverse2 = powerset powersetUniverse
    let
      expectedPowersetOfPowerset = Set.fromFoldable
        [ Set.empty
        , Set.singleton Set.empty
        , Set.singleton (Set.singleton A2)
        , Set.singleton (Set.singleton B2)
        , Set.singleton universe
        , Set.fromFoldable [ Set.empty, Set.singleton A2 ]
        , Set.fromFoldable [ Set.empty, Set.singleton B2 ]
        , Set.fromFoldable [ Set.empty, universe ]
        , Set.fromFoldable [ Set.singleton A2, Set.singleton B2 ]
        , Set.fromFoldable [ Set.singleton A2, universe ]
        , Set.fromFoldable [ Set.singleton B2, universe ]
        , Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2 ]
        , Set.fromFoldable [ Set.empty, Set.singleton A2, universe ]
        , Set.fromFoldable [ Set.empty, Set.singleton B2, universe ]
        , Set.fromFoldable [ Set.singleton A2, Set.singleton B2, universe ]
        , Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
        ]
    powersetUniverse2 `shouldEqual` expectedPowersetOfPowerset
    let
      validTopologies = Set.fromFoldable
        [ TopologySpace $ Set.fromFoldable [ Set.empty, universe ]
        , TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, universe ]
        , TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton B2, universe ]
        , TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
        ]
    (Set.filter isValidTopologySpace $ Set.map TopologySpace powersetUniverse2) `shouldEqual` validTopologies
    let
      validHausdorff = Set.fromFoldable
        [ TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
        ]
    Set.filter isHausdorff (Set.filter isValidTopologySpace $ Set.map TopologySpace powersetUniverse2) `shouldEqual` validHausdorff
    Set.filter isHausdorff (Set.map TopologySpace powersetUniverse2) `shouldEqual` validHausdorff

  describe "topology properties" do
    let
      topologiesList :: Array { set :: TopologySpace FinSet3, closedUnderUnions :: Boolean, closedUnderIntersections :: Boolean }
      topologiesList =
        [ -- {∅, {a}, {a,b}, {a,b,c}}
          { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe ], closedUnderUnions: true, closedUnderIntersections: true }
        -- {∅, {a}, {a,b}, {a,b,c}, {b}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3 ], closedUnderUnions: false, closedUnderIntersections: false }
        --{∅, {a}, {a,b}, {a,b,c}, {b}, {b,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3, Set.fromFoldable [ B3, C3 ] ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a}, {a,b}, {a,b,c}, {a,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.fromFoldable [ A3, C3 ] ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a}, {a,b}, {a,b,c}, {a,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.fromFoldable [ A3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a}, {a,b}, {a,b,c}, {a,c}, {b}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.fromFoldable [ A3, C3 ], Set.singleton B3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a}, {a,b}, {a,b,c}, {a,c}, {b}, {b,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.fromFoldable [ A3, C3 ], Set.singleton B3, Set.fromFoldable [ B3, C3 ], Set.singleton C3 ], closedUnderUnions: true, closedUnderIntersections: true }
        -- {∅, {a}, {a,b,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, universe ], closedUnderUnions: true, closedUnderIntersections: true }
        -- {∅, {a}, {a,b,c}, {b,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, universe, Set.fromFoldable [ B3, C3 ] ], closedUnderUnions: false, closedUnderIntersections: true }
        -- {∅, {a}, {a,b,c}, {a,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, universe, Set.fromFoldable [ A3, C3 ] ], closedUnderUnions: false, closedUnderIntersections: true }
        -- {∅, {a}, {a,b,c}, {a,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, universe, Set.fromFoldable [ A3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a}, {a,b,c}, {a,c}, {b,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.singleton A3, universe, Set.fromFoldable [ A3, C3 ], Set.fromFoldable [ B3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b}, {a,b,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe ], closedUnderUnions: true, closedUnderIntersections: true }
        -- {∅, {a,b}, {a,b,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe, Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b}, {a,b,c}, {b}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b}, {a,b,c}, {b}, {b,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3, Set.fromFoldable [ B3, C3 ] ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b}, {a,b,c}, {b}, {b,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3, Set.fromFoldable [ B3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe ], closedUnderUnions: true, closedUnderIntersections: true }
        -- {∅, {a,b,c}, {b}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.singleton B3 ], closedUnderUnions: false, closedUnderIntersections: true }
        -- {∅, {a,b,c}, {b}, {b,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.singleton B3, Set.fromFoldable [ B3, C3 ] ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b,c}, {b}, {b,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.singleton B3, Set.fromFoldable [ B3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b,c}, {b,c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.fromFoldable [ B3, C3 ] ], closedUnderUnions: true, closedUnderIntersections: true }
        -- {∅, {a,b,c}, {b,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.fromFoldable [ B3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b,c}, {a,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.fromFoldable [ A3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b,c}, {a,c}, {b}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.fromFoldable [ A3, C3 ], Set.singleton B3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b,c}, {a,c}, {b}, {b,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.fromFoldable [ A3, C3 ], Set.singleton B3, Set.fromFoldable [ B3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        -- {∅, {a,b,c}, {a,c}, {b,c}, {c}}
        , { set: TopologySpace $ Set.fromFoldable [ Set.empty, universe, Set.fromFoldable [ A3, C3 ], Set.fromFoldable [ B3, C3 ], Set.singleton C3 ], closedUnderUnions: false, closedUnderIntersections: false }
        ]
    describe "closedUnderUnions" do
      for_ topologiesList \testCase -> itClosedUnderUnions testCase.set testCase.closedUnderUnions

    describe "closedUnderIntersections" do
      pure unit

-- for_ topologiesList \testCase -> itClosedUnderIntersections testCase.set testCase.closedUnderUnions

-- it "Powerset of powerset of universe (3 elements)" do
--   let powersetUniverse = powerset universe
--   let powersetUniverse2 = powerset powersetUniverse
--   let
--     validTopologies = TopologySpace $ Set.fromFoldable
--       [ Set.fromFoldable [ Set.empty, universe ]
--       , Set.fromFoldable [ Set.empty, Set.singleton A3, universe ]
--       , Set.fromFoldable [ Set.empty, Set.singleton B3, universe ]
--       , Set.fromFoldable [ Set.empty, Set.singleton C3, universe ]
--       , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton B3, universe ]
--       , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton C3, universe ]
--       , Set.fromFoldable [ Set.empty, Set.singleton B3, Set.singleton C3, universe ]
--       , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton B3, Set.singleton C3, universe ]
--       ]
--   Set.filter isValidTopologySpace powersetUniverse2 `shouldEqual` validTopologies

-- let
--   validHausdorff = TopologySpace $ Set.fromFoldable
--     [ Set.fromFoldable [Set.empty, Set.singleton A3, Set.singleton B3, Set.singleton C3, universe]
--     ]
-- Set.filter isHausdorff (Set.filter isValidTopologySpace powersetUniverse2) `shouldEqual` validHausdorff
-- Set.filter isHausdorff powersetUniverse2 `shouldEqual` validHausdorff

shouldEqual
  :: forall m a
   . MonadThrow Error m
  => Pretty a
  => Ord a
  => Eq a
  => a
  -> a
  -> m Unit
shouldEqual a b = (ShowUsePretty a) `Spec.shouldEqual` (ShowUsePretty b)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec
