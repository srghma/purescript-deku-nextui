module Test.Main where

import Prelude
import Topology

import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import FinSet2 (FinSet2(..))
import FinSet3 (FinSet3(..))
import Test.Spec (class FocusWarning, Spec, describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

itTopologySpaceSatisfies :: forall a. Pretty a => Ord a => (TopologySpace a -> Boolean) -> TopologySpace a -> Boolean -> Spec Unit
itTopologySpaceSatisfies property topology expected = do
  let topologyString = pretty topology
  it (topologyString <> if expected then " should be ✅" else " should be ❌") do
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

----------------------------------------------

itTopologySpaceSatisfiesOnly :: forall a. FocusWarning => Pretty a => Ord a => (TopologySpace a -> Boolean) -> TopologySpace a -> Boolean -> Spec Unit
itTopologySpaceSatisfiesOnly property topology expected = do
  let topologyString = pretty topology
  itOnly (topologyString <> if expected then " should be ✅" else " should be ❌") do
    let actual = property topology
    unless (actual == expected)
      $ fail
      $ "actual = " <> show actual <> ", expected = " <> show expected

itClosedUnderUnionsOnly :: forall a. FocusWarning => Show a => Pretty a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itClosedUnderUnionsOnly = itTopologySpaceSatisfiesOnly closedUnderUnions

itClosedUnderIntersectionsOnly :: forall a. FocusWarning => Pretty a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itClosedUnderIntersectionsOnly = itTopologySpaceSatisfiesOnly closedUnderIntersections

itContainsEmptySetOnly :: forall a. FocusWarning => Pretty a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itContainsEmptySetOnly = itTopologySpaceSatisfiesOnly containsEmptySet

itContainsParentSetOnly :: forall a. FocusWarning => Pretty a => HasUniverse a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itContainsParentSetOnly = itTopologySpaceSatisfiesOnly containsParentSet

itIsHausdorffOnly :: forall a. FocusWarning => Pretty a => HasUniverse a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itIsHausdorffOnly = itTopologySpaceSatisfiesOnly isHausdorff

itIsValidTopologySpaceOnly :: forall a. FocusWarning => Show a => Pretty a => HasUniverse a => Ord a => TopologySpace a -> Boolean -> Spec Unit
itIsValidTopologySpaceOnly = itTopologySpaceSatisfiesOnly isValidTopologySpace

----------------------------------------------

itSubsetsOfSize2 :: forall a. Pretty a => Ord a => Set a -> Maybe (Set (TwoElementSet a)) -> Spec Unit
itSubsetsOfSize2 input expected = do
  let inputString = pretty input
  it inputString do
    let actual = subsetsOfSize2 input
    unless (actual == expected)
      $ fail
      $ "actual = " <> pretty actual <> ", expected = " <> pretty expected

spec :: Spec Unit
spec = do
  describe "TwoElementSet and subsetsOfSize2" do
    itSubsetsOfSize2
      (Set.fromFoldable [ 1, 2, 3, 4 ])
      ( Just $ Set.fromFoldable
          [ TwoElementSet 1 2
          , TwoElementSet 1 3
          , TwoElementSet 1 4
          , TwoElementSet 2 3
          , TwoElementSet 2 4
          , TwoElementSet 3 4
          ]
      )
    itSubsetsOfSize2 (Set.fromFoldable [ 1 ]) Nothing
    itSubsetsOfSize2 (Set.empty :: Set Int) Nothing

  describe "Topology" do
    describe "containsEmptySet" do
      itContainsEmptySet (Set.singleton Set.empty :: TopologySpace FinSet2) true
      itContainsEmptySet (Set.singleton (Set.singleton A2)) false
      itContainsEmptySet (Set.fromFoldable [ Set.empty, Set.empty ] :: TopologySpace FinSet2) true

    describe "containsParentSet" do
      itContainsParentSet (Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2) true
      itContainsParentSet (Set.fromFoldable [ Set.empty, Set.singleton A2 ]) false
      itContainsParentSet (Set.fromFoldable [ Set.singleton A2, Set.singleton B2 ]) false

    describe "closedUnderUnions" do
      itClosedUnderUnions (Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]) true
      itClosedUnderUnions (Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2 ]) false
      itClosedUnderUnions (Set.empty :: TopologySpace FinSet2) false
      itClosedUnderUnions (Set.singleton universe :: TopologySpace FinSet2) false
      itClosedUnderUnions (Set.fromFoldable [ Set.empty, Set.fromFoldable [ A2 ], Set.fromFoldable [ A2, B2 ] ]) true
      itClosedUnderUnions (Set.fromFoldable [ Set.empty, (universe :: Set FinSet2) ]) true

    describe "closedUnderIntersections" do
      itClosedUnderIntersections (Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]) true
      itClosedUnderIntersections (Set.fromFoldable [ Set.singleton A2, Set.singleton B2 ]) false
      itClosedUnderIntersections (Set.fromFoldable [ Set.empty, Set.singleton A2, universe ]) true

    describe "isValidTopologySpace" do
      let discreteTopology = Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
      let indiscreteTopology = Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
      let invalidTopology = Set.fromFoldable [ Set.singleton A2 ]
      let emptyTopology = Set.empty :: TopologySpace FinSet2
      let missingEmptyTopology = Set.fromFoldable [ Set.singleton A2, universe ]

      itIsValidTopologySpace discreteTopology true
      itIsValidTopologySpace indiscreteTopology true
      itIsValidTopologySpace invalidTopology false
      itIsValidTopologySpace emptyTopology false
      itIsValidTopologySpace missingEmptyTopology false

    describe "isHausdorff" do
      let discreteTopology = Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
      let indiscreteTopology = Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
      let
        hausdorffTopology =
          Set.fromFoldable
            [ Set.empty
            , Set.singleton A3
            , Set.singleton B3
            , Set.singleton C3
            , Set.fromFoldable [ A3, B3 ]
            , Set.fromFoldable [ B3, C3 ]
            , Set.fromFoldable [ A3, C3 ]
            , universe
            ] :: TopologySpace FinSet3
      let insufficientSepTopology = Set.fromFoldable [ Set.empty, Set.singleton A3, universe ] :: TopologySpace FinSet3

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
        [ Set.fromFoldable [ Set.empty, universe ]
        , Set.fromFoldable [ Set.empty, Set.singleton A2, universe ]
        , Set.fromFoldable [ Set.empty, Set.singleton B2, universe ]
        , Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
        ]
    (Set.filter isValidTopologySpace powersetUniverse2) `shouldEqual` validTopologies
    let
      validHausdorff = Set.fromFoldable
        [ Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ]
        ]
    Set.filter isHausdorff (Set.filter isValidTopologySpace powersetUniverse2) `shouldEqual` validHausdorff
    Set.filter isHausdorff (powersetUniverse2) `shouldEqual` validHausdorff

  describe "topology properties" do
    let
      topologiesList :: Array { set :: TopologySpace FinSet3, closedUnderUnions :: Boolean, closedUnderIntersections :: Boolean, isValidTopologySpace :: Boolean }
      topologiesList =
        [ { set: Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3, Set.fromFoldable [ B3, C3 ] ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.fromFoldable [ A3, C3 ] ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.fromFoldable [ A3, C3 ], Set.singleton B3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe, Set.fromFoldable [ A3, C3 ], Set.singleton B3, Set.fromFoldable [ B3, C3 ], Set.singleton C3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, universe ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, universe, Set.fromFoldable [ B3, C3 ] ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, universe, Set.fromFoldable [ A3, C3 ] ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, universe, Set.fromFoldable [ A3, C3 ], Set.singleton C3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.singleton A3, universe, Set.fromFoldable [ A3, C3 ], Set.fromFoldable [ B3, C3 ], Set.singleton C3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe, Set.singleton C3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe, Set.singleton B3, Set.fromFoldable [ B3, C3 ] ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, universe ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, universe, Set.singleton B3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, universe, Set.fromFoldable [ B3, C3 ] ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        , { set: Set.fromFoldable [ Set.empty, universe, Set.fromFoldable [ B3, C3 ], Set.singleton C3 ]
          , closedUnderUnions: true
          , closedUnderIntersections: true
          , isValidTopologySpace: true
          }
        ]

    for_ topologiesList \testCase -> do
      let
        topologyString = pretty testCase.set
        makeIt s property expected =
          it (s <> if expected then " should be ✅" else " should be ❌") do
            unless (property testCase.set == expected) $ fail $ ""

      describe topologyString do
        makeIt "closedUnderUnions" closedUnderUnions testCase.closedUnderUnions
        makeIt "closedUnderIntersections" closedUnderIntersections testCase.closedUnderIntersections
        makeIt "isValidTopologySpace" isValidTopologySpace testCase.isValidTopologySpace

    it "Powerset of powerset of universe (3 elements)" do
      let powersetUniverse = powerset universe
      let powersetUniverse2 = powerset powersetUniverse
      let
        allTopologies = Set.fromFoldable
          [ Set.empty
          , (Set.fromFoldable [ Set.empty ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, universe, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ Set.empty, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), universe, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), universe, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ universe ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ universe, (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ A3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ B3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ B3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ B3, C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ B3, C3 ]), (Set.fromFoldable [ C3 ]) ])
          , (Set.fromFoldable [ (Set.fromFoldable [ C3 ]) ])
          ]

      powersetUniverse2 `shouldEqual` allTopologies

      let
        validTopologies = Set.fromFoldable
          [ Set.fromFoldable [ Set.empty, universe ] -- τ1: Indiscrete Topology
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton B3, Set.singleton C3, Set.fromFoldable [ A3, B3 ], Set.fromFoldable [ A3, C3 ], Set.fromFoldable [ B3, C3 ], universe ] -- τ2: Discrete Topology
          , Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, B3 ], universe ] -- τ3: Included Set Topology {a,b}
          , Set.fromFoldable [ Set.empty, Set.fromFoldable [ A3, C3 ], universe ] -- τ4: Included Set Topology {a,c}
          , Set.fromFoldable [ Set.empty, Set.fromFoldable [ B3, C3 ], universe ] -- τ5: Included Set Topology {b,c}
          , Set.fromFoldable [ Set.empty, Set.singleton A3, universe ] -- τ6: Excluded Set Topology {b,c}
          , Set.fromFoldable [ Set.empty, Set.singleton B3, universe ] -- τ7: Excluded Set Topology {a,c}
          , Set.fromFoldable [ Set.empty, Set.singleton C3, universe ] -- τ8: Excluded Set Topology {a,b}
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ B3, C3 ], universe ] -- τ9: Partition Topology {a | b,c}
          , Set.fromFoldable [ Set.empty, Set.singleton B3, Set.fromFoldable [ A3, C3 ], universe ] -- τ10: Partition Topology {b | a,c}
          , Set.fromFoldable [ Set.empty, Set.singleton C3, Set.fromFoldable [ A3, B3 ], universe ] -- τ11: Partition Topology {c | a,b}
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], universe ] -- τ12: Order Topology a≼b≼c
          , Set.fromFoldable [ Set.empty, Set.singleton B3, Set.fromFoldable [ A3, B3 ], universe ] -- τ13: Order Topology b≼a≼c
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, C3 ], universe ] -- τ14: Order Topology a≼c≼b
          , Set.fromFoldable [ Set.empty, Set.singleton C3, Set.fromFoldable [ A3, C3 ], universe ] -- τ15: Order Topology c≼a≼b
          , Set.fromFoldable [ Set.empty, Set.singleton B3, Set.fromFoldable [ B3, C3 ], universe ] -- τ16: Order Topology b≼c≼a
          , Set.fromFoldable [ Set.empty, Set.singleton C3, Set.fromFoldable [ B3, C3 ], universe ] -- τ17: Order Topology c≼b≼a
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.fromFoldable [ A3, B3 ], Set.fromFoldable [ A3, C3 ], universe ] -- τ18: Particular Point Topology (a)
          , Set.fromFoldable [ Set.empty, Set.singleton B3, Set.fromFoldable [ A3, B3 ], Set.fromFoldable [ B3, C3 ], universe ] -- τ19: Particular Point Topology (b)
          , Set.fromFoldable [ Set.empty, Set.singleton C3, Set.fromFoldable [ A3, C3 ], Set.fromFoldable [ B3, C3 ], universe ] -- τ20: Particular Point Topology (c)
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton B3, Set.fromFoldable [ A3, B3 ], universe ] -- τ21: Excluded Point Topology (c)
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton C3, Set.fromFoldable [ A3, C3 ], universe ] -- τ22: Excluded Point Topology (b)
          , Set.fromFoldable [ Set.empty, Set.singleton B3, Set.singleton C3, Set.fromFoldable [ B3, C3 ], universe ] -- τ23: Excluded Point Topology (a)
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton B3, Set.fromFoldable [ A3, B3 ], Set.fromFoldable [ A3, C3 ], universe ] -- τ24
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton B3, Set.fromFoldable [ A3, B3 ], Set.fromFoldable [ B3, C3 ], universe ] -- τ25
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton C3, Set.fromFoldable [ A3, B3 ], Set.fromFoldable [ A3, C3 ], universe ] -- τ26
          , Set.fromFoldable [ Set.empty, Set.singleton A3, Set.singleton C3, Set.fromFoldable [ A3, B3 ], Set.fromFoldable [ B3, C3 ], universe ] -- τ27
          , Set.fromFoldable [ Set.empty, Set.singleton B3, Set.singleton C3, Set.fromFoldable [ A3, B3 ], Set.fromFoldable [ B3, C3 ], universe ] -- τ28
          , Set.fromFoldable [ Set.empty, Set.singleton B3, Set.singleton C3, Set.fromFoldable [ A3, C3 ], Set.fromFoldable [ B3, C3 ], universe ] -- τ29
          ]
        actualTopologies = Set.filter isValidTopologySpace powersetUniverse2

      -- log $ pretty $ Set.filter (not isValidTopologySpace) validTopologies
      -- Set.size validTopologies `shouldEqual` 28
      -- Set.size (Set.filter isValidTopologySpace validTopologies) `shouldEqual` 28
      -- Set.size actualTopologies `shouldEqual` 28

      when (actualTopologies /= validTopologies) do
        -- log $ "actual - valid"
        for_ (Set.filter (not isValidTopologySpace) validTopologies) \actualTopology -> do
          log $ pretty actualTopology
          log $ "  containsEmptySet = " <> show (containsEmptySet actualTopology)
          log $ "  containsParentSet = " <> show (containsParentSet actualTopology)
          log $ "  closedUnderUnions = " <> show (closedUnderUnions actualTopology)
          log $ "  closedUnderIntersections = " <> show (closedUnderIntersections actualTopology)
        -- log $ "valid - actual"
        -- for_ (Set.difference validTopologies actualTopologies) \actualTopology -> do
        --   log $ pretty actualTopology
        --   log $ "  containsEmptySet = " <> show (containsEmptySet actualTopology)
        --   log $ "  containsParentSet = " <> show (containsParentSet actualTopology)
        --   log $ "  closedUnderUnions = " <> show (closedUnderUnions actualTopology)
        --   log $ "  closedUnderIntersections = " <> show (closedUnderIntersections actualTopology)
        fail $ pretty actualTopologies <> " ≠ " <> pretty validTopologies

-- let
--   validHausdorff = Set.fromFoldable
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
