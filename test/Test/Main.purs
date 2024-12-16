module Test.Main where

import FinSet2
import FinSet3
import Prelude
import Topology
import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, notElem, elem)
import Data.Newtype (class Newtype)
import Effect.Exception (Error, error)

spec :: Spec Unit
spec = do
  describe "Topology" do
    describe "containsEmptySet" do
      it "should return true when topology contains empty set" do
        containsEmptySet (Set.singleton Set.empty :: TopologySpace FinSet2) `shouldEqual` true
      it "should return false when topology doesn't contain empty set" do
        containsEmptySet (Set.singleton (Set.singleton A2)) `shouldEqual` false
      it "should handle multiple empty sets" do
        let topology = Set.fromFoldable [ Set.empty, Set.empty ] :: TopologySpace FinSet2
        containsEmptySet topology `shouldEqual` true

    describe "containsParentSet" do
      it "should return true when topology contains universe" do
        let topology = Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
        containsParentSet topology `shouldEqual` true
      it "should return false when topology doesn't contain universe" do
        let topology = Set.fromFoldable [ Set.empty, Set.singleton A2 ]
        containsParentSet topology `shouldEqual` false
      it "should handle subset of universe" do
        let topology = Set.fromFoldable [ Set.singleton A2, Set.singleton B2 ]
        containsParentSet topology `shouldEqual` false

    describe "closedUnderUnions" do
      it "should validate topology closed under unions" do
        let topology = Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ] :: TopologySpace FinSet2
        closedUnderUnions topology `shouldEqual` true
      it "should reject topology not closed under unions" do
        let topology = Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2 ] :: TopologySpace FinSet2
        closedUnderUnions topology `shouldEqual` false
      it "should handle empty topology" do
        closedUnderUnions (Set.empty :: TopologySpace FinSet2) `shouldEqual` false
      it "should handle singleton topology" do
        let topology = Set.singleton universe :: TopologySpace FinSet2
        closedUnderUnions topology `shouldEqual` true

    describe "closedUnderIntersections" do
      it "should validate topology closed under intersections" do
        let topology = Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ] :: TopologySpace FinSet2
        closedUnderIntersections topology `shouldEqual` true
      it "should reject topology not closed under intersections" do
        let topology = Set.fromFoldable [ Set.singleton A2, Set.singleton B2 ] :: TopologySpace FinSet2
        closedUnderIntersections topology `shouldEqual` false
      it "should handle empty intersection" do
        let topology = Set.fromFoldable [ Set.empty, Set.singleton A2, universe ] :: TopologySpace FinSet2
        closedUnderIntersections topology `shouldEqual` true

    describe "isValidTopologySpace" do
      it "should validate discrete topology" do
        let discreteTopology = Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ] :: TopologySpace FinSet2
        isValidTopologySpace discreteTopology `shouldEqual` true
      it "should validate indiscrete topology" do
        let indiscreteTopology = Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
        isValidTopologySpace indiscreteTopology `shouldEqual` true
      it "should reject invalid topologies" do
        let invalidTopology = Set.fromFoldable [ Set.singleton A2 ] :: TopologySpace FinSet2
        isValidTopologySpace invalidTopology `shouldEqual` false
      it "should handle empty topology" do
        isValidTopologySpace (Set.empty :: TopologySpace FinSet2) `shouldEqual` false
      it "should handle missing empty set" do
        let topology = Set.fromFoldable [ Set.singleton A2, universe ] :: TopologySpace FinSet2
        isValidTopologySpace topology `shouldEqual` false

    describe "isHausdorff" do
      it "should validate Hausdorff spaces" do
        let discreteTopology = Set.fromFoldable [ Set.empty, Set.singleton A2, Set.singleton B2, universe ] :: TopologySpace FinSet2
        isHausdorff discreteTopology `shouldEqual` true
      it "should reject non-Hausdorff spaces" do
        let indiscreteTopology = Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
        isHausdorff indiscreteTopology `shouldEqual` false
      it "should handle three-point Hausdorff space" do
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
        isHausdorff hausdorffTopology `shouldEqual` true
      it "should reject insufficient separation" do
        let topology = Set.fromFoldable [ Set.empty, Set.singleton A3, universe ] :: TopologySpace FinSet3
        isHausdorff topology `shouldEqual` false

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
    Set.filter isValidTopologySpace powersetUniverse2 `shouldEqualTopologicalSets` expectedPowersetOfPowerset

shouldEqualTopologicalSets
  :: forall m a
   . MonadThrow Error m
  => ShowTopologicalSpaceElement a
  => Ord a
  => Eq a
  => Set (TopologySpace a)
  -> Set (TopologySpace a)
  -> m Unit
shouldEqualTopologicalSets a b = (Set.map ShowTopologicalSpace a) `shouldEqual` (Set.map ShowTopologicalSpace b)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec
