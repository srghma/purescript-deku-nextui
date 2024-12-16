module Test.Main where

import FinSet2
import FinSet3
import Prelude
import Topology

import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

spec :: Spec Unit
spec = do
  describe "Topology" do
    describe "containsEmptySet" do
      it "should return true when topology contains empty set" do
        containsEmptySet (Set.singleton Set.empty :: TopologySpace FinSet2) `shouldEqual` true

      it "should return false when topology doesn't contain empty set" do
        containsEmptySet (Set.singleton (Set.singleton A2)) `shouldEqual` false

    describe "containsParentSet" do
      it "should return true when topology contains universe" do
        let topology = Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
        containsParentSet topology `shouldEqual` true

      it "should return false when topology doesn't contain universe" do
        let topology = Set.fromFoldable [ Set.empty, Set.singleton A2 ]
        containsParentSet topology `shouldEqual` false

    describe "isValidTopologySpace" do
      it "should validate discrete topology" do
        let
          discreteTopology =
            Set.fromFoldable
              [ Set.empty
              , Set.singleton A2
              , Set.singleton B2
              , universe
              ] :: TopologySpace FinSet2
        isValidTopologySpace discreteTopology `shouldEqual` true

      it "should validate indiscrete topology" do
        let indiscreteTopology = Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
        isValidTopologySpace indiscreteTopology `shouldEqual` true

      it "should reject invalid topologies" do
        let invalidTopology = Set.fromFoldable [ Set.singleton A2 ] :: TopologySpace FinSet2
        isValidTopologySpace invalidTopology `shouldEqual` false

    describe "isHausdorff" do
      it "should validate Hausdorff spaces" do
        let
          discreteTopology =
            Set.fromFoldable
              [ Set.empty
              , Set.singleton A2
              , Set.singleton B2
              , universe
              ] :: TopologySpace FinSet2
        isHausdorff discreteTopology `shouldEqual` true

      it "should reject non-Hausdorff spaces" do
        let indiscreteTopology = Set.fromFoldable [ Set.empty, universe ] :: TopologySpace FinSet2
        isHausdorff indiscreteTopology `shouldEqual` false

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec
