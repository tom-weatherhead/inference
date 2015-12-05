using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.AStar;
using NUnit.Framework;

namespace Inference.Tests.AStar
{
    [TestFixture]
    public class CanMiss_Fixture
    {
        private IHeuristicSearchAlgorithm<CanMissState> algorithm = null;

        [SetUp]
        public void SetUp()
        {
            var ssg = new CanMissSuccessorStateGenerator();

            algorithm = HeuristicSearchAlgorithmFactory.Create<CanMissState>(HeuristicSearchAlgorithmType.AStar, ssg);
        }

        private void Helper(int c, int m)
        {
            var startState = CanMissState.CalculateStartState(c, m);
            var goalState = startState.CalculateGoalState();

            algorithm.Search(startState, goalState);
        }

        [Test]
        public void CanMissTest1()
        {
            var startState = CanMissState.CalculateStartState(4, 5);
            var goalState = startState.CalculateGoalState();
            var solutionState = algorithm.Search(startState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(15, solutionSteps.Count);
            Assert.AreEqual("Two cannibals travel from West to East. (W 2, 5; E 2, 0)", solutionSteps[0]);
            Assert.AreEqual("One cannibal travels from East to West. (W 3, 5; E 1, 0)", solutionSteps[1]);
            Assert.AreEqual("Two missionaries travel from West to East. (W 3, 3; E 1, 2)", solutionSteps[2]);
            Assert.AreEqual("One missionary travels from East to West. (W 3, 4; E 1, 1)", solutionSteps[3]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 2, 3; E 2, 2)", solutionSteps[4]);
            Assert.AreEqual("One cannibal travels from East to West. (W 3, 3; E 1, 2)", solutionSteps[5]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 2, 2; E 2, 3)", solutionSteps[6]);
            Assert.AreEqual("One missionary travels from East to West. (W 2, 3; E 2, 2)", solutionSteps[7]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 1, 2; E 3, 3)", solutionSteps[8]);
            Assert.AreEqual("One cannibal travels from East to West. (W 2, 2; E 2, 3)", solutionSteps[9]);
            Assert.AreEqual("Two missionaries travel from West to East. (W 2, 0; E 2, 5)", solutionSteps[10]);
            Assert.AreEqual("One cannibal travels from East to West. (W 3, 0; E 1, 5)", solutionSteps[11]);
            Assert.AreEqual("Two cannibals travel from West to East. (W 1, 0; E 3, 5)", solutionSteps[12]);
            Assert.AreEqual("One cannibal travels from East to West. (W 2, 0; E 2, 5)", solutionSteps[13]);
            Assert.AreEqual("Two cannibals travel from West to East. (W 0, 0; E 4, 5)", solutionSteps[14]);
        }

        [Test]
        public void CanMissTest1_ID()   // Iterative Deepening
        {
            var ssg = new CanMissSuccessorStateGenerator();
            var algorithmID = HeuristicSearchAlgorithmFactory.Create<CanMissState>(HeuristicSearchAlgorithmType.IterativeDeepeningAStar, ssg);
            var startState = CanMissState.CalculateStartState(4, 5);
            var goalState = startState.CalculateGoalState();
            var solutionState = algorithmID.Search(startState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(15, solutionSteps.Count);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 3, 4; E 1, 1)", solutionSteps[0]);
            Assert.AreEqual("One missionary travels from East to West. (W 3, 5; E 1, 0)", solutionSteps[1]);
            Assert.AreEqual("Two missionaries travel from West to East. (W 3, 3; E 1, 2)", solutionSteps[2]);
            Assert.AreEqual("One missionary travels from East to West. (W 3, 4; E 1, 1)", solutionSteps[3]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 2, 3; E 2, 2)", solutionSteps[4]);
            Assert.AreEqual("One cannibal travels from East to West. (W 3, 3; E 1, 2)", solutionSteps[5]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 2, 2; E 2, 3)", solutionSteps[6]);
            Assert.AreEqual("One missionary travels from East to West. (W 2, 3; E 2, 2)", solutionSteps[7]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 1, 2; E 3, 3)", solutionSteps[8]);
            Assert.AreEqual("One cannibal travels from East to West. (W 2, 2; E 2, 3)", solutionSteps[9]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 1, 1; E 3, 4)", solutionSteps[10]);
            Assert.AreEqual("One missionary travels from East to West. (W 1, 2; E 3, 3)", solutionSteps[11]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 0, 1; E 4, 4)", solutionSteps[12]);
            Assert.AreEqual("One cannibal travels from East to West. (W 1, 1; E 3, 4)", solutionSteps[13]);
            Assert.AreEqual("One cannibal and one missionary travel from West to East. (W 0, 0; E 4, 5)", solutionSteps[14]);
        }

        [Test]
        public void CanMissTest2CannibalsOnly()
        {
            var startState = CanMissState.CalculateStartState(2, 0);
            var goalState = startState.CalculateGoalState();
            var solutionState = algorithm.Search(startState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(1, solutionSteps.Count);
            Assert.AreEqual("Two cannibals travel from West to East. (W 0, 0; E 2, 0)", solutionSteps[0]);
        }

        [Test]
        public void CanMissTest3NoOne()
        {
            var startState = CanMissState.CalculateStartState(0, 0);
            var goalState = startState.CalculateGoalState();
            var solutionState = algorithm.Search(startState, goalState);

            // There is no solution because there is no one to take the canoe across the river from the west bank to the east bank.
            Assert.IsNull(solutionState);
        }

        [Test]
        public void TooFewCannibalsTest()
        {
            Assert.Throws<HeuristicSearchStateException>(() => Helper(-1, 0));
        }

        [Test]
        public void TooFewMissionariesTest()
        {
            Assert.Throws<HeuristicSearchStateException>(() => Helper(0, -1));
        }

        [Test]
        public void TooManyCannibalsTest()
        {
            Assert.Throws<HeuristicSearchStateException>(() => Helper(2, 1));
        }
    }
}
