using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.AStar;
using NUnit.Framework;

namespace Inference.Tests.AStar
{
    [TestFixture]
    public class Decanter_Fixture
    {
        private DecanterState startState;

        [SetUp]
        public void SetUp()
        {
            startState = new DecanterState(0, 0);
        }

        [Test]
        public void DecanterTest1()
        {
            var ssg = new DecanterSuccessorStateGenerator(7, 11);
            var goalState = new DecanterState(0, 5);
            var algorithm = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var solutionState = algorithm.Search(startState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(13, solutionSteps.Count);
            Assert.AreEqual("Fill jug 2. (0, 11)", solutionSteps[0]);
            Assert.AreEqual("Pour jug 2 into jug 1. (7, 4)", solutionSteps[1]);
            Assert.AreEqual("Empty jug 1. (0, 4)", solutionSteps[2]);
            Assert.AreEqual("Pour jug 2 into jug 1. (4, 0)", solutionSteps[3]);
            Assert.AreEqual("Fill jug 2. (4, 11)", solutionSteps[4]);
            Assert.AreEqual("Pour jug 2 into jug 1. (7, 8)", solutionSteps[5]);
            Assert.AreEqual("Empty jug 1. (0, 8)", solutionSteps[6]);
            Assert.AreEqual("Pour jug 2 into jug 1. (7, 1)", solutionSteps[7]);
            Assert.AreEqual("Empty jug 1. (0, 1)", solutionSteps[8]);
            Assert.AreEqual("Pour jug 2 into jug 1. (1, 0)", solutionSteps[9]);
            Assert.AreEqual("Fill jug 2. (1, 11)", solutionSteps[10]);
            Assert.AreEqual("Pour jug 2 into jug 1. (7, 5)", solutionSteps[11]);
            Assert.AreEqual("Empty jug 1. (0, 5)", solutionSteps[12]);
        }

        [Test]
        public void DecanterTest1_ID()  // Iterative Deepening
        {
            var ssg = new DecanterSuccessorStateGenerator(7, 11);
            var goalState = new DecanterState(0, 5);
            var algorithmID = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.IterativeDeepeningAStar, ssg);
            var solutionState = algorithmID.Search(startState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(13, solutionSteps.Count);
            Assert.AreEqual("Fill jug 2. (0, 11)", solutionSteps[0]);
            Assert.AreEqual("Pour jug 2 into jug 1. (7, 4)", solutionSteps[1]);
            Assert.AreEqual("Empty jug 1. (0, 4)", solutionSteps[2]);
            Assert.AreEqual("Pour jug 2 into jug 1. (4, 0)", solutionSteps[3]);
            Assert.AreEqual("Fill jug 2. (4, 11)", solutionSteps[4]);
            Assert.AreEqual("Pour jug 2 into jug 1. (7, 8)", solutionSteps[5]);
            Assert.AreEqual("Empty jug 1. (0, 8)", solutionSteps[6]);
            Assert.AreEqual("Pour jug 2 into jug 1. (7, 1)", solutionSteps[7]);
            Assert.AreEqual("Empty jug 1. (0, 1)", solutionSteps[8]);
            Assert.AreEqual("Pour jug 2 into jug 1. (1, 0)", solutionSteps[9]);
            Assert.AreEqual("Fill jug 2. (1, 11)", solutionSteps[10]);
            Assert.AreEqual("Pour jug 2 into jug 1. (7, 5)", solutionSteps[11]);
            Assert.AreEqual("Empty jug 1. (0, 5)", solutionSteps[12]);
        }

        [Test]
        public void DecanterTest2NoSolution()
        {
            var ssg = new DecanterSuccessorStateGenerator(4, 6);
            var goalState = new DecanterState(0, 1);    // At least one of the goal volumes is not divisible by GCD(capacity1, capacity2).
            var algorithm = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var solutionState = algorithm.Search(startState, goalState);

            Assert.IsNull(solutionState);
        }

        [Test]
        public void DecanterTest3NothingToDo()
        {
            var ssg = new DecanterSuccessorStateGenerator(7, 11);
            var algorithm = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var solutionState = algorithm.Search(startState, startState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(0, solutionSteps.Count);
        }

        [Test]
        public void GoalVolume1TooSmallTest()
        {
            var ssg = new DecanterSuccessorStateGenerator(7, 11);
            var algorithm = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var goalState = new DecanterState(-1, 0);

            Assert.Throws<HeuristicSearchStateException>(() => algorithm.Search(startState, goalState));
        }

        [Test]
        public void GoalVolume1TooLargeTest()
        {
            var ssg = new DecanterSuccessorStateGenerator(7, 11);
            var algorithm = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var goalState = new DecanterState(8, 0);

            Assert.Throws<HeuristicSearchStateException>(() => algorithm.Search(startState, goalState));
        }

        [Test]
        public void GoalVolume2TooSmallTest()
        {
            var ssg = new DecanterSuccessorStateGenerator(7, 11);
            var algorithm = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var goalState = new DecanterState(0, -1);

            Assert.Throws<HeuristicSearchStateException>(() => algorithm.Search(startState, goalState));
        }

        [Test]
        public void GoalVolume2TooLargeTest()
        {
            var ssg = new DecanterSuccessorStateGenerator(7, 11);
            var algorithm = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var goalState = new DecanterState(0, 12);

            Assert.Throws<HeuristicSearchStateException>(() => algorithm.Search(startState, goalState));
        }
    }
}
