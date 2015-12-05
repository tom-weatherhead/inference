using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.AStar;
using NUnit.Framework;

namespace Inference.Tests.AStar
{
    [TestFixture]
    public class EightPuzzle_Fixture
    {
        private IHeuristicSearchAlgorithm<EightPuzzleState> algorithm = null;
        private EightPuzzleState goalState;

        [SetUp]
        public void SetupTest()
        {
            var ssg = new EightPuzzleSuccessorStateGenerator();

            algorithm = HeuristicSearchAlgorithmFactory.Create<EightPuzzleState>(HeuristicSearchAlgorithmType.AStar, ssg);
            goalState = EightPuzzleState.CreateGoal();
        }

        [Test]
        public void Test1NothingToDo()
        {
            var solutionState = algorithm.Search(goalState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(0, solutionSteps.Count);
        }

        [Test]
        public void Test2()
        {
            var startState = new EightPuzzleState(new List<int>() { 0, 6, 2, 5, 1, 7, 8, 3, 4 });
            var solutionState = algorithm.Search(startState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(16, solutionSteps.Count);
            Assert.AreEqual("Slide one tile to the left. (6, 0, 2, 5, 1, 7, 8, 3, 4)", solutionSteps[0]);
            Assert.AreEqual("Slide two tiles up. (6, 1, 2, 5, 3, 7, 8, 0, 4)", solutionSteps[1]);
            Assert.AreEqual("Slide one tile to the left. (6, 1, 2, 5, 3, 7, 8, 4, 0)", solutionSteps[2]);
            Assert.AreEqual("Slide one tile down. (6, 1, 2, 5, 3, 0, 8, 4, 7)", solutionSteps[3]);
            Assert.AreEqual("Slide two tiles to the right. (6, 1, 2, 0, 5, 3, 8, 4, 7)", solutionSteps[4]);
            Assert.AreEqual("Slide one tile down. (0, 1, 2, 6, 5, 3, 8, 4, 7)", solutionSteps[5]);
            Assert.AreEqual("Slide two tiles to the left. (1, 2, 0, 6, 5, 3, 8, 4, 7)", solutionSteps[6]);
            Assert.AreEqual("Slide one tile up. (1, 2, 3, 6, 5, 0, 8, 4, 7)", solutionSteps[7]);
            Assert.AreEqual("Slide one tile to the right. (1, 2, 3, 6, 0, 5, 8, 4, 7)", solutionSteps[8]);
            Assert.AreEqual("Slide one tile up. (1, 2, 3, 6, 4, 5, 8, 0, 7)", solutionSteps[9]);
            Assert.AreEqual("Slide one tile to the left. (1, 2, 3, 6, 4, 5, 8, 7, 0)", solutionSteps[10]);
            Assert.AreEqual("Slide one tile down. (1, 2, 3, 6, 4, 0, 8, 7, 5)", solutionSteps[11]);
            Assert.AreEqual("Slide two tiles to the right. (1, 2, 3, 0, 6, 4, 8, 7, 5)", solutionSteps[12]);
            Assert.AreEqual("Slide one tile up. (1, 2, 3, 8, 6, 4, 0, 7, 5)", solutionSteps[13]);
            Assert.AreEqual("Slide one tile to the left. (1, 2, 3, 8, 6, 4, 7, 0, 5)", solutionSteps[14]);
            Assert.AreEqual("Slide one tile down. (1, 2, 3, 8, 0, 4, 7, 6, 5)", solutionSteps[15]);
        }

        [Test]
        public void Test2_ID()  // Iterative Deepening
        {
            var ssg = new EightPuzzleSuccessorStateGenerator();
            var algorithmID = HeuristicSearchAlgorithmFactory.Create<EightPuzzleState>(HeuristicSearchAlgorithmType.IterativeDeepeningAStar, ssg);
            var startState = new EightPuzzleState(new List<int>() { 0, 6, 2, 5, 1, 7, 8, 3, 4 });
            var solutionState = algorithmID.Search(startState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(16, solutionSteps.Count);
            Assert.AreEqual("Slide one tile to the left. (6, 0, 2, 5, 1, 7, 8, 3, 4)", solutionSteps[0]);
            Assert.AreEqual("Slide two tiles up. (6, 1, 2, 5, 3, 7, 8, 0, 4)", solutionSteps[1]);
            Assert.AreEqual("Slide one tile to the left. (6, 1, 2, 5, 3, 7, 8, 4, 0)", solutionSteps[2]);
            Assert.AreEqual("Slide one tile down. (6, 1, 2, 5, 3, 0, 8, 4, 7)", solutionSteps[3]);
            Assert.AreEqual("Slide two tiles to the right. (6, 1, 2, 0, 5, 3, 8, 4, 7)", solutionSteps[4]);
            Assert.AreEqual("Slide one tile down. (0, 1, 2, 6, 5, 3, 8, 4, 7)", solutionSteps[5]);
            Assert.AreEqual("Slide two tiles to the left. (1, 2, 0, 6, 5, 3, 8, 4, 7)", solutionSteps[6]);
            Assert.AreEqual("Slide one tile up. (1, 2, 3, 6, 5, 0, 8, 4, 7)", solutionSteps[7]);
            Assert.AreEqual("Slide one tile to the right. (1, 2, 3, 6, 0, 5, 8, 4, 7)", solutionSteps[8]);
            Assert.AreEqual("Slide one tile up. (1, 2, 3, 6, 4, 5, 8, 0, 7)", solutionSteps[9]);
            Assert.AreEqual("Slide one tile to the left. (1, 2, 3, 6, 4, 5, 8, 7, 0)", solutionSteps[10]);
            Assert.AreEqual("Slide one tile down. (1, 2, 3, 6, 4, 0, 8, 7, 5)", solutionSteps[11]);
            Assert.AreEqual("Slide two tiles to the right. (1, 2, 3, 0, 6, 4, 8, 7, 5)", solutionSteps[12]);
            Assert.AreEqual("Slide one tile up. (1, 2, 3, 8, 6, 4, 0, 7, 5)", solutionSteps[13]);
            Assert.AreEqual("Slide one tile to the left. (1, 2, 3, 8, 6, 4, 7, 0, 5)", solutionSteps[14]);
            Assert.AreEqual("Slide one tile down. (1, 2, 3, 8, 0, 4, 7, 6, 5)", solutionSteps[15]);
        }

        [Test]
        public void Test3OneMove()
        {
            var startState = new EightPuzzleState(new List<int>() { 1, 2, 3, 8, 4, 0, 7, 6, 5 });
            var solutionState = algorithm.Search(startState, goalState);

            Assert.IsNotNull(solutionState);

            var solutionSteps = solutionState.CompileSolution();

            Assert.AreEqual(1, solutionSteps.Count);
            Assert.AreEqual("Slide one tile to the right. (1, 2, 3, 8, 0, 4, 7, 6, 5)", solutionSteps[0]);
        }
    }
}
