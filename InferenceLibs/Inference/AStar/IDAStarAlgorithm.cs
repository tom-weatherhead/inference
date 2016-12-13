// See pages 323 and 324 in "Artificial Intelligence", by Rich and Knight (section 12.5, "Iterative Deepening")

using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;

namespace Inference.AStar
{
    // The Iterative-Deepening-A* Algorithm

    public class IDAStarAlgorithm<T> : HeuristicSearchAlgorithmBase<T> where T : AStarStateBase
    {
        private readonly Stack<T> openStack = new Stack<T>();
        private int closedSetTotalFromPreviousIterations;

        public IDAStarAlgorithm(ISuccessorStateGenerator<T> successorStateGenerator)
            : base(successorStateGenerator)
        {
        }

        public override T Search(T startState, T goalState)
        {
            successorStateGenerator.StateValidityTest(startState);
            successorStateGenerator.StateValidityTest(goalState);

            closedSetTotalFromPreviousIterations = 0;

            var threshold = startState.f;

            for (; ; )
            {
                var thresholdHasBeenSurpassed = false;
                var nextThreshold = int.MaxValue;

                openStack.Clear();
                closedSet.Clear();
                openStack.Push(startState);

                while (openStack.Count > 0)
                {
                    var currentState = openStack.Pop();

                    closedSet.Add(currentState);

                    if (currentState.Equals(goalState))
                    {
                        return currentState;
                    }

                    var possibleSuccessorStatesData = successorStateGenerator.GenerateSuccessorStates(currentState, startState, goalState);

                    foreach (var stateData in possibleSuccessorStatesData)
                    {
                        var state = stateData.Key;

                        if (state.f > threshold)
                        {
                            thresholdHasBeenSurpassed = true;

                            if (state.f < nextThreshold)
                            {
                                nextThreshold = state.f;
                            }
                        }
                        else if (!openStack.Contains(state) && !closedSet.Contains(state))
                        {
                            openStack.Push(state);
                        }
                    }
                }

                if (!thresholdHasBeenSurpassed)
                {
                    break;
                }

                threshold = nextThreshold;
                closedSetTotalFromPreviousIterations += closedSet.Count;
            }

            return null;
        }

        public override int NumStatesGenerated
        {
            get
            {
                return openStack.Count + NumStatesExamined;
            }
        }

        public override int NumStatesExamined
        {
            get
            {
                return closedSet.Count + closedSetTotalFromPreviousIterations;
            }
        }
    }
}
