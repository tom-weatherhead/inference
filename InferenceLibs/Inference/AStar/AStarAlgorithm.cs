#define TRAVERSE_AND_OPTIMIZE_COSTS

using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;
using Inference.Utilities;

namespace Inference.AStar
{
    public interface IAStarPriorityQueueRefresher<T> where T : AStarStateBase, IComparable<T>
    {
        void RefreshPriorityQueue(T state);
    }

    public class AStarAlgorithm<T> : HeuristicSearchAlgorithmBase<T>, IAStarPriorityQueueRefresher<T> where T : AStarStateBase, IComparable<T>
    {
        private readonly PriorityQueue<T> openQueue = new PriorityQueue<T>();
        private readonly HashSet<T> openSet = new HashSet<T>();     // Used to speed up the refreshing of the Open Queue

        public AStarAlgorithm(ISuccessorStateGenerator<T> successorStateGenerator)
            : base(successorStateGenerator)
        {
        }

        public void RefreshPriorityQueue(T state)
        {

            if (openSet.Contains(state))
            {
                openQueue.FindAndUpHeap(state);
            }
        }

        public override T Search(T startState, T goalState)
        {
            successorStateGenerator.StateValidityTest(startState);
            successorStateGenerator.StateValidityTest(goalState);

            openQueue.Clear();
            openSet.Clear();
            closedSet.Clear();
            openQueue.Enqueue(startState);
            openSet.Add(startState);

            while (!openQueue.IsEmpty)
            {
                var currentState = openQueue.Dequeue();

                openSet.Remove(currentState);
                closedSet.Add(currentState);

                if (currentState.Equals(goalState))
                {
                    return currentState;
                }

                var possibleSuccessorStateData = successorStateGenerator.GenerateSuccessorStates(currentState, startState, goalState);

                foreach (var stateData in possibleSuccessorStateData)
                {
#if TRAVERSE_AND_OPTIMIZE_COSTS

                    if (openSet.Contains(stateData.Key))
                    {
                        //var oldState = FindState(openQueue, stateData.Key);
                        var oldState = openQueue.Find(stateData.Key);

                        currentState.successors.Add(new KeyValuePair<AStarStateBase, int>(oldState, stateData.Value));
                        oldState.TraverseAndOptimizeCosts(currentState, stateData.Value, this);
                    }
                    else if (closedSet.Contains(stateData.Key))
                    {
                        var oldState = FindState(closedSet, stateData.Key);

                        currentState.successors.Add(new KeyValuePair<AStarStateBase, int>(oldState, stateData.Value));
                        oldState.TraverseAndOptimizeCosts(currentState, stateData.Value, this);
                    }
                    else
                    {
                        stateData.Key.TraverseAndOptimizeCosts<T>(currentState, stateData.Value, null);
                        openQueue.Enqueue(stateData.Key);
                        openSet.Add(stateData.Key);
                        currentState.successors.Add(new KeyValuePair<AStarStateBase, int>(stateData.Key, stateData.Value));
                    }
#else
                    if (!openQueue.Contains(state) && !closedSet.Contains(state))
                    {
                        openQueue.Enqueue(state);
                    }
#endif
                }
            }

            return null;
        }

        public override int NumStatesGenerated
        {
            get
            {
                return openQueue.Count + NumStatesExamined;
            }
        }

        public override int NumStatesExamined
        {
            get
            {
                return closedSet.Count;
            }
        }
    }
}
