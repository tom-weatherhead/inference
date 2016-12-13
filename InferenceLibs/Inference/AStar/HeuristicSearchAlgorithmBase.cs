using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Inference.AStar
{
    public enum HeuristicSearchAlgorithmType
    {
        AStar,
        IterativeDeepeningAStar
    }

    public interface ISuccessorStateGenerator<T> where T : AStarStateBase
    {
        void StateValidityTest(T state);    // This will throw an exception if the given state is invalid.
        IEnumerable<KeyValuePair<T, int>> GenerateSuccessorStates(T currentState, T startState, T goalState);  // TODO: If possible, do not pass the start state.
    }

    public interface IHeuristicSearchAlgorithm<T> where T : AStarStateBase
    {
        T Search(T startState, T goalState);
        bool SearchAndReport(T startState, T goalState);
        int NumStatesGenerated { get; }
        int NumStatesExamined { get; }
    }

    public class HeuristicSearchStateException : Exception
    {
        public HeuristicSearchStateException(string message)
            : base(message)
        {
        }
    }

    public abstract class HeuristicSearchAlgorithmBase<T> : IHeuristicSearchAlgorithm<T> where T : AStarStateBase
    {
        protected readonly HashSet<T> closedSet = new HashSet<T>();
        protected readonly ISuccessorStateGenerator<T> successorStateGenerator;

        protected HeuristicSearchAlgorithmBase(ISuccessorStateGenerator<T> successorStateGenerator)
        {
            this.successorStateGenerator = successorStateGenerator;
        }

        protected T FindState(IEnumerable<T> collection, T state)
        {

            foreach (var stateInCollection in collection)
            {

                if (stateInCollection.Equals(state))
                {
                    return stateInCollection;
                }
            }

            return null;
        }

        public abstract T Search(T startState, T goalState);

        // TODO: We could make OpenListCount and ClosedListCount public, so that they can be used for measuring algorithm efficiency.
        //protected abstract int OpenListCount { get; }

        public bool Report(T solutionState)
        {

            if (solutionState == null)
            {
                Console.WriteLine("No solution found.");
                Console.WriteLine(string.Format("{0} state(s) generated and examined.", NumStatesGenerated));
                return false;
            }

            solutionState.PrintSolution();
            Console.WriteLine(string.Format("{0} state(s) generated; {1} state(s) examined.",
                NumStatesGenerated, NumStatesExamined));
            return true;
        }

        public bool SearchAndReport(T startState, T goalState)
        {
            return Report(Search(startState, goalState));
        }

        public abstract int NumStatesGenerated { get; }
        public abstract int NumStatesExamined { get; }
    }

    public static class HeuristicSearchAlgorithmFactory
    {
        public static IHeuristicSearchAlgorithm<T> Create<T>(HeuristicSearchAlgorithmType algorithmType,
            ISuccessorStateGenerator<T> successorStateGenerator) where T : AStarStateBase
        {

            switch (algorithmType)
            {
                case HeuristicSearchAlgorithmType.AStar:
                    return new AStarAlgorithm<T>(successorStateGenerator);

                case HeuristicSearchAlgorithmType.IterativeDeepeningAStar:
                    return new IDAStarAlgorithm<T>(successorStateGenerator);

                default:
                    throw new ArgumentException("Invalid algorithm type", "algorithmType");
            }
        }
    }
}
