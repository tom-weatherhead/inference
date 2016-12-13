using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;
using Inference.Utilities;

namespace Inference.AStar
{
    #region AStarStateBase

    public abstract class AStarStateBase : IComparable<AStarStateBase>
    {
        public AStarStateBase parent;
        // The int in the following line is the cost of going from the current state to the corresponding successor state.
        public List<KeyValuePair<AStarStateBase, int>> successors = new List<KeyValuePair<AStarStateBase, int>>();
        public readonly string solutionStep;
        public int f;               // g + h
        public int g;               // The actual cost to go from the start state to this state.
        private readonly int h;     // The estimated cost to go from this state to the goal state.

        public AStarStateBase(AStarStateBase previousState, string newStep, int gParam, int hParam)
        {
            parent = previousState;
            solutionStep = newStep;
            g = gParam;
            h = hParam;
            f = g + h;
        }

        public int CompareTo(AStarStateBase otherState)
        {
            return otherState.f - f;    // The state with the smaller f has the higher priority.
        }

        public void TraverseAndOptimizeCosts<T>(AStarStateBase prospectiveParent, int costFromProspectiveParent,
            IAStarPriorityQueueRefresher<T> refresher) where T : AStarStateBase, IComparable<T>
        {
            var gProspective = prospectiveParent.g + costFromProspectiveParent;

            if (gProspective >= g)
            {
                return;
            }

            parent = prospectiveParent;
            g = gProspective;
            f = g + h;

            if (refresher != null)
            {
                refresher.RefreshPriorityQueue((T)this);
            }

            foreach (var successorData in successors)
            {
                successorData.Key.TraverseAndOptimizeCosts(this, successorData.Value, refresher);
            }
        }

        public void AddSolutionStep(List<string> solutionSteps)
        {

            if (parent != null)
            {
                parent.AddSolutionStep(solutionSteps);
            }

            if (!string.IsNullOrEmpty(solutionStep))
            {
                solutionSteps.Add(solutionStep);
            }
        }

        public List<string> CompileSolution()
        {
            var solution = new List<string>();

            AddSolutionStep(solution);
            return solution;
        }

        public void PrintSolution()
        {
            Console.WriteLine("Solution steps:");

            foreach (string step in CompileSolution())
            {
                Console.WriteLine("  " + step);
            }
        }
    }

    #endregion

#if DEAD_CODE

    #region AStarBase<T>

    public abstract class AStarBase<T> where T : AStarStateBase, IComparable<T>
    {
	    protected readonly IQueue<T> m_OpenQueue = new PriorityQueue<T>();
        protected readonly HashSet<T> closedSet = new HashSet<T>();

	    protected abstract void GenerateSuccessorStates(T CurState, T StartState, T GoalState);

	    protected T Search(T StartState, T GoalState)
    	{
    		m_OpenQueue.Clear();
            closedSet.Clear();
		    m_OpenQueue.Enqueue(StartState);

		    while (!m_OpenQueue.IsEmpty)
		    {
			    T CurState = m_OpenQueue.Dequeue();

			    if (CurState.Equals(GoalState))
			    {
				    return CurState;
			    }

                closedSet.Add(CurState);
			    GenerateSuccessorStates(CurState, StartState, GoalState);
		    }

		    return null;
	    }

        public bool Report(T solutionState)
        {

            if (solutionState == null)
            {
                Console.WriteLine("No solution found.");
                Console.WriteLine(string.Format("{0} state(s) generated and examined.", closedSet.Count));
                return false;
            }

            solutionState.PrintSolution();
            Console.WriteLine(string.Format("{0} state(s) generated; {1} state(s) examined.",
                m_OpenQueue.Count + closedSet.Count + 1, closedSet.Count + 1));
            return true;
        }
    }

    #endregion

#endif
}
