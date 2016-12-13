using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;

namespace Inference.AStar
{
    public class DecanterState : AStarStateBase
    {
        public readonly int volume1 = 0;
        public readonly int volume2 = 0;

        public DecanterState(int v1, int v2, DecanterState previousState, string newStep, int gParam, int hParam)
            : base(previousState, newStep, gParam, hParam)
        {
            volume1 = v1;
            volume2 = v2;
        }

        public DecanterState(int v1, int v2)
            : this(v1, v2, null, null, 0, 0)
        {
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            DecanterState otherDecanterState = obj as DecanterState;

            return otherDecanterState != null && volume1 == otherDecanterState.volume1 && volume2 == otherDecanterState.volume2;
        }

        public override int GetHashCode()
        {
            return volume1.GetHashCode() * 101 + volume2.GetHashCode();
        }
    }

#if DEAD_CODE
    public class DecanterAlgorithm : AStarBase<DecanterState>
    {
        private readonly int capacity1;
        private readonly int capacity2;

        public DecanterAlgorithm(int c1, int c2)
        {

            if (c1 <= 0 || c2 <= 0)
            {
                throw new ArgumentException("Both capacities must be greater than zero.");
            }

            capacity1 = c1;
            capacity2 = c2;
        }

        protected override void GenerateSuccessorStates(DecanterState CurState, DecanterState StartState, DecanterState GoalState)
        {

        	for( int i = 0; i < 6; i++ )
	        {
		        int nVolume1 = CurState.volume1;
		        int nVolume2 = CurState.volume2;
		        int nTransfer = 0;
		        string StepDescription = string.Empty;

        		switch (i)
		        {
			        case 0:		// Empty jug 1.
				        nVolume1 = 0;
				        StepDescription = "Empty jug 1.";
				        break;

			        case 1:		// Empty jug 2.
				        nVolume2 = 0;
				        StepDescription = "Empty jug 2.";
				        break;

			        case 2:		// Fill jug 1.
				        nVolume1 = capacity1;
				        StepDescription = "Fill jug 1.";
				        break;

			        case 3:		// Fill jug 2.
				        nVolume2 = capacity2;
				        StepDescription = "Fill jug 2.";
				        break;

			        case 4:		// Pour jug 1 into jug 2.
				        nTransfer = Math.Min(nVolume1, capacity2 - nVolume2);
				        nVolume1 -= nTransfer;
				        nVolume2 += nTransfer;
				        StepDescription = "Pour jug 1 into jug 2.";
				        break;

			        case 5:		// Pour jug 2 into jug 1.
				        nTransfer = Math.Min(nVolume2, capacity1 - nVolume1);
				        nVolume2 -= nTransfer;
				        nVolume1 += nTransfer;
				        StepDescription = "Pour jug 2 into jug 1.";
				        break;

			        default:
                        throw new Exception("DecanterAlgorithm.GenerateSuccessorStates() : Internal error");
		        }

                StepDescription = StepDescription + string.Format(" ({0}, {1})", nVolume1, nVolume2);

                DecanterState NewState = new DecanterState(nVolume1, nVolume2, CurState, StepDescription, CurState.g + 1, 0);

		        //NewState.g = CurState.g + 1;	        // Actual cost from start state to new state.
		        //NewState.h = 0;							// Estimated cost from new state to goal state.
		        //NewState.f = NewState.g + NewState.h;

		        if (!m_OpenQueue.Contains(NewState) && !closedSet.Contains(NewState))
		        {
			        m_OpenQueue.Enqueue(NewState);
		        }
	        }
        }

        public DecanterState Run(int goalVolume1, int goalVolume2)
        {

            if (goalVolume1 < 0)
            {
                throw new InvalidAStarStateException("Goal state: Volume 1 must not be less than zero.");
            }
            else if (goalVolume1 > capacity1)
            {
                throw new InvalidAStarStateException("Goal state: Volume 1 must not be greater than the capacity of container 1.");
            }
            else if (goalVolume2 < 0)
            {
                throw new InvalidAStarStateException("Goal state: Volume 2 must not be less than zero.");
            }
            else if (goalVolume2 > capacity2)
            {
                throw new InvalidAStarStateException("Goal state: Volume 2 must not be greater than the capacity of container 2.");
            }

            DecanterState StartState = new DecanterState(0, 0);
            DecanterState GoalState = new DecanterState(goalVolume1, goalVolume2);

            return Search(StartState, GoalState);
        }

        public bool RunAndReport(int goalVolume1, int goalVolume2)
        {
            return Report(Run(goalVolume1, goalVolume2));
        }
    }
#endif

    public class DecanterSuccessorStateGenerator : ISuccessorStateGenerator<DecanterState>
    {
        private readonly int capacity1;
        private readonly int capacity2;

        public DecanterSuccessorStateGenerator(int capacity1, int capacity2)
        {
            this.capacity1 = capacity1;
            this.capacity2 = capacity2;
        }

        public void StateValidityTest(DecanterState state)
        {

            if (state.volume1 < 0)
            {
                throw new HeuristicSearchStateException("Volume 1 must not be less than zero.");
            }
            else if (state.volume1 > capacity1)
            {
                throw new HeuristicSearchStateException("Volume 1 must not be greater than the capacity of container 1.");
            }
            else if (state.volume2 < 0)
            {
                throw new HeuristicSearchStateException("Volume 2 must not be less than zero.");
            }
            else if (state.volume2 > capacity2)
            {
                throw new HeuristicSearchStateException("Volume 2 must not be greater than the capacity of container 2.");
            }
        }

        public IEnumerable<KeyValuePair<DecanterState, int>> GenerateSuccessorStates(DecanterState currentState, DecanterState startState, DecanterState goalState)
        {
            var result = new List<KeyValuePair<DecanterState, int>>();

            for (int i = 0; i < 6; i++)
            {
                int nVolume1 = currentState.volume1;
                int nVolume2 = currentState.volume2;
                int nTransfer = 0;
                string StepDescription = string.Empty;

                switch (i)
                {
                    case 0:		// Empty jug 1.
                        nVolume1 = 0;
                        StepDescription = "Empty jug 1.";
                        break;

                    case 1:		// Empty jug 2.
                        nVolume2 = 0;
                        StepDescription = "Empty jug 2.";
                        break;

                    case 2:		// Fill jug 1.
                        nVolume1 = capacity1;
                        StepDescription = "Fill jug 1.";
                        break;

                    case 3:		// Fill jug 2.
                        nVolume2 = capacity2;
                        StepDescription = "Fill jug 2.";
                        break;

                    case 4:		// Pour jug 1 into jug 2.
                        nTransfer = Math.Min(nVolume1, capacity2 - nVolume2);
                        nVolume1 -= nTransfer;
                        nVolume2 += nTransfer;
                        StepDescription = "Pour jug 1 into jug 2.";
                        break;

                    case 5:		// Pour jug 2 into jug 1.
                        nTransfer = Math.Min(nVolume2, capacity1 - nVolume1);
                        nVolume2 -= nTransfer;
                        nVolume1 += nTransfer;
                        StepDescription = "Pour jug 2 into jug 1.";
                        break;

                    default:
                        throw new Exception("DecanterAlgorithm.GenerateSuccessorStates() : Internal error");
                }

                StepDescription = StepDescription + string.Format(" ({0}, {1})", nVolume1, nVolume2);

                DecanterState NewState = new DecanterState(nVolume1, nVolume2, currentState, StepDescription, currentState.g + 1, 0);

                //NewState.g = CurState.g + 1;	            // Actual cost from start state to new state.
                //NewState.h = 0;							// Estimated cost from new state to goal state.
                //NewState.f = NewState.g + NewState.h;

                result.Add(new KeyValuePair<DecanterState, int>(NewState, 1));
            }

            return result;
        }
    }
}
