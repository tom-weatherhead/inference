using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;

namespace Inference.AStar
{
    // The cannibals and missionaries problem.
    // The goal is to transfer all of the cannibals and missionaries across a river, from west to east, using a two-person canoe.

    // If, at any time, on either side of the river there is at least one missionary and the cannibals outnumber the missionaries,
    // Then the cannibals eat the missionaries, and the state is invalid.

    public class CanMissState : AStarStateBase
    {
        public readonly bool canoeIsOnWestSide;
        public readonly int numCannibalsOnWestSide;
        public readonly int numMissionariesOnWestSide;
        public readonly int numCannibalsOnEastSide;
        public readonly int numMissionariesOnEastSide;

        public CanMissState(bool canoeWest, int cw, int mw, int ce, int me,
            CanMissState previousState, string newStep, int gParam, int hParam)
            : base(previousState, newStep, gParam, hParam)
        {
            canoeIsOnWestSide = canoeWest;
            numCannibalsOnWestSide = cw;
            numMissionariesOnWestSide = mw;
            numCannibalsOnEastSide = ce;
            numMissionariesOnEastSide = me;
        }

        public CanMissState(bool canoeWest, int cw, int mw, int ce, int me)
            : this(canoeWest, cw, mw, ce, me, null, null, 0, 0)
        {
        }

        public bool IsValid()
        {

            if (numCannibalsOnWestSide < 0 || numMissionariesOnWestSide < 0 ||
                numCannibalsOnEastSide < 0 || numMissionariesOnEastSide < 0)
            {
                return false;
            }

            if (numMissionariesOnWestSide > 0 && numCannibalsOnWestSide > numMissionariesOnWestSide)
            {
                return false;
            }

            if (numMissionariesOnEastSide > 0 && numCannibalsOnEastSide > numMissionariesOnEastSide)
            {
                return false;
            }

            return true;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            CanMissState otherState = obj as CanMissState;

            return otherState != null && canoeIsOnWestSide == otherState.canoeIsOnWestSide &&
                numCannibalsOnWestSide == otherState.numCannibalsOnWestSide &&
                numMissionariesOnWestSide == otherState.numMissionariesOnWestSide &&
                numCannibalsOnEastSide == otherState.numCannibalsOnEastSide &&
                numMissionariesOnEastSide == otherState.numMissionariesOnEastSide;
        }

        public override int GetHashCode()
        {
            return canoeIsOnWestSide.GetHashCode() +
                numCannibalsOnWestSide.GetHashCode() * 101 +
                numMissionariesOnWestSide.GetHashCode() * 103 +
                numCannibalsOnEastSide.GetHashCode() * 107 +
                numMissionariesOnEastSide.GetHashCode() * 109;
        }

        public static CanMissState CalculateStartState(int c, int m)
        {
            return new CanMissState(true, c, m, 0, 0);
        }

        public CanMissState CalculateGoalState()
        {
            return new CanMissState(false, 0, 0, numCannibalsOnWestSide + numCannibalsOnEastSide, numMissionariesOnWestSide + numMissionariesOnEastSide);
        }
    }

#if DEAD_CODE
    public class CanMissAlgorithm : AStarBase<CanMissState>
    {
        public CanMissAlgorithm()
        {
        }

        protected override void GenerateSuccessorStates(CanMissState CurState, CanMissState StartState, CanMissState GoalState)
        {

            for (int i = 0; i < 5; ++i)
            {
                int numCannibalsOnSide = (CurState.canoeIsOnWestSide) ? CurState.numCannibalsOnWestSide : CurState.numCannibalsOnEastSide;
                int numMissionariesOnSide = (CurState.canoeIsOnWestSide) ? CurState.numMissionariesOnWestSide : CurState.numMissionariesOnEastSide;
                int numCannibalsInCanoe = 0;
                int numMissionariesInCanoe = 0;
                string srcSide = CurState.canoeIsOnWestSide ? "West" : "East";
                string dstSide = CurState.canoeIsOnWestSide ? "East" : "West";
                string newStepSuffix = string.Format(" from {0} to {1}.", srcSide, dstSide);
                string newStep = null;

                switch (i)
                {
                    case 0: // The canoe contains one cannibal.

                        if (numCannibalsOnSide >= 1)
                        {
                            numCannibalsInCanoe = 1;
                            newStep = "One cannibal travels" + newStepSuffix;
                        }

                        break;

                    case 1: // The canoe contains one missionary.

                        if (numMissionariesOnSide >= 1)
                        {
                            numMissionariesInCanoe = 1;
                            newStep = "One missionary travels" + newStepSuffix;
                        }

                        break;

                    case 2: // The canoe contains two cannibals.

                        if (numCannibalsOnSide >= 2)
                        {
                            numCannibalsInCanoe = 2;
                            newStep = "Two cannibals travel" + newStepSuffix;
                        }

                        break;

                    case 3: // The canoe contains two missionaries.

                        if (numMissionariesOnSide >= 2)
                        {
                            numMissionariesInCanoe = 2;
                            newStep = "Two missionaries travel" + newStepSuffix;
                        }

                        break;

                    case 4: // The canoe contains one cannibal and one missionary.

                        if (numCannibalsOnSide >= 1 && numMissionariesOnSide >= 1)
                        {
                            numCannibalsInCanoe = 1;
                            numMissionariesInCanoe = 1;
                            newStep = "One cannibal and one missionary travel" + newStepSuffix;
                        }

                        break;

                    default:
                        throw new Exception("CanMissAlgorithm.GenerateSuccessorStates() : Internal error");
                }

                if (newStep == null)
                {
                    continue;
                }

                int new_cw;
                int new_mw;
                int new_ce;
                int new_me;

                if (CurState.canoeIsOnWestSide)
                {
                    new_cw = CurState.numCannibalsOnWestSide - numCannibalsInCanoe;
                    new_mw = CurState.numMissionariesOnWestSide - numMissionariesInCanoe;
                    new_ce = CurState.numCannibalsOnEastSide + numCannibalsInCanoe;
                    new_me = CurState.numMissionariesOnEastSide + numMissionariesInCanoe;
                }
                else
                {
                    new_cw = CurState.numCannibalsOnWestSide + numCannibalsInCanoe;
                    new_mw = CurState.numMissionariesOnWestSide + numMissionariesInCanoe;
                    new_ce = CurState.numCannibalsOnEastSide - numCannibalsInCanoe;
                    new_me = CurState.numMissionariesOnEastSide - numMissionariesInCanoe;
                }

                newStep = newStep + string.Format(" (W {0}, {1}; E {2}, {3})", new_cw, new_mw, new_ce, new_me);

                CanMissState newState = new CanMissState(!CurState.canoeIsOnWestSide, new_cw, new_mw, new_ce, new_me,
                    CurState, newStep, CurState.g + 1, new_cw + new_mw);

                if (newState.IsValid() && !m_OpenQueue.Contains(newState) && !closedSet.Contains(newState))
                {
                    m_OpenQueue.Enqueue(newState);
                }
            }
        }

        public CanMissState Run(int numCannibals, int numMissionaries)
        {

            if (numCannibals < 0)
            {
                throw new InvalidAStarStateException("Start state: The number of cannibals cannot be less than zero.");
            }
            else if (numMissionaries < 0)
            {
                throw new InvalidAStarStateException("Start state: The number of missionaries cannot be less than zero.");
            }
            else if (numCannibals > numMissionaries && numMissionaries > 0)
            {
                throw new InvalidAStarStateException("Start state: The number of cannibals cannot exceed the number of missionaries if there are one or more missionaries.");
            }

            CanMissState StartState = new CanMissState(true, numCannibals, numMissionaries, 0, 0);
            CanMissState GoalState = new CanMissState(false, 0, 0, numCannibals, numMissionaries);

            return Search(StartState, GoalState);
        }

        public bool RunAndReport(int numCannibals, int numMissionaries)
        {
            return Report(Run(numCannibals, numMissionaries));
        }
    }
#endif

    public class CanMissSuccessorStateGenerator : ISuccessorStateGenerator<CanMissState>
    {
        public CanMissSuccessorStateGenerator()
        {
        }

        public void StateValidityTest(CanMissState state)
        {

            if (state == null || !state.IsValid())
            {
                throw new HeuristicSearchStateException("The state is invalid.");
            }
        }

        public IEnumerable<KeyValuePair<CanMissState, int>> GenerateSuccessorStates(CanMissState currentState, CanMissState startState, CanMissState goalState)
        {
            var result = new List<KeyValuePair<CanMissState, int>>();

            for (int i = 0; i < 5; ++i)
            {
                int numCannibalsOnSide = (currentState.canoeIsOnWestSide) ? currentState.numCannibalsOnWestSide : currentState.numCannibalsOnEastSide;
                int numMissionariesOnSide = (currentState.canoeIsOnWestSide) ? currentState.numMissionariesOnWestSide : currentState.numMissionariesOnEastSide;
                int numCannibalsInCanoe = 0;
                int numMissionariesInCanoe = 0;
                string srcSide = currentState.canoeIsOnWestSide ? "West" : "East";
                string dstSide = currentState.canoeIsOnWestSide ? "East" : "West";
                string newStepSuffix = string.Format(" from {0} to {1}.", srcSide, dstSide);
                string newStep = null;

                switch (i)
                {
                    case 0: // The canoe contains one cannibal.

                        if (numCannibalsOnSide >= 1)
                        {
                            numCannibalsInCanoe = 1;
                            newStep = "One cannibal travels" + newStepSuffix;
                        }

                        break;

                    case 1: // The canoe contains one missionary.

                        if (numMissionariesOnSide >= 1)
                        {
                            numMissionariesInCanoe = 1;
                            newStep = "One missionary travels" + newStepSuffix;
                        }

                        break;

                    case 2: // The canoe contains two cannibals.

                        if (numCannibalsOnSide >= 2)
                        {
                            numCannibalsInCanoe = 2;
                            newStep = "Two cannibals travel" + newStepSuffix;
                        }

                        break;

                    case 3: // The canoe contains two missionaries.

                        if (numMissionariesOnSide >= 2)
                        {
                            numMissionariesInCanoe = 2;
                            newStep = "Two missionaries travel" + newStepSuffix;
                        }

                        break;

                    case 4: // The canoe contains one cannibal and one missionary.

                        if (numCannibalsOnSide >= 1 && numMissionariesOnSide >= 1)
                        {
                            numCannibalsInCanoe = 1;
                            numMissionariesInCanoe = 1;
                            newStep = "One cannibal and one missionary travel" + newStepSuffix;
                        }

                        break;

                    default:
                        throw new Exception("CanMissAlgorithm.GenerateSuccessorStates() : Internal error");
                }

                if (newStep == null)
                {
                    continue;
                }

                int new_cw;
                int new_mw;
                int new_ce;
                int new_me;

                if (currentState.canoeIsOnWestSide)
                {
                    new_cw = currentState.numCannibalsOnWestSide - numCannibalsInCanoe;
                    new_mw = currentState.numMissionariesOnWestSide - numMissionariesInCanoe;
                    new_ce = currentState.numCannibalsOnEastSide + numCannibalsInCanoe;
                    new_me = currentState.numMissionariesOnEastSide + numMissionariesInCanoe;
                }
                else
                {
                    new_cw = currentState.numCannibalsOnWestSide + numCannibalsInCanoe;
                    new_mw = currentState.numMissionariesOnWestSide + numMissionariesInCanoe;
                    new_ce = currentState.numCannibalsOnEastSide - numCannibalsInCanoe;
                    new_me = currentState.numMissionariesOnEastSide - numMissionariesInCanoe;
                }

                newStep = newStep + string.Format(" (W {0}, {1}; E {2}, {3})", new_cw, new_mw, new_ce, new_me);

                var newState = new CanMissState(!currentState.canoeIsOnWestSide, new_cw, new_mw, new_ce, new_me,
                    currentState, newStep, currentState.g + 1, new_cw + new_mw);

                if (newState.IsValid())
                {
                    result.Add(new KeyValuePair<CanMissState, int>(newState, 1));
                }
            }

            return result;
        }
    }
}
