#define MANHATTAN // This actually finds more expensive solutions for Test2 and Test2_ID.

using System;
using System.Collections.Generic;
using System.Text;
using Inference.Utilities;

namespace Inference.AStar
{
    #region EightPuzzleState

    public class EightPuzzleState : AStarStateBase
    {
        public readonly List<int> tiles;

        public EightPuzzleState(List<int> t, EightPuzzleState previousState, string newStep, int gParam, int hParam)
            : base(previousState, newStep, gParam, hParam)
        {

            if (t == null)
            {
                throw new ArgumentNullException("t", "EightPuzzleState constructor: t is null.");
            }
            else if (t.Count != 9)
            {
                throw new ArgumentException(string.Format("t.Count is {0} rather than 9.", t.Count), "t");
            }

            for (int i = 0; i < 9; ++i)
            {

                if (!t.Contains(i))
                {
                    throw new ArgumentException(string.Format("t does not contain {0}.", i), "t");
                }
            }

            tiles = t;  // We will consider this List to be immutable after this point.
        }

        public EightPuzzleState(List<int> t)
            : this(t, null, null, 0, 0)
        {
        }

        public static EightPuzzleState CreateRandom()
        {
            return new EightPuzzleState(0.To(8).Shuffle());
        }

        public static EightPuzzleState CreateGoal()
        {
            return new EightPuzzleState(new List<int>() { 1, 2, 3, 8, 0, 4, 7, 6, 5 });
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            EightPuzzleState that = obj as EightPuzzleState;

            if (that == null || tiles.Count != that.tiles.Count)
            {
                return false;
            }

            for (int i = 0; i < tiles.Count; ++i)
            {

                if (tiles[i] != that.tiles[i])
                {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode()
        {
            int hashCode = 0;

            foreach (int i in tiles)
            {
                hashCode *= 9;
                hashCode += i;
            }

            return hashCode;
        }
    }

    #endregion

    public class EightPuzzleSuccessorStateGenerator : ISuccessorStateGenerator<EightPuzzleState>
    {
#if MANHATTAN
        private static readonly List<int> ManA = new List<int>() { 0, 0, 1, 2, 3, 4, 3, 2, 1 };
        private static readonly List<int> ManB = new List<int>() { 0, 1, 0, 1, 2, 3, 2, 3, 2 };
        private static readonly List<int> ManC = new List<int>() { 0, 2, 1, 0, 1, 2, 3, 4, 3 };
        private static readonly List<int> ManD = new List<int>() { 0, 1, 2, 3, 2, 3, 2, 1, 0 };
        private static readonly List<int> ManE = new List<int>() { 0, 2, 1, 2, 1, 2, 1, 2, 1 };
        private static readonly List<int> ManF = new List<int>() { 0, 3, 2, 1, 0, 1, 2, 3, 2 };
        private static readonly List<int> ManG = new List<int>() { 0, 2, 3, 4, 3, 2, 1, 0, 1 };
        private static readonly List<int> ManH = new List<int>() { 0, 3, 2, 3, 2, 1, 0, 1, 2 };
        private static readonly List<int> ManI = new List<int>() { 0, 4, 3, 2, 1, 0, 1, 2, 3 };
        private static readonly List<List<int>> Man = new List<List<int>>() { ManA, ManB, ManC, ManD, ManE, ManF, ManG, ManH, ManI };
        private static readonly List<int> NextInCycle = new List<int>() { 1, 2, 5, 0, 4, 8, 3, 6, 7 };
#endif

        public EightPuzzleSuccessorStateGenerator()
        {
        }

        public void StateValidityTest(EightPuzzleState state)
        {
            // The state validity checks are in the EightPuzzleState constructor.
        }

#if MANHATTAN
        // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/5_2.html
        // See http://www.improvedoutcomes.com/docs/WebSiteDocs/Clustering/Clustering_Parameters/Manhattan_Distance_Metric.htm

        private int ManhattanDistanceHeuristic(List<int> tiles)
        {
            var sum = 0;

            for (int i = 0; i < 9; ++i)
            {
                sum += Man[i][tiles[i]];
            }

            return sum;
        }

        private int OutOfCycleHeuristic(List<int> tiles)
        {
            var sum = 0;

            for (int i = 0; i < 9; ++i)
            {

                if (i == 4)
                {
                    continue;
                }

                var tile = tiles[i];
                var nextTileInCycle = tiles[NextInCycle[i]];

                if (tile > 0 && nextTileInCycle == tile + 1)
                {
                }
                else if (tile == 8 && nextTileInCycle == 1)
                {
                }
                else
                {
                    sum += 2;
                }
            }

            if (tiles[4] != 0)
            {
                ++sum;
            }

            return sum;
        }
#endif

        public IEnumerable<KeyValuePair<EightPuzzleState, int>> GenerateSuccessorStates(EightPuzzleState currentState, EightPuzzleState startState, EightPuzzleState goalState)
        {
            var result = new List<KeyValuePair<EightPuzzleState, int>>();

            // Calculate the row and column of the zero tile.
            int indexOfZero = currentState.tiles.IndexOf(0);

            if (indexOfZero < 0 || indexOfZero >= 9)
            {
                throw new Exception(string.Format("EightPuzzleSuccessorStateGenerator.GenerateSuccessorStates() : indexOfZero is {0}.", indexOfZero));
            }

            int rowOfZero = indexOfZero / 3;
            int columnOfZero = indexOfZero % 3;

            for (int i = 0; i < 8; ++i)
            {
                var newTiles = new List<int>(currentState.tiles);
                string newStep = null;
                int cost = 0;

                switch (i)
                {
                    case 0: // Try to slide one tile up.

                        if (rowOfZero < 2)
                        {
                            newTiles[indexOfZero] = newTiles[indexOfZero + 3];
                            newTiles[indexOfZero + 3] = 0;
                            newStep = "Slide one tile up.";
                            cost = 1;
                        }

                        break;

                    case 1: // Try to slide two tiles up.

                        if (rowOfZero == 0)
                        {
                            newTiles[indexOfZero] = newTiles[indexOfZero + 3];
                            newTiles[indexOfZero + 3] = newTiles[indexOfZero + 6];
                            newTiles[indexOfZero + 6] = 0;
                            newStep = "Slide two tiles up.";
                            cost = 2;
                        }

                        break;

                    case 2: // Try to slide one tile down.

                        if (rowOfZero > 0)
                        {
                            newTiles[indexOfZero] = newTiles[indexOfZero - 3];
                            newTiles[indexOfZero - 3] = 0;
                            newStep = "Slide one tile down.";
                            cost = 1;
                        }

                        break;

                    case 3: // Try to slide two tiles down.

                        if (rowOfZero == 2)
                        {
                            newTiles[indexOfZero] = newTiles[indexOfZero - 3];
                            newTiles[indexOfZero - 3] = newTiles[indexOfZero - 6];
                            newTiles[indexOfZero - 6] = 0;
                            newStep = "Slide two tiles down.";
                            cost = 2;
                        }

                        break;

                    case 4: // Try to slide one tile to the left.

                        if (columnOfZero < 2)
                        {
                            newTiles[indexOfZero] = newTiles[indexOfZero + 1];
                            newTiles[indexOfZero + 1] = 0;
                            newStep = "Slide one tile to the left.";
                            cost = 1;
                        }

                        break;

                    case 5: // Try to slide two tiles to the left.

                        if (columnOfZero == 0)
                        {
                            newTiles[indexOfZero] = newTiles[indexOfZero + 1];
                            newTiles[indexOfZero + 1] = newTiles[indexOfZero + 2];
                            newTiles[indexOfZero + 2] = 0;
                            newStep = "Slide two tiles to the left.";
                            cost = 2;
                        }

                        break;

                    case 6: // Try to slide one tile to the right.

                        if (columnOfZero > 0)
                        {
                            newTiles[indexOfZero] = newTiles[indexOfZero - 1];
                            newTiles[indexOfZero - 1] = 0;
                            newStep = "Slide one tile to the right.";
                            cost = 1;
                        }

                        break;

                    case 7: // Try to slide two tiles to the right.

                        if (columnOfZero == 2)
                        {
                            newTiles[indexOfZero] = newTiles[indexOfZero - 1];
                            newTiles[indexOfZero - 1] = newTiles[indexOfZero - 2];
                            newTiles[indexOfZero - 2] = 0;
                            newStep = "Slide two tiles to the right.";
                            cost = 2;
                        }

                        break;

                    default:
                        break;
                }

                if (newStep == null)
                {
                    continue;
                }

                newStep = string.Format("{0} ({1})", newStep, string.Join(", ", newTiles));

#if MANHATTAN
                // Cost multiplier      Steps in sol'n to Test2     Steps in sol'n to Test2_ID
                // 1                    19 (best is 16)             29 (best is 16)
                // 2                    20                          19
                // 3                    16 (matches best sol'n)     16 (matches best sol'n)
                // 4                    16 (matches best sol'n)     16 (matches best sol'n)
                // 5                    16 (matches best sol'n)     16 (matches best sol'n)
                // 10                   16 (matches best sol'n)     16 (matches best sol'n)
                cost *= 3;

                int h = ManhattanDistanceHeuristic(newTiles) + 3 * OutOfCycleHeuristic(newTiles);
#else
                int h = 0;

                for (int j = 0; j < 9; ++j)
                {

                    if (newTiles[j] != 0 && newTiles[j] != goalState.tiles[j])
                    {
                        ++h;
                    }
                }
#endif

                EightPuzzleState newState = new EightPuzzleState(newTiles, currentState, newStep, currentState.g + cost, h);

                result.Add(new KeyValuePair<EightPuzzleState, int>(newState, cost));
            }

            return result;
        }
    }
}
