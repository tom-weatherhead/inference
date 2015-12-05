using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Inference.Parser
{
    public class SLR1Parser : LR0Parser
    {
        public SLR1Parser(IGrammar g)
            : base(g)
        {
        }

        public SLR1Parser(GrammarSelector gs)
            : this(GrammarFactory.Create(gs))
        {
        }

        // Adapted from Fischer and LeBlanc, pages 158-159.

        private ShiftReduceAction GetAction(CFSMState S, Symbol tokenAsSymbol, out int reduceProductionNum)
        {
            ShiftReduceAction result = ShiftReduceAction.Error;
            bool reduceResultFound = false;   // In order for the grammar to be SLR(1), there must be at most one result per state-symbol pair.

            reduceProductionNum = -1;

            // 1) Search for Reduce actions.

            foreach (LR0Configuration c in S.ConfigurationSet)
            {
                Production matchedProduction = c.ConvertToProductionIfAllMatched();

                if (matchedProduction == null)
                {
                    continue;
                }

                HashSet<Symbol> currentFollowSet = null;

                for (int i = 0; i < grammar.Productions.Count; ++i)
                {
                    Production productionToCompare = grammar.Productions[i].StripOutSemanticActions();

                    if (matchedProduction.Equals(productionToCompare))
                    {
                        // Is tokenAsSymbol in Follow(productionToCompare.lhs) ?

                        if (currentFollowSet == null)
                        {
                            currentFollowSet = follow_set[matchedProduction.lhs];
                        }

                        if (currentFollowSet.Contains(tokenAsSymbol))
                        {

                            if (reduceResultFound && reduceProductionNum != i)
                            {
                                //throw new ReduceReduceConflictException("GetAction() : Multiple actions found; grammar is not SLR(1).");
                                // 2013/10/22 : To find out why my CLU grammar is not SLR(1):
                                throw new ReduceReduceConflictException(string.Format(
                                    "GetAction() : Multiple actions found; grammar is not SLR(1).  Symbol {0}, productions {1} and {2}.",
                                    //tokenAsSymbol, reduceProductionNum, i));
                                    tokenAsSymbol, grammar.Productions[reduceProductionNum].ToString(), grammar.Productions[i].ToString())); // The .ToString() here may be unnecessary.
                            }

                            result = ShiftReduceAction.Reduce;
                            reduceProductionNum = i;
                            reduceResultFound = true;
                        }
                    }
                }
            }

            // 2) Search for Shift and Accept actions.
            /*
            bool shiftOrAcceptResultFound = false;

            foreach (LR0Configuration c in S.ConfigurationSet)
            {
                Symbol symbol;

                if (c.FindSymbolAfterDot(out symbol) && symbol == tokenAsSymbol)
                {
                    shiftOrAcceptResultFound = true;
                }
            }
             */
            Symbol symbol;
            bool shiftOrAcceptResultFound = S.ConfigurationSet.Any(c => c.FindSymbolAfterDot(out symbol) && symbol == tokenAsSymbol);

            if (shiftOrAcceptResultFound)
            {

                if (reduceResultFound)
                {
                    throw new ShiftReduceConflictException(string.Format(
                        "GetAction() : Multiple actions found; grammar is not SLR(1).  Symbol {0}, production {1}.",
                        tokenAsSymbol, grammar.Productions[reduceProductionNum].ToString())); // The .ToString() here may be unnecessary.
                }

                result = (tokenAsSymbol == Symbol.T_EOF) ? ShiftReduceAction.Accept : ShiftReduceAction.Shift;
            }

            /*
            // Test:

            if (result == ShiftReduceAction.Error)
            {
                StringBuilder sb = new StringBuilder();
                string separator = string.Empty;

                foreach (Symbol transitionSymbol in S.Transitions.Keys)
                {
                    sb.Append(separator);
                    sb.Append(transitionSymbol.ToString());
                    separator = ", ";
                }

                throw new Exception(string.Format("GetAction() error: symbol = {0}; transition keys = {1}", tokenAsSymbol, sb.ToString()));
            }
             */

            return result;
        }

        protected override ShiftReduceAction GetActionCaller(CFSMState S, Symbol tokenAsSymbol, out int reduceProductionNum)
        {
            return GetAction(S, tokenAsSymbol, out reduceProductionNum);
        }
    }
}
