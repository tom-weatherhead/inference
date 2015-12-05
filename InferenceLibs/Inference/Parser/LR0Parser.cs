using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Inference.Parser
{
    #region LR0Configuration

    public class LR0Configuration
    {
        public readonly Symbol ProductionLHS;
        public readonly List<Symbol> ProductionRHS = new List<Symbol>();    // Will contain exactly one instance of the symbol Dot.

        public LR0Configuration(Symbol lhs)
        {
            ProductionLHS = lhs;
        }

        public LR0Configuration(Symbol lhs, List<Symbol> rhs)
        {
            ProductionLHS = lhs;
            rhs.ForEach(symbol => ProductionRHS.Add(symbol));
        }

        public LR0Configuration(LR0Configuration src)   // Copy constructor.
            : this(src.ProductionLHS, src.ProductionRHS)
        {
        }

        public LR0Configuration(Production p)
        {
            ProductionLHS = p.lhs;
            ProductionRHS.Add(Symbol.Dot);
            ProductionRHS.AddRange(p.RHSWithNoSemanticActions());
        }

        public override string ToString()
        {
            /*
            var sb = new StringBuilder();

            sb.Append(lhs.ToString() + " ->");

            foreach (object o in rhs)
            {
                sb.Append(" " + o.ToString());
            }

            return sb.ToString();
             */
            return string.Format("{0} -> {1}", ProductionLHS, string.Join(" ", ProductionRHS));
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var that = obj as LR0Configuration;

            if (that == null || ProductionLHS != that.ProductionLHS || ProductionRHS.Count != that.ProductionRHS.Count)
            {
                return false;
            }

            for (int i = 0; i < ProductionRHS.Count; ++i)
            {

                if (ProductionRHS[i] != that.ProductionRHS[i])
                {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode()
        {
            /*
            int hashCode = ProductionLHS.GetHashCode();

            foreach (Symbol symbol in ProductionRHS)
            {
                hashCode *= 101;
                hashCode += symbol.GetHashCode();
            }

            return hashCode;
             */
            return ProductionRHS
                .Select(symbol => symbol.GetHashCode())
                .Aggregate(ProductionLHS.GetHashCode(), (accumulator, hashCode) => accumulator * 101 + hashCode);
        }

        public int FindDot()
        {
            return ProductionRHS.FindIndex(symbol => symbol == Symbol.Dot);
            /*
            for (int i = 0; i < ProductionRHS.Count; ++i)
            {

                if (ProductionRHS[i] == Symbol.Dot)
                {
                    return i;
                }
            }

            return -1;
             */
        }

        public bool FindSymbolAfterDot(out Symbol symbol)
        {
            var i = FindDot();

            if (i >= 0 && i < ProductionRHS.Count - 1)
            {
                symbol = ProductionRHS[i + 1];
                return true;
            }

            symbol = Symbol.Lambda;
            return false;
        }

        public List<Symbol> FindSuffix(int numSymbolsToSkipAfterDot)
        {
            var i = FindDot();

            if (i < 0)
            {
                return null;
            }

            /*
            List<Symbol> rho = new List<Symbol>();

            i += numSymbolsToSkipAfterDot + 1;

            while (i < ProductionRHS.Count)
            {
                rho.Add(ProductionRHS[i++]);
            }

            return rho;
             */
            return ProductionRHS.Skip(i + numSymbolsToSkipAfterDot + 1).ToList();
        }

        public LR0Configuration AdvanceDot()
        {
            var dotIndex = FindDot();

            if (dotIndex < 0)
            {
                throw new InternalErrorException("LR0Configuration.AdvanceDot() : No dot found.");
            }

            var newRHS = ProductionRHS.Where(symbol => symbol != Symbol.Dot).ToList();
            var newConf = new LR0Configuration(ProductionLHS, newRHS);

            if (dotIndex >= ProductionRHS.Count - 1)
            {
                throw new InternalErrorException("LR0Configuration.AdvanceDot() : The dot cannot be advanced any further.");
            }

            newConf.ProductionRHS.Insert(dotIndex + 1, Symbol.Dot);
            return newConf;
        }

        public Production ConvertToProductionIfAllMatched()
        {
            var dotIndex = FindDot();

            if (ProductionRHS.Count == 2 && dotIndex == 0 && ProductionRHS[1] == Symbol.Lambda) // A necessary hack.
            {
                return new Production(ProductionLHS, new List<object>() { Symbol.Lambda }, 0);
            }

            if (dotIndex != ProductionRHS.Count - 1)
            {
                return null;
            }

            var rhs = new List<object>();

            // .ForEach(symbol => rhs.Add(symbol)) is used because rhs is of type List<object>, not List<Symbol> .
            ProductionRHS.Where(symbol => symbol != Symbol.Dot).ToList().ForEach(symbol => rhs.Add(symbol));

            return new Production(ProductionLHS, rhs);
        }
    }

    #endregion

    #region CFSMState

    public class CFSMState
    {
        public readonly HashSet<LR0Configuration> ConfigurationSet;
        public readonly Dictionary<Symbol, CFSMState> Transitions = new Dictionary<Symbol, CFSMState>();

        public CFSMState(HashSet<LR0Configuration> cs)
        {
            ConfigurationSet = cs;
        }

        public override bool Equals(object obj)
        {
            // TODO: Find a better implementation for this function.  Beware of cycles in the finite state machine (or ignore the transitions in this function).

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var that = obj as CFSMState;

            // TODO: Should we also consider Transitions.Keys?
            return that != null && ConfigurationSet.IsSubsetOf(that.ConfigurationSet) && that.ConfigurationSet.IsSubsetOf(ConfigurationSet);
        }

        public override int GetHashCode()
        {
            // TODO: Find a better implementation for this function.  Beware of cycles in the finite state machine (or ignore the transitions in this function).
            //int hashCode = 0;

            /*
            foreach (LR0Configuration conf in ConfigurationSet)
            {
                // The order of the configurations in the set doesn't affect the hash code.
                hashCode += conf.GetHashCode();
            }
             */
            //ConfigurationSet.ToList().ForEach(conf => hashCode += conf.GetHashCode());
            //return ConfigurationSet.Select(conf => conf.GetHashCode()).Sum();

            // "unchecked" suppresses the OverflowException.  Sum() always executes within a checked block, and may throw the exception.
            //return unchecked(ConfigurationSet.Select(conf => conf.GetHashCode()).Aggregate((accumulator, element) => accumulator + element));

            // The order of the configurations in the set doesn't affect the hash code.
            // Passing a seed of 0 to Aggregate() avoids a possible "sequence contains no elements" error.
            return ConfigurationSet.Select(conf => conf.GetHashCode()).Aggregate(0, (accumulator, element) => accumulator + element);

            // TODO: Should we also consider Transitions.Keys?
            //return hashCode;
        }
    }

    #endregion

    #region CharacteristicFiniteStateMachine

    public class CharacteristicFiniteStateMachine
    {
        public readonly List<CFSMState> StateList = new List<CFSMState>();
        public readonly CFSMState StartState;
        public readonly CFSMState ErrorState;

        public CharacteristicFiniteStateMachine(CFSMState ss)
        {
            StartState = ss;
            ErrorState = new CFSMState(new HashSet<LR0Configuration>());
            StateList.Add(StartState);
            StateList.Add(ErrorState);
        }

        public CFSMState FindStateWithLabel(HashSet<LR0Configuration> cs)
        {
            // Returns null if no state has the given configuration set.
            return StateList.Find(state => cs.IsSubsetOf(state.ConfigurationSet) && state.ConfigurationSet.IsSubsetOf(cs));
        }
    }

    #endregion

    #region CFSMStateSymbolPair

    class CFSMStateSymbolPair
    {
        public readonly CFSMState state;
        public readonly Symbol symbol;

        public CFSMStateSymbolPair(CFSMState st, Symbol sy)
        {
            state = st;
            symbol = sy;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var that = obj as CFSMStateSymbolPair;

            return that != null && state.Equals(that.state) && symbol == that.symbol;
        }

        public override int GetHashCode()
        {
            return state.GetHashCode() * 101 + symbol.GetHashCode();
        }
    }

    #endregion

    #region LR0Parser

    public class LR0Parser : ParserBase
    {
        private readonly HashSet<Symbol> AllSymbols;
        protected readonly CharacteristicFiniteStateMachine machine;
        private readonly Dictionary<CFSMStateSymbolPair, CFSMState> GoToTable = new Dictionary<CFSMStateSymbolPair, CFSMState>();
        private readonly Production startingProduction;

        public LR0Parser(IGrammar g)
            : base(g)
        {
            AllSymbols = new HashSet<Symbol>(g.Terminals);
            AllSymbols.UnionWith(g.NonTerminals);
            machine = build_CFSM();
            build_go_to_table();
            startingProduction = g.FindStartingProduction();    // No need to .StripOutSemanticActions(); they have already been removed.
        }

        public LR0Parser(GrammarSelector gs)
            : this(GrammarFactory.Create(gs))
        {
        }

        public int NumberOfStates
        {
            get
            {
                return machine.StateList.Count;
            }
        }

        // Adapted from Fischer and LeBlanc, page 146.

        private HashSet<LR0Configuration> closure0(HashSet<LR0Configuration> s)
        {
            var sPrime = new HashSet<LR0Configuration>(s);
            var additions = new HashSet<LR0Configuration>();

            do
            {
                additions.Clear();

                foreach (var conf1 in sPrime)
                {
                    Symbol A;

                    if (!conf1.FindSymbolAfterDot(out A) || !grammar.NonTerminals.Contains(A))
                    {
                        continue;
                    }

                    foreach (var p in grammar.Productions)
                    {

                        if (p.lhs != A)
                        {
                            continue;
                        }

                        var addition = new LR0Configuration(p);

                        if (!sPrime.Contains(addition) && !additions.Contains(addition))
                        {
                            additions.Add(addition);
                        }
                    }
                }

                sPrime.UnionWith(additions);
            }
            while (additions.Count > 0);

            return sPrime;
        }

        // Adapted from Fischer and LeBlanc, page 147.

        private HashSet<LR0Configuration> go_to0(HashSet<LR0Configuration> s, Symbol X)
        {
            var sb = new HashSet<LR0Configuration>();

            foreach (var c in s)
            {
                Symbol symbol;

                if (!c.FindSymbolAfterDot(out symbol) || symbol != X)
                {
                    continue;
                }

                sb.Add(c.AdvanceDot());
            }

            return closure0(sb);
        }

        // See Fischer and LeBlanc, page 147.

        private HashSet<LR0Configuration> compute_s0()
        {
            var p = grammar.FindStartingProduction();

            return closure0(new HashSet<LR0Configuration>() { new LR0Configuration(p) });
        }

        // Adapted from Fischer and LeBlanc, page 148.

        private CharacteristicFiniteStateMachine build_CFSM()
        {
            var s0 = compute_s0();
            var startState = new CFSMState(s0);
            var cfsm = new CharacteristicFiniteStateMachine(startState);
            var S = new Stack<HashSet<LR0Configuration>>();

            S.Push(s0);

            while (S.Count > 0)
            {
                var s = S.Pop();

                // Consider both terminals and non-terminals.

                foreach (var X in AllSymbols)
                {
                    var g = go_to0(s, X);

                    /*
                    if (g.Count == 0)
                    {
                        continue;
                    }
                     */

                    var stateG = cfsm.FindStateWithLabel(g);

                    if (stateG == null)
                    {
                        stateG = new CFSMState(g);
                        cfsm.StateList.Add(stateG);
                        S.Push(g);
                    }

                    // Create a transition under X from the state s labels to the state g labels.
                    var stateS = cfsm.FindStateWithLabel(s);

                    if (stateS.Transitions.ContainsKey(X))
                    {
                        throw new InternalErrorException("LR0Parser.build_CFSM() : Finite state machine transition is being overwritten.");
                    }

                    stateS.Transitions[X] = stateG;
                }
            }

            return cfsm;
        }

        // Adapted from Fischer and LeBlanc, pages 150-151.

        private ShiftReduceAction GetAction(CFSMState S, out int reduceProductionNum)
        {
            var result = ShiftReduceAction.Error;
            var reduceOrAcceptResultFound = false;   // In order for the grammar to be LR(0), there must be at most one result per state-symbol pair.

            reduceProductionNum = -1;

            // 1) Search for Reduce actions.

            foreach (var c in S.ConfigurationSet)
            {
                var matchedProduction = c.ConvertToProductionIfAllMatched();

                if (matchedProduction == null)
                {
                    continue;
                }

                for (var i = 0; i < grammar.Productions.Count; ++i)
                {
                    var productionToCompare = grammar.Productions[i].StripOutSemanticActions();

                    if (matchedProduction.Equals(productionToCompare))
                    {

                        if (reduceOrAcceptResultFound && reduceProductionNum != i)
                        {
                            throw new ReduceReduceConflictException("GetAction() : Multiple actions found; grammar is not LR(0).");
                        }

                        result = matchedProduction.Equals(startingProduction) ? ShiftReduceAction.Accept : ShiftReduceAction.Reduce;

                        reduceProductionNum = i;
                        reduceOrAcceptResultFound = true;
                    }
                }
            }

            // 2) Search for Shift and Accept actions.
            /*
            bool shiftResultFound = false;

            foreach (LR0Configuration c in S.ConfigurationSet)
            {
                Symbol symbol;

                if (c.FindSymbolAfterDot(out symbol) && grammar.Terminals.Contains(symbol))
                {
                    shiftResultFound = true;
                }
            }
             */
            Symbol symbol;
            var shiftResultFound = S.ConfigurationSet.Any(c => c.FindSymbolAfterDot(out symbol) && grammar.Terminals.Contains(symbol));

            if (shiftResultFound)
            {

                if (reduceOrAcceptResultFound)
                {
                    throw new ShiftReduceConflictException("GetAction() : Multiple actions found; grammar is not LR(0).");
                }

                result = ShiftReduceAction.Shift;
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

                throw new Exception(string.Format("GetAction() error: transition keys = {0}", sb.ToString()));
            }
             */

            return result;
        }

        protected virtual ShiftReduceAction GetActionCaller(CFSMState S, Symbol tokenAsSymbol, out int reduceProductionNum)
        {
            return GetAction(S, out reduceProductionNum);
        }

        // Adapted from Fischer and LeBlanc, page 150.

        private void build_go_to_table()
        {
            GoToTable.Clear();

            foreach (var S in machine.StateList)
            {

                foreach (var X in S.Transitions.Keys)
                {
                    GoToTable[new CFSMStateSymbolPair(S, X)] = S.Transitions[X];
                }
            }
        }

        public CFSMState go_to(CFSMState S, Symbol tokenAsSymbol)
        {
            var pair = new CFSMStateSymbolPair(S, tokenAsSymbol);

            if (!GoToTable.ContainsKey(pair))
            {
                throw new InternalErrorException(string.Format("go_to() failed on token {0}", tokenAsSymbol));
            }

            return GoToTable[pair];
        }

        // Adapted from Fischer and LeBlanc, page 142.

        private object shift_reduce_driver(List<Token> tokenList, bool parse)
        {

            if (tokenList.Count == 0)
            {
                throw new SyntaxException("Token list is empty");
            }

            var tokenNum = 0;
            var tokenAsSymbol = grammar.TokenToSymbol(tokenList[tokenNum]);
            var parseStack = new Stack<CFSMState>();  // The parse stack, which contains CFSM states.
            var semanticStack = new Stack<object>();

            parseStack.Push(machine.StartState);

            while (parseStack.Count > 0)
            {
                var S = parseStack.Peek();
                int reduceProductionNum;
                var action = GetActionCaller(S, tokenAsSymbol, out reduceProductionNum);

                switch (action)
                {
                    case ShiftReduceAction.Accept:

                        if (!parse)
                        {
                            return null;
                        }

                        var semanticStackSize = semanticStack.Count;

                        if (semanticStackSize != 1)
                        {
                            /*
                            Console.WriteLine("Beginning of semantic stack dump:");

                            while (semanticStack.Count > 0)
                            {
                                object o = semanticStack.Pop();

                                if (o == null)
                                {
                                    Console.WriteLine("  null");
                                }
                                else
                                {
                                    Console.WriteLine("  {0}: {1}", o.GetType().FullName, o.ToString());
                                }
                            }

                            Console.WriteLine("End of semantic stack dump.");
                             */
                            throw new GrammarException(string.Format("There were {0} objects on the semantic stack; expected exactly one", semanticStackSize));
                        }

                        return semanticStack.Pop();

                    case ShiftReduceAction.Shift:
                        //Console.WriteLine("Shift: tokenAsSymbol is {0}.", tokenAsSymbol);   // Temporary debug code.
                        parseStack.Push(go_to(S, tokenAsSymbol));

                        if (parse)
                        {
                            grammar.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, tokenList[tokenNum]);
                        }

                        // Get next token.
                        ++tokenNum;

                        if (tokenNum >= tokenList.Count)
                        {
                            //throw new SyntaxException("Unexpected end of token list");
                            tokenNum = tokenList.Count - 1; // Hack.  Even after the last token has been shifted, we still need to reduce.  So stick around.
                        }

                        tokenAsSymbol = grammar.TokenToSymbol(tokenList[tokenNum]);

                        break;

                    case ShiftReduceAction.Reduce:

                        if (reduceProductionNum < 0 || reduceProductionNum >= grammar.Productions.Count)
                        {
                            throw new InternalErrorException("Reduce: Invalid production number");
                        }

                        var unstrippedProduction = grammar.Productions[reduceProductionNum];

                        //Console.WriteLine("Reduce: Production is {0}.", unstrippedProduction);// Temporary debug code.

                        // Pop the production's non-Lambda symbols off of the parse stack.
                        unstrippedProduction.RHSWithNoSemanticActions()
                            .Where(symbol => symbol != Symbol.Lambda)
                            .ToList()
                            .ForEach(symbol => parseStack.Pop());

                        var SPrime = parseStack.Peek();

                        parseStack.Push(go_to(SPrime, unstrippedProduction.lhs));

                        if (parse && unstrippedProduction.rhs.Count > 0)
                        {
                            // Grammar requirement: Every semantic action string appears at the end of a production.
                            var semanticAction = unstrippedProduction.rhs[unstrippedProduction.rhs.Count - 1] as string;

                            if (semanticAction != null)
                            {
                                grammar.ExecuteSemanticAction(semanticStack, semanticAction);
                            }
                        }

                        break;

                    default:    // I.e. Error
                        throw new SyntaxException(
                            string.Format("LR0Parser.shift_reduce_driver() : Syntax error at symbol {0}", tokenAsSymbol),
                            tokenList[tokenNum].Line, tokenList[tokenNum].Column);
                }
            }

            throw new InternalErrorException("LR0Parser.shift_reduce_driver() : The parse stack is empty, but the Accept state has not been reached.");
        }

        public override void Recognize(List<Token> tokenList)
        {
            // Throws an exception if an error is encountered.
            shift_reduce_driver(tokenList, false);
        }

        public override object Parse(List<Token> tokenList)
        {
            return shift_reduce_driver(tokenList, true);
        }
    }

    #endregion
}
