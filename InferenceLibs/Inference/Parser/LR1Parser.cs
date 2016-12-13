using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Inference.Parser
{
    #region ShiftReduceAction

    public enum ShiftReduceAction
    {
        Error,
        Accept,
        Shift,
        Reduce
    }

    #endregion

    #region LR1Configuration

    public class LR1Configuration : LR0Configuration
    {
        public readonly Symbol Lookahead;

        public LR1Configuration(Symbol lhs, Symbol look)
            : base(lhs)
        {
            Lookahead = look;
        }

        public LR1Configuration(LR0Configuration src, Symbol look)
            : base(src)
        {
            Lookahead = look;
        }

        public LR1Configuration(Production p, Symbol look)
            : base(p)
        {
            Lookahead = look;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            LR0Configuration thatBase = obj as LR0Configuration;
            LR1Configuration that = obj as LR1Configuration;

            return base.Equals(thatBase) && that != null && Lookahead == that.Lookahead;
        }

        public override int GetHashCode()
        {
            return base.GetHashCode() * 101 + Lookahead.GetHashCode();  // ThAW 2013/05/08: The + was a second *
        }

        // The "new" keyword is used here because this function hides a function in the base class which differs only by return type.

        public new LR1Configuration AdvanceDot()
        {
            return new LR1Configuration(base.AdvanceDot(), Lookahead);
        }
    }

    #endregion

    #region FSMState

    public class FSMState
    {
        public readonly HashSet<LR1Configuration> ConfigurationSet;
        public readonly Dictionary<Symbol, FSMState> Transitions = new Dictionary<Symbol, FSMState>();

        public FSMState(HashSet<LR1Configuration> cs)
        {
            ConfigurationSet = cs;
        }

        public override bool Equals(object obj)
        {
            // TODO: Find a better implementation for this function.  Beware of cycles in the finite state machine (or ignore the transitions in this function).
            // Note: The LR(1) parser works with the current code.

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            FSMState that = obj as FSMState;

            // TODO: Should we also consider Transitions.Keys?
            return that != null && ConfigurationSet.IsSubsetOf(that.ConfigurationSet) && that.ConfigurationSet.IsSubsetOf(ConfigurationSet);
        }

        public override int GetHashCode()
        {
            // TODO: Find a better implementation for this function.  Beware of cycles in the finite state machine (or ignore the transitions in this function).
            // Note: The LR(1) parser works with the current code.
            /*
            int hashCode = 0;

            foreach (LR1Configuration conf in ConfigurationSet)
            {
                // The order of the configurations in the set doesn't affect the hash code.
                hashCode += conf.GetHashCode();
            }

            // TODO: Should we also consider Transitions.Keys?
            return hashCode;
             */

            // The order of the configurations in the set doesn't affect the hash code.
            return ConfigurationSet
                .Select(conf => conf.GetHashCode())
                .Aggregate(0, (accumulator, hashCode) => accumulator + hashCode);
        }
    }

    #endregion

    #region FiniteStateMachine

    public class FiniteStateMachine
    {
        public readonly List<FSMState> StateList = new List<FSMState>();
        public readonly FSMState StartState;

        public FiniteStateMachine(FSMState ss)
        {
            StartState = ss;
            StateList.Add(ss);
        }

        public FSMState FindStateWithLabel(HashSet<LR1Configuration> cs)
        {
            // Returns null if no state has the given configuration set.
            return StateList.Find(state => cs.IsSubsetOf(state.ConfigurationSet) && state.ConfigurationSet.IsSubsetOf(cs));
        }
    }

    #endregion

    #region StateSymbolPair

    class StateSymbolPair
    {
        public readonly FSMState state;
        public readonly Symbol symbol;

        public StateSymbolPair(FSMState st, Symbol sy)
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

            StateSymbolPair that = obj as StateSymbolPair;

            return that != null && state.Equals(that.state) && symbol == that.symbol;
        }

        public override int GetHashCode()
        {
            return state.GetHashCode() * 101 + symbol.GetHashCode();
        }
    }

    #endregion

    #region LR1Parser

    public class LR1Parser : ParserBase
    {
        private readonly HashSet<Symbol> AllSymbols;
        public readonly FiniteStateMachine machine;
        private readonly Dictionary<StateSymbolPair, FSMState> GoToTable = new Dictionary<StateSymbolPair, FSMState>();

        public LR1Parser(IGrammar g)
            : base(g)
        {
            AllSymbols = new HashSet<Symbol>(g.Terminals);
            AllSymbols.UnionWith(g.NonTerminals);
            machine = build_LR1();
            build_go_to_table();
        }

        public LR1Parser(GrammarSelector gs)
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

        // Adapted from Fischer and LeBlanc, page 156.

        private HashSet<LR1Configuration> closure1(HashSet<LR1Configuration> s)
        {
            var sPrime = new HashSet<LR1Configuration>(s);
            var additions = new HashSet<LR1Configuration>();

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

                    var rho = conf1.FindSuffix(1);

                    if (rho == null)
                    {
                        continue;
                    }

                    var l = conf1.Lookahead;

                    if (l != Symbol.Lambda || rho.Count == 0) // Test
                    {
                        rho.Add(l);     // Now rho is actually rho l.
                    }

                    var firstSet = ComputeFirst(rho);

                    foreach (var p in grammar.Productions)
                    {

                        if (p.lhs != A)
                        {
                            continue;
                        }

                        foreach (var u in firstSet)
                        {
                            var addition = new LR1Configuration(p.StripOutSemanticActions(), u);

                            if (!sPrime.Contains(addition) && !additions.Contains(addition))
                            {
                                additions.Add(addition);
                            }
                        }
                    }
                }

                sPrime.UnionWith(additions);
            }
            while (additions.Count > 0);

            return sPrime;
        }

        // Adapted from Fischer and LeBlanc, page 157.

        private HashSet<LR1Configuration> go_to1(HashSet<LR1Configuration> s, Symbol X)
        {
            var sb = new HashSet<LR1Configuration>();

            foreach (var c in s)
            {
                Symbol symbol;

                if (!c.FindSymbolAfterDot(out symbol) || symbol != X)
                {
                    continue;
                }

                sb.Add(c.AdvanceDot());
            }

            return closure1(sb);
        }

        // See Fischer and LeBlanc, page 157.

        private HashSet<LR1Configuration> compute_s0()
        {
            var p = grammar.FindStartingProduction();

            return closure1(new HashSet<LR1Configuration>() { new LR1Configuration(p, Symbol.Lambda) });
        }

        // Adapted from Fischer and LeBlanc, page 158.

        private FiniteStateMachine build_LR1()
        {
            var s0 = compute_s0();
            var startState = new FSMState(s0);
            var fsm = new FiniteStateMachine(startState);
            var S = new Stack<HashSet<LR1Configuration>>();

            S.Push(s0);

            while (S.Count > 0)
            {
                var s = S.Pop();

                // Consider both terminals and non-terminals.

                foreach (var X in AllSymbols)
                {
                    var g = go_to1(s, X);

                    /*
                    if (g.Count == 0)
                    {
                        continue;
                    }
                     */

                    var stateG = fsm.FindStateWithLabel(g);

                    if (stateG == null)
                    {
                        stateG = new FSMState(g);
                        fsm.StateList.Add(stateG);
                        S.Push(g);
                    }

                    // Create a transition under X from the state s labels to the state g labels.
                    var stateS = fsm.FindStateWithLabel(s);

                    if (stateS.Transitions.ContainsKey(X))
                    {
                        throw new InternalErrorException("LR1Parser.build_LR1() : Finite state machine transition is being overwritten.");
                    }

                    stateS.Transitions[X] = stateG;
                }
            }

            return fsm;
        }

        // Adapted from Fischer and LeBlanc, pages 158-159.

        private ShiftReduceAction GetAction(FSMState S, Symbol tokenAsSymbol, out int reduceProductionNum)
        {
            var result = ShiftReduceAction.Error;
            var reduceResultFound = false;   // In order for the grammar to be LR(1), there must be at most one result per state-symbol pair.

            reduceProductionNum = -1;

            // 1) Search for Reduce actions.

            foreach (var c in S.ConfigurationSet)
            {

                if (c.Lookahead != tokenAsSymbol)
                {
                    continue;
                }

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

                        if (reduceResultFound && reduceProductionNum != i)
                        {
                            throw new ReduceReduceConflictException("GetAction() : Multiple actions found; grammar is not LR(1).");
                        }

                        result = ShiftReduceAction.Reduce;
                        reduceProductionNum = i;
                        reduceResultFound = true;
                    }
                }
            }

            // 2) Search for Shift and Accept actions.
            Symbol symbol;
            var shiftOrAcceptResultFound = S.ConfigurationSet.Any(c => c.FindSymbolAfterDot(out symbol) && symbol == tokenAsSymbol);

            if (shiftOrAcceptResultFound)
            {

                if (reduceResultFound)
                {
                    throw new ShiftReduceConflictException("GetAction() : Multiple actions found; grammar is not LR(1).");
                }

                result = (tokenAsSymbol == Symbol.T_EOF) ? ShiftReduceAction.Accept : ShiftReduceAction.Shift;
            }

            return result;
        }

        // Adapted from Fischer and LeBlanc, page 150.
        
        private void build_go_to_table()
        {
            GoToTable.Clear();

            foreach (var S in machine.StateList)
            {

                foreach (var X in S.Transitions.Keys)
                {
                    GoToTable[new StateSymbolPair(S, X)] = S.Transitions[X];
                }
            }
        }

        private FSMState go_to(FSMState S, Symbol tokenAsSymbol)
        {
            var pair = new StateSymbolPair(S, tokenAsSymbol);

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
            var parseStack = new Stack<FSMState>();  // The parse stack, which contains machine states.
            var semanticStack = new Stack<object>();

            parseStack.Push(machine.StartState);

            while (parseStack.Count > 0)
            {
                var S = parseStack.Peek();
                int reduceProductionNum;
                var action = GetAction(S, tokenAsSymbol, out reduceProductionNum);

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
                            Console.WriteLine("Beginning of semantic stack dump:");

                            while (semanticStack.Count > 0)
                            {
                                var o = semanticStack.Pop();

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
                            throw new GrammarException(string.Format("There were {0} objects on the semantic stack; expected exactly one", semanticStackSize));
                        }

                        return semanticStack.Pop();

                    case ShiftReduceAction.Shift:
                        parseStack.Push(go_to(S, tokenAsSymbol));

                        if (parse)
                        {
                            grammar.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, tokenList[tokenNum]);
                        }

                        // Get next token.
                        ++tokenNum;

                        if (tokenNum >= tokenList.Count)
                        {
                            throw new SyntaxException("Unexpected end of token list");
                        }

                        tokenAsSymbol = grammar.TokenToSymbol(tokenList[tokenNum]);

                        break;

                    case ShiftReduceAction.Reduce:

                        if (reduceProductionNum < 0 || reduceProductionNum >= grammar.Productions.Count)
                        {
                            throw new InternalErrorException("Reduce: Invalid production number");
                        }

                        var unstrippedProduction = grammar.Productions[reduceProductionNum];
                        /*
                        Production p = unstrippedProduction.StripOutSemanticActions();

                        for (int i = 0; i < p.rhs.Count; ++i)
                        {

                            if (!p.rhs[i].Equals(Symbol.Lambda))    // Test; hack.
                            {
                                parseStack.Pop();
                            }
                        }
                         */

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
                            string.Format("LR1Parser.shift_reduce_driver() : Syntax error at symbol {0}", tokenAsSymbol),
                            tokenList[tokenNum].Line, tokenList[tokenNum].Column);
                }
            }

            throw new InternalErrorException("LR1Parser.shift_reduce_driver() : The parse stack is empty, but the Accept state has not been reached.");
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
