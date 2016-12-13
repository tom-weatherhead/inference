using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Domain;

namespace Inference.Parser
{
    #region SymbolPair

    class SymbolPair
    {
        public Symbol NonTerminal { get; set; }
        public Symbol Terminal { get; set; }

        public SymbolPair(Symbol n, Symbol t)
        {
            NonTerminal = n;
            Terminal = t;
        }

        public override bool Equals(object obj)
        {
            var other = obj as SymbolPair;

            return other != null && NonTerminal == other.NonTerminal && Terminal == other.Terminal;
        }

        public override int GetHashCode()
        {
            return NonTerminal.GetHashCode() * 101 + Terminal.GetHashCode();
        }
    }

    #endregion

    #region LL1Parser

    public class LL1Parser : ParserBase
    {
        private readonly Dictionary<Production, HashSet<Symbol>> Predict = new Dictionary<Production, HashSet<Symbol>>();
        private readonly Dictionary<SymbolPair, Production> ParseTable = new Dictionary<SymbolPair, Production>();

        public LL1Parser(IGrammar g)
            : base(g)
        {
            FillPredict();
            FillParseTable();
        }

        public LL1Parser(GrammarSelector gs)
            : this(GrammarFactory.Create(gs))
        {
        }

        private void FillPredict()
        {

            foreach (var p in grammar.Productions)
            {
                var s = ComputeFirst(p.RHSWithNoSemanticActions());

                if (s.Contains(Symbol.Lambda))
                {
                    s = withoutLambda(s);
                    s.UnionWith(follow_set[p.lhs]);
                }

                Predict[p] = s;
            }
        }

        private void FillParseTable()
        {

            foreach (var p in grammar.Productions)
            {
                var predict = Predict[p];

                foreach (var t in predict)
                {
                    var sp = new SymbolPair(p.lhs, t);

                    if (ParseTable.ContainsKey(sp))
                    {
                        throw new GrammarException(string.Format(
                            "Error in FillParseTable() : Table entry not unique; p.lhs = {0}; t = {1}; p1 = {2}; p2 = {3}",
                            p.lhs, t, ParseTable[sp], p));
                    }

                    ParseTable[sp] = p;
                }
            }
        }

        // Adapted from Fischer and LeBlanc, page 121 (function lldriver())

        private object LLDriver(List<Token> tokenList, bool parse)
        {

            if (tokenList.Count == 0)
            {
                throw new SyntaxException("Token list is empty");
            }

            var tokenNum = 0;
            var tokenAsSymbol = grammar.TokenToSymbol(tokenList[tokenNum]);
            var parseStack = new Stack<object>();  // The parse stack
            var semanticStack = new Stack<object>();

            parseStack.Push(grammar.StartSymbol);

            while (parseStack.Count > 0)
            {
                var X = parseStack.Peek();

                if (X is string)
                {

                    if (parse)
                    {
                        var action = X as string;

                        grammar.ExecuteSemanticAction(semanticStack, action);
                    }

                    parseStack.Pop();
                }
                else if (X is Symbol)
                {
                    var symbolX = (Symbol)X;
                    var sp = new SymbolPair(symbolX, tokenAsSymbol);

                    if (grammar.NonTerminals.Contains(symbolX) && ParseTable.ContainsKey(sp))
                    {
                        var p = ParseTable[sp];

                        //Console.WriteLine("Using production " + p.ToString());
                        parseStack.Pop();

                        for (var i = p.rhs.Count - 1; i >= 0; --i)
                        {

                            if (!(p.rhs[i] is Symbol) || (Symbol)p.rhs[i] != Symbol.Lambda)
                            {
                                // Push semantic actions, and any symbols except Lambda.
                                parseStack.Push(p.rhs[i]);
                            }
                        }
                    }
                    else if (symbolX == tokenAsSymbol)
                    {
                        //Console.WriteLine("Matched token/symbol " + X.ToString());

                        if (parse)
                        {
                            grammar.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, tokenList[tokenNum]);
                        }

                        parseStack.Pop();

                        if (parseStack.Count > 0)
                        {
                            ++tokenNum;

                            if (tokenNum >= tokenList.Count)
                            {
                                throw new SyntaxException("End of token list; parse stack is not empty");
                            }

                            tokenAsSymbol = grammar.TokenToSymbol(tokenList[tokenNum]);
                        }
                    }
                    else
                    {
                        throw new SyntaxException(
                            string.Format("Failed to match symbol {0} to symbol {1} (token {2}) value {3}",
                                X, tokenAsSymbol, tokenList[tokenNum].TokenType, tokenList[tokenNum].TokenValue),
                            tokenList[tokenNum].Line, tokenList[tokenNum].Column);
                    }
                }
                else
                {
                    throw new InternalErrorException("Unrecognized parse stack entry of type " + ((X != null) ? X.GetType().FullName : "null"));
                }
            }

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
        }

        public override void Recognize(List<Token> tokenList)
        {
            // Throws an exception if an error is encountered.
            LLDriver(tokenList, false);
        }

        public override object Parse(List<Token> tokenList)
        {
            return LLDriver(tokenList, true);
        }
    }

    #endregion
}
