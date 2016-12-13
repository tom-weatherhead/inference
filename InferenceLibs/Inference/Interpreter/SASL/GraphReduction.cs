using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.LISP;
using Inference.Interpreter.Scheme;
using Inference.Parser;

// See Kamin, section 5.9 (pages 186 - 197) and exercises 9 - 14 (page 203).

namespace Inference.Interpreter.SASL
{
    #region IConvertibleToGraph

    public interface IConvertibleToGraph
    {
        IExpression<ISExpression> ConvertToGraph();
    }

    #endregion

    #region GraphReductionNode

    // An interior (non-leaf) node of the graph.

    public class GraphReductionNode : IExpression<ISExpression>
    {
        public IExpression<ISExpression> LeftChild;
        public IExpression<ISExpression> RightChild;

        public GraphReductionNode(IExpression<ISExpression> l, IExpression<ISExpression> r)
        {
            LeftChild = l;
            RightChild = r;
        }

        public override string ToString()
        {
            return string.Format("({0} {1})", LeftChild, RightChild);
        }

        public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            throw new NotImplementedException("GraphReductionNode.Evaluate()");
        }
    }

    #endregion

    #region SKIOp

    public class SKIOp : IExpression<ISExpression>
    {
        public readonly string Name;
        public readonly int ExpectedNumArgs;

        public SKIOp(string name)
        {
            Name = name;

            switch (Name)
            {
                case "S":
                    ExpectedNumArgs = 3;
                    break;

                case "K":
                    ExpectedNumArgs = 2;
                    break;

                case "I":
                    ExpectedNumArgs = 1;
                    break;

                default:
                    throw new Exception(string.Format("SKIOp constructor: Invalid name '{0}'", name));
            }
        }

        public override string ToString()
        {
            return Name;
        }

        public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            throw new Exception("SKIOps cannot be evaluated; they must be reduced");
        }
    }

    #endregion

    #region GraphReducer

    public class GraphReducer
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly EnvironmentFrame<ISExpression> localEnvironment;   // ... or should we just use the global environment?
        private readonly IGlobalInfo<ISExpression> globalInfo;
        private readonly Dictionary<Variable<ISExpression>, IExpression<ISExpression>> dictGraphs = new Dictionary<Variable<ISExpression>, IExpression<ISExpression>>();

        public GraphReducer(ITokenizer t, IParser p, EnvironmentFrame<ISExpression> l, IGlobalInfo<ISExpression> g)
        {

            if (t == null)
            {
                tokenizer = TokenizerFactory.Create(GrammarSelector.SASL);
            }
            else
            {
                tokenizer = t;
            }

            if (p == null)
            {
                parser = ParserFactory.Create(ParserSelector.LALR1, GrammarSelector.SASL);
            }
            else
            {
                parser = p;
            }

            localEnvironment = l;
            globalInfo = g;
        }

        public void Clear()
        {
            dictGraphs.Clear();
        }

        public void Reduce(ref IExpression<ISExpression> n)   // See page 195.
        {
            var done = false;
            var nsave = n;
            var args = new Stack<GraphReductionNode>();

            while (!done)
            {

                while (n is GraphReductionNode) // A GraphReductionNode always represents the application of a function.
                {
                    var temp = (GraphReductionNode)n;

                    args.Push(temp);
                    n = temp.LeftChild;
                }

                // n is an atomic node.

                if (n is IntegerLiteral)
                {
                    done = true;
                }
                else if (n is Variable<ISExpression>)
                {
                    var v = (Variable<ISExpression>)n;

                    // TODO: If the value associated with v is a closure, should we convert the closure's expression to SKI and assign it to n?
                    // What would we do about the closure's environment?

                    if (!dictGraphs.ContainsKey(v))
                    {
                        throw new Exception(string.Format("Reduce() : Undefined symbol '{0}'.", v));
                    }

                    n = dictGraphs[v];
                }
                else if (n is SKIOp)
                {
                    var skiOp = (SKIOp)n;

                    if (args.Count < skiOp.ExpectedNumArgs)
                    {
                        done = true;
                    }
                    else
                    {
                        GraphReductionNode n1;
                        GraphReductionNode n2;
                        GraphReductionNode n3;

                        switch (skiOp.Name)
                        {
                            case "S":
                                n1 = args.Pop();
                                n2 = args.Pop();
                                n3 = args.Pop();
                                n = PerformS(n1, n2, n3);
                                break;

                            case "K":
                                n1 = args.Pop();
                                n2 = args.Pop();
                                n = PerformK(n1, n2);
                                break;

                            case "I":
                                n1 = args.Pop();
                                n = PerformI(n1);
                                break;

                            default:
                                throw new Exception(string.Format("Reduce() : Invalid SKIOp '{0}'", skiOp));
                        }
                    }
                }
                else // n is a value-op, including if, S, K, or I.
                {
                    var valueOp = (SASLPrimOp)n;
                    GraphReductionNode n1;
                    GraphReductionNode n2;
                    GraphReductionNode n3;

                    if (args.Count < valueOp.ExpectedNumArgs)
                    {
                        done = true;
                    }
                    else if (valueOp.OperatorName.Value == "if")
                    {
                        n1 = args.Pop();
                        n2 = args.Pop();
                        n3 = args.Pop();
                        n = PerformIf(n1, n2, n3);
                    }
                    else if (valueOp.ExpectedNumArgs != 2)
                    {
                        throw new Exception(string.Format("Reduce() : Cannot perform the value op '{0}' because it does not take 2 arguments", valueOp));
                    }
                    else
                    {
                        n1 = args.Pop();
                        n2 = args.Pop();
                        n = PerformValueOp(valueOp, n1, n2);
                    }
                }

                if (!done)
                {
                    // Save the result of the latest reduction or substitution..

                    if (args.Count > 0)
                    {
                        args.Peek().LeftChild = n;
                    }
                    else
                    {
                        nsave = n;
                    }
                }
            }

            n = nsave;
        }

        public GraphReductionNode PerformS(GraphReductionNode n1, GraphReductionNode n2, GraphReductionNode n3)   // See page 196.
        {
            return new GraphReductionNode(
                new GraphReductionNode(n1.RightChild, n3.RightChild),
                new GraphReductionNode(n2.RightChild, n3.RightChild));
        }

        public IExpression<ISExpression> PerformK(GraphReductionNode n1, GraphReductionNode n2)  // Exercise 12
        {
            return n1.RightChild;
        }

        public IExpression<ISExpression> PerformI(GraphReductionNode n1) // Exercise 12
        {
            return n1.RightChild;
        }

        public IExpression<ISExpression> PerformIf(GraphReductionNode n1, GraphReductionNode n2, GraphReductionNode n3)    // Exercise 12
        {
            var n1RightChild = n1.RightChild;
            var n2RightChild = n2.RightChild;
            var n3RightChild = n3.RightChild;

            Reduce(ref n1RightChild);

            var n1RightChildAsInt = n1RightChild as IntegerLiteral;

            if (n1RightChildAsInt != null && n1RightChildAsInt.Value == 0)
            {
                // Else part.
                Reduce(ref n3RightChild);
                return n3RightChild;
            }
            else
            {
                // Then part.
                Reduce(ref n2RightChild);
                return n2RightChild;
            }
        }

        public IExpression<ISExpression> PerformValueOp(SASLPrimOp valueOp, GraphReductionNode n1, GraphReductionNode n2) // See page 196.
        {
            Reduce(ref n1.RightChild);
            Reduce(ref n2.RightChild);

            var args = new ExpressionList<ISExpression>();

            args.Value.Add(n1.RightChild);
            args.Value.Add(n2.RightChild);

            var result = valueOp.Call(args, localEnvironment, globalInfo);

            if (result.Equals(globalInfo.TrueValue))
            {
                return new IntegerLiteral(1);
            }
            else if (result.Equals(globalInfo.FalseValue))
            {
                return new IntegerLiteral(0);
            }
            else if (!(result is IntegerLiteral))
            {
                throw new Exception(string.Format("Graph reduction: PerformValueOp: Unexpected result '{0}' of type {1}.",
                    result, result.GetType().FullName));
            }

            return (IExpression<ISExpression>)result;
        }

        private IExpression<ISExpression> ProcessSet(SetUsage<ISExpression> setUsage)
        {
            var convertibleExpr = setUsage.Expression as IConvertibleToGraph;

            if (convertibleExpr == null)
            {
                throw new Exception("ProcessInput() : Set usage: Expression is not IConvertibleToGraph.");
            }

            var graph = convertibleExpr.ConvertToGraph();

            dictGraphs[setUsage.VariableName] = graph;
            return graph;
        }

        public IExpression<ISExpression> ProcessInput(string input)
        {
            var parseResult = parser.Parse(tokenizer.Tokenize(input));

            if (parseResult is SetUsage<ISExpression>)
            {
                return ProcessSet((SetUsage<ISExpression>)parseResult);
            }

            var convertibleExpr = parseResult as IConvertibleToGraph;

            if (convertibleExpr == null)
            {
                throw new Exception("ProcessInput() : Expression is not IConvertibleToGraph.");
            }

            IExpression<ISExpression> graph = convertibleExpr.ConvertToGraph();

            Reduce(ref graph);
            return graph;
        }
    }

    #endregion
}
