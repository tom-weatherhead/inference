using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Interpreter.LISP;
using Inference.Interpreter.SASL;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.SASL
{
    [TestFixture]
    public class GraphReduction_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly SASLGlobalInfo globalInfo;
        private readonly GraphReducer graphReducer;
        private readonly SKIOp S;
        private readonly SKIOp K;
        private readonly SKIOp I;

        public GraphReduction_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.SASL);
            parser = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.SASL);
            globalInfo = new SASLGlobalInfo(tokenizer, parser);
            graphReducer = new GraphReducer(tokenizer, parser, globalInfo.GlobalEnvironment, globalInfo);
            S = new SKIOp("S");
            K = new SKIOp("K");
            I = new SKIOp("I");
        }

        [SetUp]
        public void SetUp()
        {
            graphReducer.Clear();
        }

        private object GetParseResult(string input)
        {
            var parseResult = parser.Parse(tokenizer.Tokenize(input));

            Assert.IsNotNull(parseResult);
            //Assert.AreEqual(input, parseResult.ToString());   // This fails on multi-line or whitespace-formatted input.
            return parseResult;
        }

        private IExpression<ISExpression> GetIExpression(string input)
        {
            var expr = GetParseResult(input) as IExpression<ISExpression>;

            Assert.IsNotNull(expr);

            return expr;
        }

        [Test]
        public void ValueOpTest()
        {
            var n1 = new GraphReductionNode(
                new SASLPrimOp(new Name("*", 0, 0)),
                new IntegerLiteral(7));
            IExpression<ISExpression> n2 = new GraphReductionNode(n1, new IntegerLiteral(13));

            graphReducer.Reduce(ref n2);

            Assert.IsTrue(n2 is IntegerLiteral);
            Assert.AreEqual("91", n2.ToString());
        }

        [Test]
        public void DoubleTest()    // Calculate (double 5); see page 191.
        {
            const int x = 5;
            const int expectedResult = 2 * x;
            var n1 = new GraphReductionNode(K, new SASLPrimOp(new Name("*", 0, 0)));            // (K *)
            var n2 = new GraphReductionNode(S, n1);                                             // (S (K *))
            var n3 = new GraphReductionNode(n2, I);                                             // ((S (K *)) I)
            var n4 = new GraphReductionNode(S, n3);                                             // (S ((S (K *)) I))
            var n5 = new GraphReductionNode(K, new IntegerLiteral(2));                          // (K 2)
            var n6 = new GraphReductionNode(n4, n5);                                            // ((S ((S (K *)) I)) (K 2))
            IExpression<ISExpression> n7 = new GraphReductionNode(n6, new IntegerLiteral(x));   // (((S ((S (K *)) I)) (K 2)) 5)

            graphReducer.Reduce(ref n7);

            Assert.IsTrue(n7 is IntegerLiteral);
            Assert.AreEqual(expectedResult.ToString(), n7.ToString());
        }

        [Test]
        public void AbstractionTest()
        {
            var expr = GetIExpression("(lambda (x) ((* x) 2))") as IConvertibleToGraph;    // Note the currying of *

            Assert.IsNotNull(expr);

            var abstractedGraph = expr.ConvertToGraph();

            Assert.AreEqual("((S ((S (K *)) I)) (K 2))", abstractedGraph.ToString());

            const int x = 5;
            const int expectedResult = 2 * x;
            IExpression<ISExpression> n7 = new GraphReductionNode(abstractedGraph, new IntegerLiteral(x));  // (((S ((S (K *)) I)) (K 2)) 5)

            graphReducer.Reduce(ref n7);

            Assert.IsTrue(n7 is IntegerLiteral);
            Assert.AreEqual(expectedResult.ToString(), n7.ToString());
        }

        [Test]
        public void ProcessInputTest()
        {
            const int x = 5;
            const int expectedResult = 2 * x;

            Assert.AreEqual("((S ((S (K *)) I)) (K 2))", graphReducer.ProcessInput("(set double (lambda (x) ((* x) 2)))").ToString());

            var exprString = string.Format("(double {0})", x);

            Assert.AreEqual(expectedResult.ToString(), graphReducer.ProcessInput(exprString).ToString());
        }

        [Test]
        public void CurriedMultTest()
        {
            graphReducer.ProcessInput("(set mult (lambda (x) (lambda (y) ((* x) y))))");

            Assert.AreEqual("91", graphReducer.ProcessInput("((mult 7) 13)").ToString());
        }

        [Test]
        public void CurriedMinusTest()  // In this case, the order of the operands is important, since - is not commutative.
        {
            graphReducer.ProcessInput("(set minus (lambda (x) (lambda (y) ((- x) y))))");

            Assert.AreEqual("6", graphReducer.ProcessInput("((minus 13) 7)").ToString());
        }

        [Test]
        public void CurriedIfTest()
        {
            Assert.AreEqual("0", graphReducer.ProcessInput("((= 13) 7)").ToString());
            Assert.AreEqual("1", graphReducer.ProcessInput("((= 7) 7)").ToString());

            Assert.AreEqual("28", graphReducer.ProcessInput("(((if ((= 13) 7)) 6) 28)").ToString());
            Assert.AreEqual("6", graphReducer.ProcessInput("(((if ((= 7) 7)) 6) 28)").ToString());
        }

        [Test]
        public void GCDTest()
        {
            graphReducer.ProcessInput("(set mod (lambda (m) (lambda (n) ((- m) ((* n) ((/ m) n))))))");
            graphReducer.ProcessInput("(set gcd (lambda (m) (lambda (n) (((if ((= n) 0)) m) ((gcd n) ((mod m) n))))))");

            Assert.AreEqual("6", graphReducer.ProcessInput("((mod 13) 7)").ToString());
            Assert.AreEqual("1", graphReducer.ProcessInput("((mod 7) 6)").ToString());
            Assert.AreEqual("0", graphReducer.ProcessInput("((mod 6) 1)").ToString());

            Assert.AreEqual("1", graphReducer.ProcessInput("((gcd 13) 7)").ToString());
            Assert.AreEqual("4", graphReducer.ProcessInput("((gcd 144) 100)").ToString());
        }

        [Test]
        public void DoubleReductionTest()
        {
            graphReducer.ProcessInput("(set mult (lambda (x) (lambda (y) ((* x) y))))");

            var graph1 = graphReducer.ProcessInput("(mult 7)"); // The first reduction occurs here.
            IExpression<ISExpression> graph2 = new GraphReductionNode(graph1, new IntegerLiteral(13));

            graphReducer.Reduce(ref graph2);                    // The second reduction occurs here.

            Assert.AreEqual("91", graph2.ToString());
        }
    }
}
