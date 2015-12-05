using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Inference.Interpreter;
using Inference.Interpreter.LISP;
using Inference.Parser;

namespace Inference.MSTests.Interpreter.LISP
{
    [TestClass]
    public class Parser_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly LISPGlobalInfo globalInfo;

        public Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.LISP);
            parser = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.LISP);
            globalInfo = new LISPGlobalInfo(tokenizer, parser);
        }

        [TestInitialize]
        public void SetUp()
        {
            globalInfo.Clear();
            globalInfo.LoadPresets();
        }

        private object GetParseResult(string input)
        {
            var parseResult = parser.Parse(tokenizer.Tokenize(input));

            Assert.IsNotNull(parseResult);
            //Assert.AreEqual(input, parseResult.ToString());   // This fails on multi-line or whitespace-formatted input.
            return parseResult;
        }

        private string Evaluate(string input)
        {
            var expr = GetParseResult(input) as IExpression<ISExpression>;

            Assert.IsNotNull(expr);

            var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);

            Assert.IsNotNull(sexpr);

            return sexpr.ToString();
        }

        [TestMethod]
        public void RecognizeTest()
        {
            parser.Recognize(tokenizer.Tokenize("(define double (x) (+ x x))"));
            parser.Recognize(tokenizer.Tokenize("(double 5)"));

            parser.Recognize(tokenizer.Tokenize("(define +1 (n) (+ n 1))"));
            parser.Recognize(tokenizer.Tokenize("(+1 5)"));

            parser.Recognize(tokenizer.Tokenize("(define not (boolval) (if boolval 0 1))"));
            parser.Recognize(tokenizer.Tokenize("(define <> (x y) (not (= x y)))"));

            parser.Recognize(tokenizer.Tokenize("(define mod (m n) (- m (* n (/ m n))))"));
            parser.Recognize(tokenizer.Tokenize("(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"));

            parser.Recognize(tokenizer.Tokenize("13"));
            parser.Recognize(tokenizer.Tokenize("'13"));
            parser.Recognize(tokenizer.Tokenize("'T"));
            parser.Recognize(tokenizer.Tokenize("'()"));
            parser.Recognize(tokenizer.Tokenize("'(1 2 3)"));
        }

        [TestMethod]
        public void ParseIntegerLiteralTest()
        {
            var value = 13;
            var expr = parser.Parse(tokenizer.Tokenize(value.ToString())) as IntegerLiteral;

            Assert.IsNotNull(expr);
            Assert.AreEqual(value, expr.Value);
        }

        [TestMethod]
        public void ParseQuotedIntegerLiteralTest()
        {
            var value = 13;
            var parseResult = parser.Parse(tokenizer.Tokenize("'" + value.ToString()));

            Assert.IsNotNull(parseResult);
            Assert.AreEqual("'" + value.ToString(), parseResult.ToString());

            var expr = parseResult as IExpression<ISExpression>;

            Assert.IsNotNull(expr);

            var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);

            Assert.IsNotNull(sexpr);
            Assert.IsTrue(sexpr.IsNumber());
            Assert.IsTrue(sexpr is IntegerLiteral);

            var intlit = sexpr as IntegerLiteral;

            Assert.AreEqual(value, intlit.Value);
        }

        [TestMethod]
        public void ParseQuotedSymbolTest()
        {
            var value = "T";
            var parseResult = parser.Parse(tokenizer.Tokenize("'" + value));

            Assert.IsNotNull(parseResult);
            Assert.AreEqual("'" + value, parseResult.ToString());

            var expr = parseResult as IExpression<ISExpression>;

            Assert.IsNotNull(expr);

            var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);

            Assert.IsNotNull(sexpr);
            Assert.IsTrue(sexpr.IsSymbol());
            Assert.IsTrue(sexpr is LISPSymbol);

            var symbol = sexpr as LISPSymbol;

            Assert.AreEqual(value, symbol.Value);
        }
    }
}
