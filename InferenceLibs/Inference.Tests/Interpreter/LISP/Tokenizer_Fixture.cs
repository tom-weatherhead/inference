using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.LISP
{
    [TestFixture]
    public class Tokenizer_Fixture
    {
        private readonly ITokenizer tokenizer;

        public Tokenizer_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.LISP);
        }

        /*
        [Test]
        public void ApostropheTest()
        {
            var listOfTokens = tokenizer.Tokenize("'");

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_Apostrophe, listOfTokens[0].TokenType);
            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void PredicateTest()
        {
            var predicateName = "abc_123";
            var listOfTokens = tokenizer.Tokenize(predicateName + "?");

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_BoolIdent, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual(predicateName, (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }
         */

        [Test]
        public void PlusOneTest()
        {
            var plusOneString = "+1";
            var listOfTokens = tokenizer.Tokenize(plusOneString);

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_Ident, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual(plusOneString, (string)listOfTokens[0].TokenValue);
            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void HangTest()
        {
            tokenizer.Tokenize("(define double (x) (+ x x))");
            tokenizer.Tokenize("(double 5)");

            tokenizer.Tokenize("(define +1 (n) (+ n 1))");
            tokenizer.Tokenize("(+1 5)");
        }

        [Test]
        public void CommentTest()
        {
            var sb = new StringBuilder();

            sb.AppendLine("(+ 2 ; Comment");
            sb.AppendLine("3)");

            var listOfTokens = tokenizer.Tokenize(sb.ToString());

            Assert.AreEqual(6, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_LeftBracket, listOfTokens[0].TokenType);
            Assert.AreEqual(TokenType.T_Ident, listOfTokens[1].TokenType);
            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[2].TokenType);
            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[3].TokenType);
            Assert.AreEqual(TokenType.T_RightBracket, listOfTokens[4].TokenType);
            Assert.AreEqual(TokenType.T_EOF, listOfTokens[5].TokenType);
        }

        [Test]
        public void IntLiteralTest()
        {
            var inputValues = new List<int>() { 1, -1 };
            var listOfTokens = tokenizer.Tokenize(string.Join(" ", inputValues));

            Assert.AreEqual(3, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is int);
            Assert.AreEqual(inputValues[0], (int)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[1].TokenType);
            Assert.IsTrue(listOfTokens[1].TokenValue is int);
            Assert.AreEqual(inputValues[1], (int)listOfTokens[1].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[2].TokenType);
        }

        [Test]
        public void FloatLiteralTest()
        {
            var inputValues = new List<double>() { 13.0, -1.25, 0.00125, -1250.0 };
            //var listOfTokens = tokenizer.Tokenize(string.Join(" ", inputValues)); // If double x == 13.0, then x.ToString() appears to be "13", not "13.0".
            var listOfTokens = tokenizer.Tokenize("13.0 -1.25 1.25e-3 -1.25e3");

            Assert.AreEqual(5, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_FltLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is double);
            Assert.AreEqual(inputValues[0], (double)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_FltLit, listOfTokens[1].TokenType);
            Assert.IsTrue(listOfTokens[1].TokenValue is double);
            Assert.AreEqual(inputValues[1], (double)listOfTokens[1].TokenValue);

            Assert.AreEqual(TokenType.T_FltLit, listOfTokens[2].TokenType);
            Assert.IsTrue(listOfTokens[2].TokenValue is double);
            Assert.AreEqual(inputValues[2], (double)listOfTokens[2].TokenValue);

            Assert.AreEqual(TokenType.T_FltLit, listOfTokens[3].TokenType);
            Assert.IsTrue(listOfTokens[3].TokenValue is double);
            Assert.AreEqual(inputValues[3], (double)listOfTokens[3].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[4].TokenType);
        }

        [Test]
        public void StringLiteralTest()
        {
            const string expectedValue = "ABC";
            var inputString = string.Format("\"{0}\"", expectedValue);
            var listOfTokens = tokenizer.Tokenize(inputString);

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_StrLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual(expectedValue, (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void NewlinePrefixTest()
        {
            var listOfTokens = tokenizer.Tokenize("\n\n\n7");

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is int);
            Assert.AreEqual(7, (int)listOfTokens[0].TokenValue);
            Assert.AreEqual(4, listOfTokens[0].Line);   // This is the important test.

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }
    }
}
