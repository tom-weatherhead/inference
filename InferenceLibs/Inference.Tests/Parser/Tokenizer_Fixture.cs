using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Parser
{
    [TestFixture]
    public class Tokenizer_Fixture
    {
        private readonly ITokenizer tokenizer;

        public Tokenizer_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
        }

        [Test]
        public void EmptyStringTest()
        {
            List<Token> listOfTokens = tokenizer.Tokenize(string.Empty);

            Assert.AreEqual(1, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[0].TokenType);
        }

        [Test]
        public void PositiveIntLitTest()
        {
            int n = 123;
            List<Token> listOfTokens = tokenizer.Tokenize(n.ToString());

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is int);
            Assert.AreEqual(n, (int)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void NegativeIntLitTest()
        {
            int n = -123;
            List<Token> listOfTokens = tokenizer.Tokenize(n.ToString());

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is int);
            Assert.AreEqual(n, (int)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void PosNegIntLitsTest()
        {
            List<Token> listOfTokens = tokenizer.Tokenize("123-456");

            Assert.AreEqual(3, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is int);
            Assert.AreEqual(123, (int)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[1].TokenType);
            Assert.IsTrue(listOfTokens[1].TokenValue is int);
            Assert.AreEqual(-456, (int)listOfTokens[1].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[2].TokenType);
        }

        [Test]
        public void DifferenceIntLitsTest()
        {
            List<Token> listOfTokens = tokenizer.Tokenize("123 - 456");

            Assert.AreEqual(4, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is int);
            Assert.AreEqual(123, (int)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_Minus, listOfTokens[1].TokenType);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[2].TokenType);
            Assert.IsTrue(listOfTokens[2].TokenValue is int);
            Assert.AreEqual(456, (int)listOfTokens[2].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[3].TokenType);
        }

        [Test]
        public void PositiveFloatLitTest()
        {
            double n = 123.125;
            List<Token> listOfTokens = tokenizer.Tokenize(n.ToString());

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_FltLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is double);
            Assert.AreEqual(n, (double)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void StrLitTest()
        {
            string str = "abc";
            string quotedStr = "\"" + str + "\"";
            List<Token> listOfTokens = tokenizer.Tokenize(quotedStr);

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_StrLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual(str, (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void StrLitQuoteTest()
        {
            string str = "\"abc\"\"def\"";
            List<Token> listOfTokens = tokenizer.Tokenize(str);

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_StrLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual("abc\"def", (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void IdentTest()
        {
            string str = "abc";
            List<Token> listOfTokens = tokenizer.Tokenize(str);

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_Ident, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual(str, (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void BoolIdentTest()
        {
            string str = "abc";
            List<Token> listOfTokens = tokenizer.Tokenize("@" + str);

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_BoolIdent, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual(str, (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void VariableTest()
        {
            string str = "abc";
            List<Token> listOfTokens = tokenizer.Tokenize("?" + str);

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_Variable, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual(str, (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void ArrowTest()
        {
            List<Token> listOfTokens = tokenizer.Tokenize("->");

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_Arrow, listOfTokens[0].TokenType);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void ManIsMortalNoSpacesTest()
        {
            List<Token> listOfTokens = tokenizer.Tokenize("@isMan(?x)->@isMortal(?x)");

            Assert.AreEqual(10, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_BoolIdent, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual("isMan", (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_LeftBracket, listOfTokens[1].TokenType);

            Assert.AreEqual(TokenType.T_Variable, listOfTokens[2].TokenType);
            Assert.IsTrue(listOfTokens[2].TokenValue is string);
            Assert.AreEqual("x", (string)listOfTokens[2].TokenValue);

            Assert.AreEqual(TokenType.T_RightBracket, listOfTokens[3].TokenType);

            Assert.AreEqual(TokenType.T_Arrow, listOfTokens[4].TokenType);

            Assert.AreEqual(TokenType.T_BoolIdent, listOfTokens[5].TokenType);
            Assert.IsTrue(listOfTokens[5].TokenValue is string);
            Assert.AreEqual("isMortal", (string)listOfTokens[5].TokenValue);

            Assert.AreEqual(TokenType.T_LeftBracket, listOfTokens[6].TokenType);

            Assert.AreEqual(TokenType.T_Variable, listOfTokens[7].TokenType);
            Assert.IsTrue(listOfTokens[7].TokenValue is string);
            Assert.AreEqual("x", (string)listOfTokens[7].TokenValue);

            Assert.AreEqual(TokenType.T_RightBracket, listOfTokens[8].TokenType);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[9].TokenType);
        }

        [Test]
        public void ManIsMortalSpacesTest()
        {
            List<Token> listOfTokens = tokenizer.Tokenize("@isMan(?x) -> @isMortal(?x)");

            Assert.AreEqual(10, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_BoolIdent, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual("isMan", (string)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_LeftBracket, listOfTokens[1].TokenType);

            Assert.AreEqual(TokenType.T_Variable, listOfTokens[2].TokenType);
            Assert.IsTrue(listOfTokens[2].TokenValue is string);
            Assert.AreEqual("x", (string)listOfTokens[2].TokenValue);

            Assert.AreEqual(TokenType.T_RightBracket, listOfTokens[3].TokenType);

            Assert.AreEqual(TokenType.T_Arrow, listOfTokens[4].TokenType);

            Assert.AreEqual(TokenType.T_BoolIdent, listOfTokens[5].TokenType);
            Assert.IsTrue(listOfTokens[5].TokenValue is string);
            Assert.AreEqual("isMortal", (string)listOfTokens[5].TokenValue);

            Assert.AreEqual(TokenType.T_LeftBracket, listOfTokens[6].TokenType);

            Assert.AreEqual(TokenType.T_Variable, listOfTokens[7].TokenType);
            Assert.IsTrue(listOfTokens[7].TokenValue is string);
            Assert.AreEqual("x", (string)listOfTokens[7].TokenValue);

            Assert.AreEqual(TokenType.T_RightBracket, listOfTokens[8].TokenType);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[9].TokenType);
        }

        [Test]
        public void SkolemTest()
        {
            string str = "S1";
            List<Token> listOfTokens = tokenizer.Tokenize("$" + str);

            Assert.AreEqual(2, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_SkolemIdent, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual(str, listOfTokens[0].TokenValue as string);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[1].TokenType);
        }

        [Test]
        public void MultiLineTest1()
        {
            StringBuilder sb = new StringBuilder();

            sb.AppendLine("a || b");
            sb.AppendLine("|| c");

            List<Token> listOfTokens = tokenizer.Tokenize(sb.ToString());

            Assert.AreEqual(6, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_Ident, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual("a", listOfTokens[0].TokenValue as string);
            Assert.AreEqual(1, listOfTokens[0].Line);
            Assert.AreEqual(1, listOfTokens[0].Column);

            Assert.AreEqual(TokenType.T_2OrBar, listOfTokens[1].TokenType);
            Assert.AreEqual(3, listOfTokens[1].Column);

            Assert.AreEqual(TokenType.T_Ident, listOfTokens[2].TokenType);
            Assert.IsTrue(listOfTokens[2].TokenValue is string);
            Assert.AreEqual("b", listOfTokens[2].TokenValue as string);
            Assert.AreEqual(6, listOfTokens[2].Column);

            Assert.AreEqual(TokenType.T_2OrBar, listOfTokens[3].TokenType);
            Assert.AreEqual(2, listOfTokens[3].Line);
            Assert.AreEqual(1, listOfTokens[3].Column);

            Assert.AreEqual(TokenType.T_Ident, listOfTokens[4].TokenType);
            Assert.IsTrue(listOfTokens[4].TokenValue is string);
            Assert.AreEqual("c", listOfTokens[4].TokenValue as string);
            Assert.AreEqual(4, listOfTokens[4].Column);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[5].TokenType);
        }

        [Test]
        public void MultiLineTest2()
        {
            StringBuilder sb = new StringBuilder();

            sb.AppendLine("a || b");
            sb.AppendLine();
            sb.AppendLine("|| c");

            List<Token> listOfTokens = tokenizer.Tokenize(sb.ToString());

            Assert.AreEqual(6, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_Ident, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is string);
            Assert.AreEqual("a", listOfTokens[0].TokenValue as string);
            Assert.AreEqual(1, listOfTokens[0].Line);
            Assert.AreEqual(1, listOfTokens[0].Column);

            Assert.AreEqual(TokenType.T_2OrBar, listOfTokens[1].TokenType);

            Assert.AreEqual(TokenType.T_Ident, listOfTokens[2].TokenType);
            Assert.IsTrue(listOfTokens[2].TokenValue is string);
            Assert.AreEqual("b", listOfTokens[2].TokenValue as string);

            Assert.AreEqual(TokenType.T_2OrBar, listOfTokens[3].TokenType);
            Assert.AreEqual(3, listOfTokens[3].Line);
            Assert.AreEqual(1, listOfTokens[3].Column);

            Assert.AreEqual(TokenType.T_Ident, listOfTokens[4].TokenType);
            Assert.IsTrue(listOfTokens[4].TokenValue is string);
            Assert.AreEqual("c", listOfTokens[4].TokenValue as string);
            Assert.AreEqual(4, listOfTokens[4].Column);

            Assert.AreEqual(TokenType.T_EOF, listOfTokens[5].TokenType);
        }

        [Test]
        public void TokenizeErrorTest1()
        {
            Assert.Throws<TokenizerException>(() => tokenizer.Tokenize("a & b"));
        }

        [Test]
        public void TokenizeErrorTest2()
        {
            Assert.Throws<TokenizerException>(() => tokenizer.Tokenize("a &&& b"));
        }
    }
}
