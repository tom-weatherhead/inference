using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.Prolog
{
    [TestFixture]
    public class Prolog2Tokenizer_Fixture
    {
        private readonly ITokenizer tokenizer;

        public Prolog2Tokenizer_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Prolog2);
        }

        [Test]
        public void StringLiteralTest()
        {
            var intLit = 123;
            var strLit = "This is text!";
            var listOfTokens = tokenizer.Tokenize(string.Format("{0} '{1}' \"{2}\".", intLit, strLit, strLit));

            Assert.AreEqual(5, listOfTokens.Count);

            Assert.AreEqual(TokenType.T_IntLit, listOfTokens[0].TokenType);
            Assert.IsTrue(listOfTokens[0].TokenValue is int);
            Assert.AreEqual(intLit, (int)listOfTokens[0].TokenValue);

            Assert.AreEqual(TokenType.T_StrLit2, listOfTokens[1].TokenType);
            Assert.IsTrue(listOfTokens[1].TokenValue is string);
            Assert.AreEqual(strLit, (string)listOfTokens[1].TokenValue);

            Assert.AreEqual(TokenType.T_StrLit, listOfTokens[2].TokenType);
            Assert.IsTrue(listOfTokens[2].TokenValue is string);
            Assert.AreEqual(strLit, (string)listOfTokens[2].TokenValue);

            Assert.AreEqual(TokenType.T_Dot, listOfTokens[3].TokenType);
            Assert.AreEqual(TokenType.T_EOF, listOfTokens[4].TokenType);
        }
    }
}
