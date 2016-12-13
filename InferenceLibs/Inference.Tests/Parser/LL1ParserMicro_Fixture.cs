using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Parser
{
    [TestFixture]
    public class LL1ParserMicro_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public LL1ParserMicro_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Micro);
            parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Micro);
        }

        [Test]
        public void RecognizeTest1()
        {
            parser.Recognize(tokenizer.Tokenize("begin abc := def + 123; i := i - 1; end"));
        }

        [Test]
        public void RecognizeErrorTest1()
        {
            Assert.Throws<SyntaxException>(() => parser.Recognize(tokenizer.Tokenize("begin abc := def + 123; i := i - 1;")));
        }

        [Test]
        public void RecognizeErrorTest2()
        {
            Assert.Throws<SyntaxException>(() => parser.Recognize(tokenizer.Tokenize("begin abc := := def + 123; i := i - 1; end")));
        }
    }
}
