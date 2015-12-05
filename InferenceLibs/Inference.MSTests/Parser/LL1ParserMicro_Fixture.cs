using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Inference.Parser;

namespace Inference.MSTests.Parser
{
    [TestClass]
    public class LL1ParserMicro_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public LL1ParserMicro_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Micro);
            parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Micro);
        }

        [TestMethod]
        public void RecognizeTest1()
        {
            parser.Recognize(tokenizer.Tokenize("begin abc := def + 123; i := i - 1; end"));
        }

        [TestMethod]
        [ExpectedException(typeof(SyntaxException))]
        public void RecognizeErrorTest1()
        {
            //Assert.Throws<SyntaxException>(() => parser.Recognize(tokenizer.Tokenize("begin abc := def + 123; i := i - 1;")));
            parser.Recognize(tokenizer.Tokenize("begin abc := def + 123; i := i - 1;"));
        }

        [TestMethod]
        [ExpectedException(typeof(SyntaxException))]
        public void RecognizeErrorTest2()
        {
            //Assert.Throws<SyntaxException>(() => parser.Recognize(tokenizer.Tokenize("begin abc := := def + 123; i := i - 1; end")));
            parser.Recognize(tokenizer.Tokenize("begin abc := := def + 123; i := i - 1; end"));
        }
    }
}
