using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Parser
{
    #region Grammar1

    class Grammar1 : GrammarBase
    {
        // The "G1" grammar from Chapter 6 of Fischer and LeBlanc

        public Grammar1()
            : base(Symbol.N_Start)
        {
            Terminals.Add(Symbol.T_LeftBracket);
            Terminals.Add(Symbol.T_RightBracket);
            Terminals.Add(Symbol.T_ID);
            Terminals.Add(Symbol.T_Plus);
            Terminals.Add(Symbol.T_EOF);

            NonTerminals.Add(Symbol.N_Start);
            NonTerminals.Add(Symbol.N_Expression);
            NonTerminals.Add(Symbol.N_Primary);     // Use N_Primary in place of the non-terminal T.

            // See Fischer and LeBlanc, page 152
            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Expression, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Expression, Symbol.T_Plus, Symbol.N_Primary }, 2));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Primary }, 3));
            Productions.Add(new Production(Symbol.N_Primary, new List<object>() { Symbol.T_ID }, 4));
            Productions.Add(new Production(Symbol.N_Primary, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.T_RightBracket }, 5));
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            throw new NotImplementedException("Grammar1.ExecuteSemanticAction()");
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            throw new NotImplementedException("Grammar1.PushTokenOntoSemanticStack()");
        }
    }

    #endregion

    #region LR0Parser_Fixture

    [TestFixture]
    public class LR0Parser_Fixture
    {
        private readonly ITokenizer tokenizer;
        private IParser parser;     // Cannot be readonly - some of the tests replace the parser.
        private readonly IParser parserGrammar1;

        public LR0Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parserGrammar1 = ParserFactory.Create(ParserSelector.LR0, new Grammar1());
        }

        [SetUp]
        public void SetUp()
        {
            parser = parserGrammar1;
        }

        [Test]
        public void RecognizeTest1()
        {
            parser.Recognize(tokenizer.Tokenize("a"));
        }

        [Test]
        public void RecognizeTest2()
        {
            parser.Recognize(tokenizer.Tokenize("(a + b) + (c + d)"));
        }

        [Test]
        public void RecognizeErrorTest1()
        {
            Assert.Throws<SyntaxException>(() => parser.Recognize(tokenizer.Tokenize("a +")));
        }

        [Test]
        public void RecognizeErrorTest2()
        {
            Assert.Throws<SyntaxException>(() => parser.Recognize(tokenizer.Tokenize("a b")));
        }

        [Test]
        public void MicroTest()
        {
            parser = ParserFactory.Create(ParserSelector.LR0, GrammarSelector.Micro);

            Assert.Throws<ShiftReduceConflictException>(() => parser.Recognize(tokenizer.Tokenize("begin abc := def + 123; i := i - 1; end")));
        }

        [Test]
        public void InferenceTest()
        {
            parser = ParserFactory.Create(ParserSelector.LR0, GrammarSelector.Inference);

            Assert.Throws<ShiftReduceConflictException>(() => parser.Recognize(tokenizer.Tokenize("@isMan(?x) -> @isMortal(?x)")));
        }
    }

    #endregion
}
