using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Parser
{
    class BadGrammarBase : GrammarBase
    {
        public BadGrammarBase()
            : base(Symbol.N_Start)
        {
            Terminals.Add(Symbol.T_ID);
            Terminals.Add(Symbol.T_EOF);

            NonTerminals.Add(Symbol.N_Start);
            NonTerminals.Add(Symbol.N_Expression);
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            throw new NotImplementedException("BadGrammarBase.ExecuteSemanticAction()");
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            throw new NotImplementedException("BadGrammarBase.PushTokenOntoSemanticStack()");
        }
    }

    class BadGrammar1 : BadGrammarBase
    {
        public BadGrammar1()
        {
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_ID, Symbol.T_EOF }, 1));
        }
    }

    class BadGrammar2 : BadGrammarBase
    {
        public BadGrammar2()
        {
            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.T_ID }, 1));
        }
    }

    class BadGrammar3 : BadGrammarBase
    {
        public BadGrammar3()
        {
            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.T_ID, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Expression, Symbol.T_EOF }, 2));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_ID }, 3));
        }
    }

    [TestFixture]
    public class Grammar_Fixture
    {
        [Test]
        public void NoStartingProductionTest()
        {
            var grammar = new BadGrammar1();

            Assert.Throws<GrammarException>(() => grammar.FindStartingProduction());
        }

        [Test]
        public void NoStartingProductionWithEOFTest()
        {
            var grammar = new BadGrammar2();

            Assert.Throws<GrammarException>(() => grammar.FindStartingProduction());
        }

        [Test]
        public void MultipleStartingProductionsTest()
        {
            var grammar = new BadGrammar3();

            Assert.Throws<GrammarException>(() => grammar.FindStartingProduction());
        }
    }
}
