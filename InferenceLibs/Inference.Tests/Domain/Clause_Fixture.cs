using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Domain;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Domain
{
    [TestFixture]
    public class Clause_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public Clause_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Inference);
        }

        private Clause StringToClause(string strInput)
        {
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<Clause> clauses = Clause.ConvertBooleanExpressionToClausalForm(expr);

            Assert.AreEqual(1, clauses.Count);

            return clauses[0];
        }

        [Test]
        public void ContradictionTest1()
        {
            string strInput = "@isMan(Socrates) && !@isMan(Socrates)";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<Clause> srcClauses = Clause.ConvertBooleanExpressionToClausalForm(expr);

            Assert.AreEqual(2, srcClauses.Count);

            List<Clause> results = srcClauses[0].Resolve(srcClauses[1]);

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results[0].IsContradiction());
        }

        [Test]
        public void ContradictionTest2()
        {
            string strInput = "@isMan(?x) -> @isMortal(?x)";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<Clause> results = Clause.ConvertBooleanExpressionToClausalForm(expr);

            Assert.AreEqual(1, results.Count);
            Assert.IsFalse(results[0].IsContradiction());
        }

        [Test]
        public void EquivalenceTest1()
        {
            Clause clause = StringToClause("@isMan(?x) -> @isMortal(?x)");

            Assert.IsTrue(clause.IsEquivalentTo(clause));
        }

        [Test]
        public void EquivalenceTest2()
        {
            Clause clause1 = StringToClause("@isMan(?x) -> @isMortal(?x)");
            Clause clause2 = StringToClause("@isMan(?y) -> @isMortal(?y)");

            Assert.IsTrue(clause1.IsEquivalentTo(clause2));
            Assert.IsTrue(clause2.IsEquivalentTo(clause1));
        }

        [Test]
        public void EquivalenceTest3()
        {
            Clause clause1 = StringToClause("!@isMan(?x) || @isMortal(?x)");
            Clause clause2 = StringToClause("@isMortal(?y) || !@isMan(?y)");

            Assert.IsTrue(clause1.IsEquivalentTo(clause2));
            Assert.IsTrue(clause2.IsEquivalentTo(clause1));
        }

        [Test]
        public void EquivalenceTest4()
        {
            Clause clause1 = StringToClause("!@isMan(?x) || @isMortal(?x)");
            Clause clause2 = StringToClause("@isMortal2(?y) || !@isMan(?y)");

            Assert.IsFalse(clause1.IsEquivalentTo(clause2));
            Assert.IsFalse(clause2.IsEquivalentTo(clause1));
        }

        [Test]
        public void EquivalenceTest5()
        {
            Clause clause1 = StringToClause("@F(?x) || @G(?x)");
            Clause clause2 = StringToClause("@F(?y) || @G(?z)");

            Assert.IsFalse(clause1.IsEquivalentTo(clause2));
            Assert.IsFalse(clause2.IsEquivalentTo(clause1));
        }
    }
}
