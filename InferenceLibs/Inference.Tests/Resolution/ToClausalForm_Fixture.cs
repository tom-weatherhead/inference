using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Domain;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Resolution
{
    [TestFixture]
    public class ToClausalForm_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public ToClausalForm_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Inference);
        }

        private List<string> ClauseListToStringList(List<Clause> lc)
        {
            List<string> result = new List<string>();

            foreach (var clause in lc)
            {
                result.Add(clause.ToString());
            }

            return result;
        }

        [Test]
        public void ComplexTest1()
        {
            string strInput = "(@isMan(?x) -> @isMortal(?x)) && @isMan(Socrates)";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(2, results.Count);
            Assert.IsTrue(results.Contains("!@isMan(?x) || @isMortal(?x)"));
            Assert.IsTrue(results.Contains("@isMan(Socrates)"));
        }

        [Test]
        public void DeMorganizeTest1()
        {
            string strInput = "!(@F(?x) && @G(?x))";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results.Contains("!@F(?x) || !@G(?x)"));
        }

        [Test]
        public void DeMorganizeTest2()
        {
            string strInput = "!(@F(?x) || @G(?x))";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(2, results.Count);
            Assert.IsTrue(results.Contains("!@F(?x)"));
            Assert.IsTrue(results.Contains("!@G(?x)"));
        }

        [Test]
        public void GrandparentTest()
        {
            string strInput = "(@isParentOf(?x, ?y) && @isParentOf(?y, ?z)) -> @isGrandparentOf(?x, ?z)";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results.Contains("!@isParentOf(?x, ?y) || !@isParentOf(?y, ?z) || @isGrandparentOf(?x, ?z)"));
        }

        [Test]
        public void ForAllTest()
        {
            string strInput = "A ?x (@pred(?x))";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results.Contains("@pred(?x)"));
        }

        [Test]
        public void ExistsTest()
        {
            string strInput = "E ?x (@pred(?x))";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results.Contains("@pred($S1())"));
        }

        [Test]
        public void MixedQuantifiersTest()
        {
            string strInput = "A ?x (E ?y (@pred(?x, ?y)))";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results.Contains("@pred(?x, $S1(?x))"));
        }

        [Test]
        public void NegatedForAllTest()
        {
            string strInput = "!(A ?x (@pred(?x)))";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results.Contains("!@pred($S1())"));
        }

        [Test]
        public void NegatedExistsTest()
        {
            string strInput = "!(E ?x (@pred(?x)))";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<string> results = ClauseListToStringList(Clause.ConvertBooleanExpressionToClausalForm(expr));

            Assert.AreEqual(1, results.Count);
            Assert.IsTrue(results.Contains("!@pred(?x)"));
        }
    }
}
