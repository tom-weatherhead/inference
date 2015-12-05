using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Domain;
using Inference.Parser;
using Inference.Tests.Utility;
using NUnit.Framework;

namespace Inference.Tests.Resolution
{
    [TestFixture]
    public class KnowledgeBase_Fixture : TestFixtureWithSampleDataBase
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public KnowledgeBase_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.LALR1, GrammarSelector.Inference);
        }

        [SetUp]
        public void SetupTest()
        {
            InitData();
        }

        private Clause StringToClause(string strInput)  // Duplicated from Clause_Fixture
        {
            var expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            var clauses = Clause.ConvertBooleanExpressionToClausalForm(expr);

            Assert.AreEqual(1, clauses.Count);

            return clauses[0];
        }

        [Test]
        public void LoadTest()
        {
            knowledgeBase.Load();

            Assert.AreEqual(1, knowledgeBase.ClauseDict.Count);
            Assert.AreEqual(0, knowledgeBase.clausesNotSavedToDatabase.Count);
            Assert.IsTrue(knowledgeBase.ContainsEquivalent(StringToClause("@isMan(?x) -> @isMortal(?x)")));
        }

        [Test]
        public void SaveUnsavedClauseTest()
        {
            var clauseAsString = "@unsavedClauseTestPredicate()";
            var clause = StringToClause(clauseAsString);

            knowledgeBase.Load();
            knowledgeBase.AddClausesAndResolve(clauseAsString, false);

            Assert.AreEqual(2, knowledgeBase.ClauseDict.Count);
            Assert.AreEqual(1, knowledgeBase.clausesNotSavedToDatabase.Count);
            Assert.IsTrue(knowledgeBase.ContainsEquivalent(clause));

            knowledgeBase.SaveAllUnsavedClauses();

            Assert.AreEqual(2, knowledgeBase.ClauseDict.Count);
            Assert.AreEqual(0, knowledgeBase.clausesNotSavedToDatabase.Count);
            Assert.IsTrue(knowledgeBase.ContainsEquivalent(clause));

            knowledgeBase.Load();

            Assert.AreEqual(2, knowledgeBase.ClauseDict.Count);
            Assert.AreEqual(0, knowledgeBase.clausesNotSavedToDatabase.Count);
            Assert.IsTrue(knowledgeBase.ContainsEquivalent(clause));
        }

        [Test]
        public void DontSaveUnsavedClauseTest()
        {
            var clauseAsString = "@unsavedClauseTestPredicate()";
            var clause = StringToClause(clauseAsString);

            knowledgeBase.Load();
            knowledgeBase.AddClausesAndResolve(clauseAsString, false);

            Assert.AreEqual(2, knowledgeBase.ClauseDict.Count);
            Assert.AreEqual(1, knowledgeBase.clausesNotSavedToDatabase.Count);
            Assert.IsTrue(knowledgeBase.ContainsEquivalent(clause));

            knowledgeBase.Load();

            Assert.AreEqual(1, knowledgeBase.ClauseDict.Count);
            Assert.AreEqual(0, knowledgeBase.clausesNotSavedToDatabase.Count);
            Assert.IsFalse(knowledgeBase.ContainsEquivalent(clause));
        }

        [Test]
        public void SocratesIsMortalTest()
        {
            //string strInput1 = "@isMan(?x) -> @isMortal(?x)";    // Already in the knowledge base
            var clause = StringToClause("@isMortal(Socrates)");

            knowledgeBase.AddClausesAndResolve("@isMan(Socrates)", true);

            //Assert.AreEqual(3, knowledgeBase.ClauseDict.Count);
            Assert.IsTrue(knowledgeBase.ContainsEquivalent(clause));
        }

        [Test]
        public void GrandparentTest1()
        {
            knowledgeBase.AddClausesAndResolve("(@isParentOf(?x, ?y) && @isParentOf(?y, ?z)) -> @isGrandparentOf(?x, ?z)", true);
            knowledgeBase.AddClausesAndResolve("@isParentOf(a, b) && @isParentOf(b, c)", true);

            Assert.IsTrue(knowledgeBase.ContainsEquivalent(StringToClause("@isGrandparentOf(a, c)")));
        }

        [Test]
        public void GrandparentTest2()
        {
            var saveToDatabase = true;

            knowledgeBase.AddClausesAndResolve("@isParentOf(a, b)", saveToDatabase);
            knowledgeBase.AddClausesAndResolve("@isParentOf(b, c)", saveToDatabase);
            knowledgeBase.AddClausesAndResolve("(@isParentOf(?x, ?y) && @isParentOf(?y, ?z)) -> @isGrandparentOf(?x, ?z)", saveToDatabase);

            Assert.IsTrue(knowledgeBase.ContainsEquivalent(StringToClause("@isGrandparentOf(a, c)")));
        }
    }
}
