using System;
using System.Collections.Generic;
using Inference.DAO;
using Inference.Domain;
using Inference.Parser;
using Inference.Persistence;
using Inference.Resolution;
using NUnit.Framework;

namespace Inference.Tests.Utility
{
    /// <summary>No actual test, but only test data initialization.</summary>
    public abstract class TestFixtureWithSampleDataBase : TestFixtureBase
    {
        protected readonly KnowledgeBase knowledgeBase = new KnowledgeBase();

        public TestFixtureWithSampleDataBase()
        {
            Inference.Persistence.PersistenceInitializer.Init();
        }

        /// <summary>Create test data for our domain model.</summary>
        /// <throws>Exception</throws>
        protected internal virtual void InitData()
        {
            DropDatabase();
            CreateDatabase();
            knowledgeBase.Clear();

            knowledgeBase.AddClausesAndResolve("@isMan(?x) -> @isMortal(?x)", true);

            /*
            Assert.AreEqual(0, knowledgeBase.ClauseDict.Count);

            int numClausesMadePersistent = knowledgeBase.AddClausesAndResolve("@isMan(?x) -> @isMortal(?x)", true);

            Assert.AreEqual(1, numClausesMadePersistent);
             */

            /*
            // Testing: Writing
            List<Token> listOfTokens = Tokenizer.Tokenize("@isMan(?x) -> @isMortal(?x)");
            IParser parser = new LL1Parser(GrammarSelector.Inference);
            IBooleanExpression expr = parser.Parse(listOfTokens) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<Clause> clauses = Clause.ConvertBooleanExpressionToClausalForm(expr);

            Assert.AreEqual(1, clauses.Count);

            //return clauses[0];
            ClauseInDatabaseDAO dao = new ClauseInDatabaseDAO();
            ClauseInDatabase record = new ClauseInDatabase(clauses[0].ToString());

            dao.MakePersistent(record);

            NHibernateHelper.CommitTransaction();
            NHibernateHelper.CloseSession();
             */

            // Testing: Reading
            var dao2 = new ClauseInDatabaseDAO();
            var dbRecords = dao2.FindAll();

            if (dbRecords.Count == 0)
            {
                throw new Exception("TestFixtureWithSampleDataBase.InitData() : There are no clauses in the database.");
            }

            // No need to dispose of the transaction, since we didn't write to the database.
            //NHibernateHelper.CommitTransaction();
            //NHibernateHelper.CloseSession();
        }
    }
}
