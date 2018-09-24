using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
//using NHibernateHelperLib.Persistence;
using Inference.DAO;
using Inference.Domain;
using Inference.Parser;
//using Inference.Persistence;

namespace Inference.Resolution
{
    public class KnowledgeBase
    {
        public readonly Dictionary<Guid, Clause> ClauseDict = new Dictionary<Guid, Clause>();
        public readonly List<Clause> clausesNotSavedToDatabase = new List<Clause>();
        public readonly ITokenizer tokenizer;
        public readonly IParser parser;

        public KnowledgeBase()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.LALR1, GrammarSelector.Inference);
        }

        public void Clear()
        {
            ClauseDict.Clear();
            clausesNotSavedToDatabase.Clear();
        }

        public void Load()
        {
            Clear();

            /*
            var dao = new ClauseInDatabaseDAO();
            var dbRecords = dao.FindAll();

            if (dbRecords.Count == 0)
            {
                throw new Exception("KnowledgeBase.Load() : There are no clauses in the database.");
            }

            foreach (var record in dbRecords)
            {
                var strInput = record.Clause;
                var expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

                if (expr == null)
                {
                    throw new KnowledgeBaseException("KnowledgeBase.Load() : The following string does not parse to an IBooleanExpression: " + strInput);
                }

                var clauses = Clause.ConvertBooleanExpressionToClausalForm(expr);

                if (clauses.Count != 1)
                {
                    throw new KnowledgeBaseException(string.Format("KnowledgeBase.Load() : The following string converts to {0} clauses rather than one: {1}", clauses.Count, strInput));
                }

                if (ClauseDict.ContainsKey(record.Id))
                {
                    throw new KnowledgeBaseException("KnowledgeBase.Load() : Duplicate GUID detected.");
                }

                ClauseDict[record.Id] = clauses[0];
            }
             */
        }

        public void SaveAllUnsavedClauses()
        {

            if (clausesNotSavedToDatabase.Count == 0)
            {
                return;
            }

            var dao = new ClauseInDatabaseDAO();

            foreach (var c in clausesNotSavedToDatabase)
            {
                var record = new ClauseInDatabase(c.ToString());

                dao.MakePersistent(record);

                if (record.Id.Equals(Guid.Empty))
                {
                    throw new KnowledgeBaseException("KnowledgeBase.SaveAllUnsavedClauses() : The GUID of a newly added record is empty.");
                }
            }

            clausesNotSavedToDatabase.Clear();

            //NHibernateHelper.CommitTransaction();
            //NHibernateHelper.CloseSession();
        }

        public bool ContainsEquivalent(IEnumerable<Clause> collection, Clause clause)
        {
            /*
            foreach (Clause clause2 in collection)
            {

                if (clause.IsEquivalentTo(clause2))
                {
                    return true;
                }
            }

            return false;
             */
            return collection.Any(clause2 => clause.IsEquivalentTo(clause2));
        }

        public bool ContainsEquivalent(Clause clause)
        {
            return ContainsEquivalent(ClauseDict.Values, clause);
        }

        public int ResolveClause(Clause newClause, bool saveToDatabase)
        {
            var numClausesMadePersistent = 0;

            /*
            if (newClause == null)
            {
                throw new ArgumentNullException("newClause", "KnowledgeBase.AddClauseAndResolve() : newClause is null");
            }
            else if (newClause.IsContradiction())
            {
                //Console.WriteLine("KnowledgeBase.AddClauseAndResolve() : newClause is a contradiction.");
                //return;
                throw new Exception("KnowledgeBase.AddClauseAndResolve() : newClause is a contradiction.");
            }
            else if (newClause.IsTautology())
            {
                //Console.WriteLine("KnowledgeBase.AddClauseAndResolve() : newClause is a tautology: " + newClause.ToString());
                //return;
                throw new Exception("KnowledgeBase.AddClauseAndResolve() : newClause is a tautology: " + newClause.ToString());
            }
            else if (ContainsEquivalent(newClause))
            {
                Console.WriteLine("KnowledgeBase.AddClauseAndResolve() : newClause is already in the knowledge base.");
                return 0;
                //throw new Exception("KnowledgeBase.AddClauseAndResolve() : newClause is already in the knowledge base: " + newClause.ToString());
            }
             */

            var addList = new List<Clause>();

            //Console.WriteLine("KnowledgeBase.AddClauseAndResolve() : Adding input clause: " + newClause.ToString());
            //addList.Add(newClause);

            foreach (var c in ClauseDict.Values)
            {
                var addList2 = newClause.Resolve(c);

                foreach (var c2 in addList2)
                {

                    if (c2.IsContradiction())
                    {
                        throw new ContradictionException("KnowledgeBase.ResolveClause() : A contradiction was generated by resolution.");
                    }
                    else if (c2.IsTautology())
                    {
                        //Console.WriteLine("KnowledgeBase.ResolveClause() : A tautology was generated by resolution: " + c2.ToString());
                        //return;
                        throw new KnowledgeBaseException("KnowledgeBase.ResolveClause() : A tautology was generated by resolution: " + c2.ToString());
                    }
                    else if (ContainsEquivalent(c2) || ContainsEquivalent(addList, c2))
                    {
                        Console.WriteLine("KnowledgeBase.ResolveClause() : Duplicate clause generated by resolution: " + c2.ToString());
                    }
                    else
                    {
                        Console.WriteLine("KnowledgeBase.ResolveClause() : New clause generated by resolution: " + c2.ToString());
                        addList.Add(c2);
                    }
                }
            }

            if (addList.Count == 0)
            {
                Console.WriteLine("KnowledgeBase.ResolveClause() : No new clauses generated by resolution.");
                //throw new Exception("KnowledgeBase.ResolveClause() : No new clauses added or generated by resolution.");
                //return;
            }
            else if (!saveToDatabase)
            {

                foreach (var c in addList)
                {
                    ClauseDict[Guid.NewGuid()] = c;
                    clausesNotSavedToDatabase.Add(c);
                    ResolveClause(c, false);
                }
            }
            else
            {
                var dao = new ClauseInDatabaseDAO();

                foreach (var c in addList)
                {
                    var record = new ClauseInDatabase(c.ToString());

                    dao.MakePersistent(record);
                    ++numClausesMadePersistent;

                    if (record.Id.Equals(Guid.Empty))
                    {
                        throw new KnowledgeBaseException("KnowledgeBase.AddClauseAndResolve() : The GUID of a newly added record is empty.");
                    }

                    ClauseDict[record.Id] = c;
                    numClausesMadePersistent += ResolveClause(c, true);
                }

                //NHibernateHelper.CommitTransaction();
                //NHibernateHelper.CloseSession();
            }

            return numClausesMadePersistent;
        }

        public int AddClauseAndResolve(Clause newClause, bool saveToDatabase)
        {
            var numClausesMadePersistent = 0;

            if (newClause == null)
            {
                throw new ArgumentNullException("newClause", "KnowledgeBase.AddClauseAndResolve() : newClause is null");
            }
            else if (newClause.IsContradiction())
            {
                //Console.WriteLine("KnowledgeBase.AddClauseAndResolve() : newClause is a contradiction.");
                //return;
                throw new ContradictionException("KnowledgeBase.AddClauseAndResolve() : newClause is a contradiction.");
            }
            else if (newClause.IsTautology())
            {
                //Console.WriteLine("KnowledgeBase.AddClauseAndResolve() : newClause is a tautology: " + newClause.ToString());
                //return;
                throw new KnowledgeBaseException("KnowledgeBase.AddClauseAndResolve() : newClause is a tautology: " + newClause.ToString());
            }
            else if (ContainsEquivalent(newClause))
            {
                Console.WriteLine("KnowledgeBase.AddClauseAndResolve() : newClause is already in the knowledge base.");
                return 0;
                //throw new Exception("KnowledgeBase.AddClauseAndResolve() : newClause is already in the knowledge base: " + newClause.ToString());
            }

            Console.WriteLine("KnowledgeBase.AddClauseAndResolve() : Adding input clause: " + newClause.ToString());

            if (!saveToDatabase)
            {
                ClauseDict[Guid.NewGuid()] = newClause;
                clausesNotSavedToDatabase.Add(newClause);
                ResolveClause(newClause, false);
            }
            else
            {
                var dao = new ClauseInDatabaseDAO();
                var record = new ClauseInDatabase(newClause.ToString());

                dao.MakePersistent(record);
                ++numClausesMadePersistent;

                if (record.Id.Equals(Guid.Empty))
                {
                    throw new Exception("KnowledgeBase.AddClauseAndResolve() : The GUID of a newly added record is empty.");
                }

                ClauseDict[record.Id] = newClause;
                numClausesMadePersistent = ResolveClause(newClause, true) + 1;

                //NHibernateHelper.CommitTransaction();
                //NHibernateHelper.CloseSession();
            }

            return numClausesMadePersistent;
        }

        public int AddClausesAndResolve(string strInput, bool saveToDatabase)
        {
            var expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            if (expr == null)
            {
                throw new KnowledgeBaseException("KnowledgeBase.AddClauseAndResolve() : The following string does not parse to an IBooleanExpression: " + strInput);
            }

            var clauses = Clause.ConvertBooleanExpressionToClausalForm(expr);
            var sum = 0;

            foreach (var clause in clauses)
            {
                sum += AddClauseAndResolve(clause, saveToDatabase);
            }

            return sum;
        }

        public void PrintAll()
        {
            Console.WriteLine("Beginning of knowledge base dump:");

            foreach (var clause in ClauseDict.Values)
            {
                Console.WriteLine("  " + clause.ToString());
            }

            Console.WriteLine("End of knowledge base dump.");
        }
    }
}
