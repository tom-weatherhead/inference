﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NHibernateHelperLib.Persistence;
using Inference.AStar;
using Inference.Domain;
using Inference.Expert;
using Inference.Interpreter;
using Inference.Interpreter.LISP;
using Inference.Parser;
using Inference.Persistence;
using Inference.Resolution;
using Inference.Utilities;

namespace InferenceConsoleApp
{
    class Program
    {
        static void Test1()
        {
            //string str = "abc ?var 123";
            //string str = "abc=123/-64";
            //string str = "abc=123&/-64";
            //string str = "begin abc := def + 123; i := i - 1; end";
            string str = "@isMan(?x) -> @isMortal(?x)";
            //string str = "!@isMan(?x) || @isMortal(?x)";
            //string str = "!@isMan(Socrates)";
            string str2 = "@isMan(Socrates)";
            List<Token> listOfTokens = TokenizerFactory.Create(GrammarSelector.Inference).Tokenize(str);

            /*
            Console.WriteLine("The string to tokenize is '{0}'", str);

            foreach (Token token in listOfTokens)
            {
                Console.WriteLine("Column {0}: Token {1} value {2}", token.Column, token.TokenType, token.TokenValue);
            }
             */

            //LL1Parser parser = new LL1Parser(GrammarSelector.Micro);
            IParser parser = new LL1Parser(GrammarSelector.Inference);

            Console.WriteLine("The parser was successfully initialized.");

            object resultOfParse = parser.Parse(listOfTokens);
            Clause clause1 = null;
            /*
            Disjunction disj1 = resultOfParse as Disjunction;

            listOfTokens = Tokenizer.Tokenize(str2);
            //parser = new LL1Parser(GrammarSelector.Inference);
            resultOfParse = parser.Parse(listOfTokens);

            Disjunction disj2 = resultOfParse as Disjunction;
             */

            Console.WriteLine("The list of tokens was successfully recognized by the parser.");
            Console.WriteLine("This is the object that was generated by the parser: " + resultOfParse.ToString());

            if (resultOfParse.ToString() == str)
            {
                Console.WriteLine("The string version of the output is identical to the input string.");
            }

            if (resultOfParse is IBooleanExpression)
            {
                var expr = resultOfParse as IBooleanExpression;
                var clauses = expr.ToClausalForm();

                Console.WriteLine("The clausal form of {0} :", resultOfParse.ToString());

                foreach (var clause in clauses)
                {
                    Console.WriteLine("  " + clause.ToString());
                }

                if (clauses.Count == 1)
                {
                    clause1 = clauses[0];
                }
            }

            Clause clause2 = null;

            listOfTokens = TokenizerFactory.Create(GrammarSelector.Inference).Tokenize(str2);
            //parser = new LL1Parser(GrammarSelector.Inference);
            resultOfParse = parser.Parse(listOfTokens);

            if (resultOfParse is IBooleanExpression)
            {
                var expr = resultOfParse as IBooleanExpression;
                var clauses = expr.ToClausalForm();

                Console.WriteLine("The clausal form of {0} :", resultOfParse.ToString());

                foreach (var clause in clauses)
                {
                    Console.WriteLine("  " + clause.ToString());
                }

                if (clauses.Count == 1)
                {
                    clause2 = clauses[0];
                }
            }

            /*
            Console.WriteLine("Disjunction 1: " + disj1.ToString());
            Console.WriteLine("Disjunction 2: " + disj2.ToString());

            Console.WriteLine("Disj 1 is equivalent to Disj 1: " + disj1.IsEquivalent(disj1).ToString());
            Console.WriteLine("Disj 1 is equivalent to Disj 2: " + disj1.IsEquivalent(disj2).ToString());
             */

            if (clause1 != null && clause2 != null)
            {
                //List<Clause> resultOfResolution = clause1.Resolve(clause2);
                List<Clause> resultOfResolution = clause2.Resolve(clause1);

                Console.WriteLine("Result of resolution (count {0}) :", resultOfResolution.Count);

                foreach (var d in resultOfResolution)
                {
                    string strResult = d.ToString();

                    if (string.IsNullOrEmpty(strResult))
                    {
                        Console.WriteLine("  Empty string");
                    }
                    else
                    {
                        Console.WriteLine("  " + strResult);
                    }
                }
            }
        }

        static void Test2()
        {
            string str = "@isMan(?x) -> @isMortal(?x)";
            string str2 = "@isMan(Socrates)";
            KnowledgeBase kb = new KnowledgeBase();

            kb.AddClausesAndResolve(str, false);
            kb.AddClausesAndResolve(str2, false);
            kb.PrintAll();
        }

        static void Test3()
        {
            bool saveToDatabase = true;
            KnowledgeBase knowledgeBase = new KnowledgeBase();

            knowledgeBase.AddClausesAndResolve("@isParentOf(a, b)", saveToDatabase);
            knowledgeBase.AddClausesAndResolve("@isParentOf(b, c)", saveToDatabase);
            knowledgeBase.AddClausesAndResolve("(@isParentOf(?x, ?y) && @isParentOf(?y, ?z)) -> @isGrandparentOf(?x, ?z)", saveToDatabase);

            //Assert.IsTrue(knowledgeBase.ContainsEquivalent(StringToClause("@isGrandparentOf(a, c)")));

            knowledgeBase.PrintAll();
        }

        static void Test4Decant()
        {
            var ssg = new DecanterSuccessorStateGenerator(7, 11);
            var algorithm = HeuristicSearchAlgorithmFactory.Create<DecanterState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var startState = new DecanterState(0, 0);
            var goalState = new DecanterState(0, 5);

            algorithm.SearchAndReport(startState, goalState);
        }

        static void Test5CanMiss()
        {
            var ssg = new CanMissSuccessorStateGenerator();
            //var algorithm = HeuristicSearchAlgorithmFactory.Create<CanMissState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var algorithm = HeuristicSearchAlgorithmFactory.Create<CanMissState>(HeuristicSearchAlgorithmType.IterativeDeepeningAStar, ssg);
            var startState = CanMissState.CalculateStartState(4, 5);    //new CanMissState(true, 4, 5, 0, 0);
            var goalState = startState.CalculateGoalState();            //new CanMissState(false, 0, 0, 4, 5);

            algorithm.SearchAndReport(startState, goalState);
        }

        static void Test6LR1Init()
        {
            IGrammar grammar = GrammarFactory.Create(GrammarSelector.Micro);
            IParser parser = new LR1Parser(grammar);

            Console.WriteLine("LR(1) parser initialized.");
        }

        static void Test7LR1RecognizeManIsMortal()
        {
            string strInput = "@isMan(?x) -> @isMortal(?x)";
            List<Token> listOfTokens = TokenizerFactory.Create(GrammarSelector.Inference).Tokenize(strInput);
            IGrammar grammar = GrammarFactory.Create(GrammarSelector.Inference);
            IParser parser = new LR1Parser(grammar);

            parser.Recognize(listOfTokens);
            Console.WriteLine("LR(1) parser recognized that man is mortal.");
        }

        static void Test8ExpertSystem()
        {
            //ModifiedAStarAlgorithm expertSystem = new ModifiedAStarAlgorithm(DomainSelector.JackAutomotive);
            ModifiedAStarAlgorithm expertSystem = new ModifiedAStarAlgorithm(DomainSelector.JackComputer);

            expertSystem.SearchAndReport();
        }

        static void Test9LR0Init()
        {
            IGrammar grammar = GrammarFactory.Create(GrammarSelector.Micro);
            IParser parser = new LR0Parser(grammar);

            Console.WriteLine("LR(0) parser initialized.");
        }

        static void Test10LALR1Init()
        {
            IParser parser = new LALR1Parser(GrammarSelector.Inference);

            Console.WriteLine("LALR(1) parser initialized.");
        }

        static void Test11EightPuzzle()
        {
            var ssg = new EightPuzzleSuccessorStateGenerator();
            var algorithm = HeuristicSearchAlgorithmFactory.Create<EightPuzzleState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var startState = EightPuzzleState.CreateRandom();
            var goalState = EightPuzzleState.CreateGoal();

            algorithm.SearchAndReport(startState, goalState);
        }

        static void Test12EightPuzzleAlgorithmComparison()
        {
            var ssg = new EightPuzzleSuccessorStateGenerator();
            var algorithmAStar = HeuristicSearchAlgorithmFactory.Create<EightPuzzleState>(HeuristicSearchAlgorithmType.AStar, ssg);
            var algorithmIDAStar = HeuristicSearchAlgorithmFactory.Create<EightPuzzleState>(HeuristicSearchAlgorithmType.IterativeDeepeningAStar, ssg);
            var startState = new EightPuzzleState(new List<int>() { 0, 6, 2, 5, 1, 7, 8, 3, 4 });
            var goalState = EightPuzzleState.CreateGoal();

            Console.WriteLine();
            Console.WriteLine("A*");
            algorithmAStar.SearchAndReport(startState, goalState);
            Console.WriteLine();
            Console.WriteLine("Iterative-Deepening-A*");
            algorithmIDAStar.SearchAndReport(startState, goalState);
            Console.WriteLine();
        }

        static ISExpression EvaluateLISPExpression(string input, ITokenizer tokenizer, IParser parser, IGlobalInfo<ISExpression> globalInfo)
        {
            var expr = parser.Parse(tokenizer.Tokenize(input)) as IExpression<ISExpression>;

            return expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);
        }

        static void Test13LISPVariableUsage()
        {
            Console.WriteLine("Test13LISPVariableUsage");

            var tokenizer = TokenizerFactory.Create(GrammarSelector.LISP);
            var parser = ParserFactory.Create(ParserSelector.LALR1, GrammarSelector.LISP);
            //var localEnvironment = new EnvironmentFrame<ISExpression>(null);
            var input = "(+ n 1)";
            var parseResult = parser.Parse(tokenizer.Tokenize(input));

            Console.WriteLine("Output: " + parseResult.ToString());
        }

        static void Test14LISP_GCD()
        {
            Console.WriteLine("Test13LISPVariableUsage");

            var tokenizer = TokenizerFactory.Create(GrammarSelector.LISP);
            var parser = ParserFactory.Create(ParserSelector.LALR1, GrammarSelector.LISP);
            var globalInfo = new LISPGlobalInfo(tokenizer, parser);

            EvaluateLISPExpression("(define mod (m n) (- m (* n (/ m n))))", tokenizer, parser, globalInfo);
            EvaluateLISPExpression("(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))", tokenizer, parser, globalInfo);

            var sexpr = EvaluateLISPExpression("(gcd 343 91)", tokenizer, parser, globalInfo);

            Console.WriteLine("Output: " + sexpr.ToString());
        }

        static void Test15FormatDoublesForAPL()
        {
            var doubles = new List<double>() { 13.0, 12345.0, 1.03125, 12345.03125 };

            Console.WriteLine(string.Join(" ", doubles.Select(d => d == Math.Floor(d) ? d.ToString("0.0") : d.ToString())));
        }

#if DEAD_CODE
        static void Test16BTreeInsertAscending()
        {
            var btree = new BTree<int>();
            var sorted = new List<int>();

            for (var i = 1; i <= 8; ++i)
            {
                sorted.Add(i);
                btree.Insert(i);

                //Assert.AreEqual(string.Join(" ", sorted), string.Join(" ", btree.InOrderTraversal()));
                Console.WriteLine("Traversal for i == {0}: {1}", i, string.Join(" ", btree.InOrderTraversal()));
            }
        }
#endif

        static void Test17RedBlackTreeInsertAscending()
        {
            var rbtree = new RedBlackTree<int, string>();

            rbtree.Add(1, "1");
            rbtree.Add(2, "2");
            rbtree.Add(3, "3");
        }

        static void Test18CloneStack() // 2014/05/03.
        {
            Console.WriteLine("Test18CloneStack");

            var stack = new Stack<int>();

            stack.Push(2);
            stack.Push(3);
            stack.Push(5);
            stack.Push(7);

            var list = new List<int>();
            //var stack2 = new Stack<int>();

            foreach (var i in stack)
            {
                list.Add(i);
                //stack2.Push(i);
            }

            list.Reverse();

            var stack2 = new Stack<int>(list);

            while (stack2.Count > 0)
            {
                Console.WriteLine(stack2.Pop());
            }
        }

        static void Main(string[] args)
        {
            var quiet = args.Length > 1;    // True iff we are invoking an interpreter on a particular file.

            if (!quiet)
            {
                Console.WriteLine("InferenceConsoleApp: Begin");
            }

            try
            {

                if (args == null || args.Length == 0)
                {
                    Test18CloneStack();
                }
                else
                {
                    Inference.Interpreter.IInterpreter interpreter = null;

                    switch (args[0].ToLower())
                    {
                        case "create":
                            Console.WriteLine("Creating database...");
                            NHibernateHelper.CreateDatabase();
                            Console.WriteLine("Database created.");
                            break;

                        case "drop":
                            Console.WriteLine("Dropping database...");
                            NHibernateHelper.DropDatabase();
                            Console.WriteLine("Database dropped.");
                            break;

                        case "update":
                            Console.WriteLine("Updating database...");
                            NHibernateHelper.UpdateDatabase();
                            Console.WriteLine("Database updated.");
                            break;

                        // TODO 2013/10/18 : Pass quiet to each interpreter constructor.
                        case "lisp":
                            interpreter = new Inference.Interpreter.LISP.LISPInterpreter(quiet);
                            break;

                        case "apl":
                            interpreter = new Inference.Interpreter.APL.APLInterpreter();
                            break;

                        case "scheme":
                            interpreter = new Inference.Interpreter.Scheme.SchemeInterpreter(quiet);
                            break;

                        case "sasl":
                            interpreter = new Inference.Interpreter.SASL.SASLInterpreter();
                            break;

                        case "clu":
                            interpreter = new Inference.Interpreter.CLU.CLUInterpreter();
                            break;

                        case "smalltalk":
                            interpreter = new Inference.Interpreter.Smalltalk.SmalltalkInterpreter();
                            break;

                        case "prolog":
                            interpreter = new Inference.Interpreter.Prolog.PrologInterpreter(GrammarSelector.Prolog);
                            break;

                        case "prolog2":
                            interpreter = new Inference.Interpreter.Prolog.PrologInterpreter(GrammarSelector.Prolog2);
                            break;

                        default:
                            Console.WriteLine("Unrecognized argument '{0}'", args[0]);
                            break;
                    }

                    if (interpreter != null)
                    {

                        if (args.Length > 1)
                        {
                            // TODO: for (var i = 1; i < args.Length; ++i) // Load all files that are listed on the command line.
                            // TODO 2013/10/18 : Pass quiet to interpreter.LoadFile()
                            interpreter.LoadFile(args[1]);
                        }
                        else
                        {
                            interpreter.ReadEvalPrintLoop();
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("{0} caught: {1}", ex.GetType().FullName, ex.Message);

                for (Exception ex2 = ex.InnerException; ex2 != null; ex2 = ex2.InnerException)
                {
                    Console.WriteLine("  {0}: {1}", ex2.GetType().FullName, ex2.Message);
                }
            }

            if (!quiet)
            {
                Console.WriteLine("InferenceConsoleApp: End");
            }
        }
    }
}