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
    public class Resolution_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public Resolution_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Inference);
        }

        [Test]
        public void SocratesIsMortalTest1()
        {
            string strInput1 = "!@isMan(?x) || @isMortal(?x)";
            string strInput2 = "@isMan(Socrates)";
            IBooleanExpression expr1 = parser.Parse(tokenizer.Tokenize(strInput1)) as IBooleanExpression;
            IBooleanExpression expr2 = parser.Parse(tokenizer.Tokenize(strInput2)) as IBooleanExpression;

            Assert.IsNotNull(expr1);
            Assert.IsNotNull(expr2);

            List<Clause> clauseList1 = Clause.ConvertBooleanExpressionToClausalForm(expr1);
            List<Clause> clauseList2 = Clause.ConvertBooleanExpressionToClausalForm(expr2);

            Assert.AreEqual(1, clauseList1.Count);
            Assert.AreEqual(1, clauseList2.Count);

            List<Clause> resolutionResult = clauseList1[0].Resolve(clauseList2[0]);

            Assert.AreEqual(1, resolutionResult.Count);
            Assert.AreEqual("@isMortal(Socrates)", resolutionResult[0].ToString());
        }

        [Test]
        public void SocratesIsMortalTest2()
        {
            string strInput1 = "@isMan(?x) -> @isMortal(?x)";
            string strInput2 = "@isMan(Socrates)";
            IBooleanExpression expr1 = parser.Parse(tokenizer.Tokenize(strInput1)) as IBooleanExpression;
            IBooleanExpression expr2 = parser.Parse(tokenizer.Tokenize(strInput2)) as IBooleanExpression;

            Assert.IsNotNull(expr1);
            Assert.IsNotNull(expr2);

            List<Clause> clauseList1 = Clause.ConvertBooleanExpressionToClausalForm(expr1);
            List<Clause> clauseList2 = Clause.ConvertBooleanExpressionToClausalForm(expr2);

            Assert.AreEqual(1, clauseList1.Count);
            Assert.AreEqual(1, clauseList2.Count);

            List<Clause> resolutionResult = clauseList1[0].Resolve(clauseList2[0]);

            Assert.AreEqual(1, resolutionResult.Count);
            Assert.AreEqual("@isMortal(Socrates)", resolutionResult[0].ToString());
        }

        [Test]
        public void SocratesIsMortalTest3()
        {
            string strInput = "(@isMan(?x) -> @isMortal(?x)) && @isMan(Socrates)";
            IBooleanExpression expr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(expr);

            List<Clause> clauseList = Clause.ConvertBooleanExpressionToClausalForm(expr);

            Assert.AreEqual(2, clauseList.Count);

            List<Clause> resolutionResult = clauseList[0].Resolve(clauseList[1]);

            Assert.AreEqual(1, resolutionResult.Count);
            Assert.AreEqual("@isMortal(Socrates)", resolutionResult[0].ToString());
        }

        [Test]
        public void VariableRenamingTest()
        {
            string strInput1 = "@F(?w) || @G(?x)";
            string strInput2 = "!@F(?y) || @H(?x)";
            IBooleanExpression expr1 = parser.Parse(tokenizer.Tokenize(strInput1)) as IBooleanExpression;
            IBooleanExpression expr2 = parser.Parse(tokenizer.Tokenize(strInput2)) as IBooleanExpression;

            Assert.IsNotNull(expr1);
            Assert.IsNotNull(expr2);

            List<Clause> clauseList1 = Clause.ConvertBooleanExpressionToClausalForm(expr1);
            List<Clause> clauseList2 = Clause.ConvertBooleanExpressionToClausalForm(expr2);

            Assert.AreEqual(1, clauseList1.Count);
            Assert.AreEqual(1, clauseList2.Count);

            List<Clause> resolutionResult = clauseList1[0].Resolve(clauseList2[0]);

            Assert.AreEqual(1, resolutionResult.Count);

            HashSet<Variable> variables3 = resolutionResult[0].FindVariables(VariableSearchType.All);

            Assert.AreEqual(2, variables3.Count);
            Assert.IsTrue(variables3.Contains(new Variable("x")));
            Assert.IsTrue(variables3.Contains(new Variable("var1")));
        }

        [Test]
        public void SkolemFunctionRenamingTest()
        {
            string strInput1 = "@F(?w) || E ?x (@G(?x))";
            string strInput2 = "!@F(?y) || E ?z (@H(?z))";
            IBooleanExpression expr1 = parser.Parse(tokenizer.Tokenize(strInput1)) as IBooleanExpression;
            IBooleanExpression expr2 = parser.Parse(tokenizer.Tokenize(strInput2)) as IBooleanExpression;

            Assert.IsNotNull(expr1);
            Assert.IsNotNull(expr2);

            List<Clause> clauseList1 = Clause.ConvertBooleanExpressionToClausalForm(expr1);
            List<Clause> clauseList2 = Clause.ConvertBooleanExpressionToClausalForm(expr2);

            Assert.AreEqual(1, clauseList1.Count);
            Assert.AreEqual(1, clauseList2.Count);

            HashSet<string> skolemFuncNames1 = clauseList1[0].FindAllSkolemFunctionNames();
            HashSet<string> skolemFuncNames2 = clauseList2[0].FindAllSkolemFunctionNames();

            Assert.AreEqual(1, skolemFuncNames1.Count);
            Assert.AreEqual(1, skolemFuncNames2.Count);
            Assert.IsTrue(skolemFuncNames1.Contains("S1"));
            Assert.IsTrue(skolemFuncNames2.Contains("S1"));

            List<Clause> resolutionResult = clauseList1[0].Resolve(clauseList2[0]);

            Assert.AreEqual(1, resolutionResult.Count);

            HashSet<string> skolemFuncNames3 = resolutionResult[0].FindAllSkolemFunctionNames();

            Assert.AreEqual(2, skolemFuncNames3.Count);
            Assert.IsTrue(skolemFuncNames3.Contains("S1"));
            Assert.IsTrue(skolemFuncNames3.Contains("S2"));
        }
    }
}
