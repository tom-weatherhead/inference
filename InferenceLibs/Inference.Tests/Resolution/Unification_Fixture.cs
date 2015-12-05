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
    public class Unification_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public Unification_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Inference);
        }

        [Test]
        public void EqualConstsTest()
        {
            Constant c1 = new Constant("a");
            Constant c2 = new Constant("a");
            Substitution sub = c1.Unify(c2);

            //Assert.IsTrue(c1.Equals(c2));
            Assert.AreEqual(c1, c2);
            //Assert.IsTrue(c1.Name == c2.Name);
            Assert.IsNotNull(sub);
            Assert.AreEqual(0, sub.SubstitutionList.Count);
        }

        [Test]
        public void UnequalConstsTest()
        {
            Constant c1 = new Constant("a");
            Constant c2 = new Constant("b");
            Substitution sub = c1.Unify(c2);

            Assert.IsNull(sub);
        }

        [Test]
        public void VarConstTest()
        {
            Variable v = new Variable("x");
            Constant c = new Constant("a");
            Substitution sub = v.Unify(c);

            Assert.IsNotNull(sub);
            Assert.AreEqual(1, sub.SubstitutionList.Count);
            Assert.IsTrue(sub.SubstitutionList.ContainsKey(v));
            Assert.AreEqual(c, sub.SubstitutionList[v]);
        }

        [Test]
        public void ConstVarTest()
        {
            Constant c = new Constant("a");
            Variable v = new Variable("x");
            Substitution sub = c.Unify(v);

            Assert.IsNotNull(sub);
            Assert.AreEqual(1, sub.SubstitutionList.Count);
            Assert.IsTrue(sub.SubstitutionList.ContainsKey(v));
            Assert.AreEqual(c, sub.SubstitutionList[v]);
        }

        [Test]
        public void EqualVarsTest()
        {
            Variable v1 = new Variable("x");
            Variable v2 = new Variable("x");
            Substitution sub = v1.Unify(v2);

            Assert.IsNotNull(sub);
            Assert.AreEqual(0, sub.SubstitutionList.Count);
        }

        [Test]
        public void UnequalVarsTest()
        {
            Variable v1 = new Variable("x");
            Variable v2 = new Variable("y");
            Substitution sub = v1.Unify(v2);

            Assert.IsNotNull(sub);
            Assert.AreEqual(1, sub.SubstitutionList.Count);
            Assert.IsTrue(sub.SubstitutionList.ContainsKey(v1));
            Assert.AreEqual(v2, sub.SubstitutionList[v1]);
        }

        [Test]
        public void NotUnifiableTest1()
        {
            Variable v1 = new Variable("x");
            IUnifiable uni1 = parser.Parse(tokenizer.Tokenize("@f(?x)")) as IUnifiable;

            Assert.IsNotNull(uni1);

            Substitution sub1 = v1.Unify(uni1);

            Assert.IsNull(sub1);

            Substitution sub2 = uni1.Unify(v1);

            Assert.IsNull(sub2);
        }

        [Test]
        public void NotUnifiableTest2()
        {
            IUnifiable uni1 = parser.Parse(tokenizer.Tokenize("@f(?x, ?y)")) as IUnifiable;
            IUnifiable uni2 = parser.Parse(tokenizer.Tokenize("@f(a, g(?y))")) as IUnifiable;

            Assert.IsNotNull(uni1);
            Assert.IsNotNull(uni2);

            Substitution sub1 = uni1.Unify(uni2);

            Assert.IsNull(sub1);

            Substitution sub2 = uni2.Unify(uni1);

            Assert.IsNull(sub2);
        }

        [Test]
        public void NotUnifiableTest3()
        {
            IUnifiable uni1 = parser.Parse(tokenizer.Tokenize("@f(?x)")) as IUnifiable;
            IUnifiable uni2 = parser.Parse(tokenizer.Tokenize("@g(?y)")) as IUnifiable;

            Assert.IsNotNull(uni1);
            Assert.IsNotNull(uni2);

            Substitution sub1 = uni1.Unify(uni2);

            Assert.IsNull(sub1);

            Substitution sub2 = uni2.Unify(uni1);

            Assert.IsNull(sub2);
        }

        [Test]
        public void NotUnifiableTest4()
        {
            IUnifiable uni1 = parser.Parse(tokenizer.Tokenize("@f(?x)")) as IUnifiable;
            IUnifiable uni2 = parser.Parse(tokenizer.Tokenize("@f(?y, ?z)")) as IUnifiable;

            Assert.IsNotNull(uni1);
            Assert.IsNotNull(uni2);

            Substitution sub1 = uni1.Unify(uni2);

            Assert.IsNull(sub1);

            Substitution sub2 = uni2.Unify(uni1);

            Assert.IsNull(sub2);
        }

        [Test]
        public void NotUnifiableTest5()
        {
            IUnifiable uni1 = parser.Parse(tokenizer.Tokenize("@f(g(?x))")) as IUnifiable;
            IUnifiable uni2 = parser.Parse(tokenizer.Tokenize("@f(h(?y))")) as IUnifiable;

            Assert.IsNotNull(uni1);
            Assert.IsNotNull(uni2);

            Substitution sub1 = uni1.Unify(uni2);

            Assert.IsNull(sub1);

            Substitution sub2 = uni2.Unify(uni1);

            Assert.IsNull(sub2);
        }

        [Test]
        public void NotUnifiableTest6()
        {
            IUnifiable uni1 = parser.Parse(tokenizer.Tokenize("@f(a)")) as IUnifiable;
            IUnifiable uni2 = parser.Parse(tokenizer.Tokenize("@f(b)")) as IUnifiable;

            Assert.IsNotNull(uni1);
            Assert.IsNotNull(uni2);

            Substitution sub1 = uni1.Unify(uni2);

            Assert.IsNull(sub1);

            Substitution sub2 = uni2.Unify(uni1);

            Assert.IsNull(sub2);
        }

        [Test]
        public void NotUnifiableTest7()
        {
            IUnifiable uni1 = parser.Parse(tokenizer.Tokenize("@f(a)")) as IUnifiable;
            IUnifiable uni2 = parser.Parse(tokenizer.Tokenize("@f(g(?x))")) as IUnifiable;

            Assert.IsNotNull(uni1);
            Assert.IsNotNull(uni2);

            Substitution sub1 = uni1.Unify(uni2);

            Assert.IsNull(sub1);

            Substitution sub2 = uni2.Unify(uni1);

            Assert.IsNull(sub2);
        }
    }
}
