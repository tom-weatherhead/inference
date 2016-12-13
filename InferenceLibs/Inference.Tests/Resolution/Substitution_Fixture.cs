using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Domain;
using NUnit.Framework;

namespace Inference.Tests.Resolution
{
    [TestFixture]
    public class Substitution_Fixture
    {
        [Test]
        public void EmptySubConstTest()
        {
            Substitution sub = new Substitution();
            Constant c1 = new Constant("a");
            Constant c2 = c1.ApplySubstitution(sub) as Constant;

            Assert.IsNotNull(c2);
            Assert.AreEqual(c1, c2);
        }

        [Test]
        public void EmptySubVarTest()
        {
            Substitution sub = new Substitution();
            Variable v1 = new Variable("x");
            Variable v2 = v1.ApplySubstitution(sub) as Variable;

            Assert.IsNotNull(v2);
            Assert.AreEqual(v1, v2);
        }

        [Test]
        public void ComposeTest1()
        {
            Variable vw = new Variable("w");
            Variable vx = new Variable("x");
            Variable vy = new Variable("y");
            Variable vz = new Variable("z");
            Constant ca = new Constant("a");
            Constant cb = new Constant("b");
            Substitution sub1 = new Substitution();
            Substitution sub2 = new Substitution();

            sub1.SubstitutionList[vx] = vy;
            sub1.SubstitutionList[vz] = ca;
            sub2.SubstitutionList[vy] = vx;
            sub2.SubstitutionList[vw] = cb;

            Substitution sub3 = sub1.Compose(sub2);

            Assert.IsNotNull(sub3);
            Assert.AreEqual(3, sub3.SubstitutionList.Count);
            Assert.IsTrue(sub3.SubstitutionList.ContainsKey(vw));
            Assert.AreEqual(cb, sub3.SubstitutionList[vw]);
            Assert.IsTrue(sub3.SubstitutionList.ContainsKey(vy));
            Assert.AreEqual(vx, sub3.SubstitutionList[vy]);
            Assert.IsTrue(sub3.SubstitutionList.ContainsKey(vz));
            Assert.AreEqual(ca, sub3.SubstitutionList[vz]);
        }

        [Test]
        public void ComposeTest2()
        {
            Variable vx = new Variable("x");
            Variable vy = new Variable("y");
            Variable vz = new Variable("z");
            Substitution sub1 = new Substitution();
            Substitution sub2 = new Substitution();

            sub1.SubstitutionList[vx] = vy;
            sub2.SubstitutionList[vx] = vz;

            Substitution sub3 = sub1.Compose(sub2);

            Assert.IsNotNull(sub3);
            Assert.AreEqual(1, sub3.SubstitutionList.Count);
            Assert.IsTrue(sub3.SubstitutionList.ContainsKey(vx));
            Assert.IsTrue(sub3.SubstitutionList.ContainsValue(vy));
        }


        [Test]
        public void ComposeTest3()  // See the CS 486 course notes and refer to the fact that the substitution ?x <- ?z is excluded from sub3.
        {
            Variable vx = new Variable("x");
            Variable vy = new Variable("y");
            Variable vz = new Variable("z");
            Substitution sub1 = new Substitution();
            Substitution sub2 = new Substitution();

            sub1.SubstitutionList[vx] = vy;
            sub2.SubstitutionList[vy] = vx;
            sub2.SubstitutionList[vx] = vz;

            Substitution sub3 = sub1.Compose(sub2);

            Assert.IsNotNull(sub3);
            Assert.IsFalse(sub3.SubstitutionList.ContainsKey(vx));
            Assert.IsTrue(sub3.SubstitutionList.ContainsKey(vy));
            Assert.AreEqual(vx, sub3.SubstitutionList[vy]);
            Assert.AreEqual(1, sub3.SubstitutionList.Count);
        }
    }
}
