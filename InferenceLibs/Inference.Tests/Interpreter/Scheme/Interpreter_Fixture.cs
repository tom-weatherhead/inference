using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;
using NUnit.Framework;

//using Path = System.IO.Path;
using IInterpreterUnitTestInterface = Inference.Interpreter.IInterpreterUnitTestInterface;
using SchemeInterpreter = Inference.Interpreter.Scheme.SchemeInterpreter;

namespace Inference.Tests.Interpreter.Scheme
{
    [TestFixture]
    public class Interpreter_Fixture
    {
        private readonly IInterpreterUnitTestInterface interpreter = new SchemeInterpreter();

        /*
        private string LoadFile(string filename)
        {
            return interpreter.ReadLineForTest("load " + Path.Combine(interpreter.DefaultDirectoryFromTests, filename));
        }
         */

        [SetUp]
        public void SetUp()
        {
            interpreter.ReadLineForTest("clear");
        }

        [Test]
        public void AdditionTest()
        {
            Assert.AreEqual("+", interpreter.ReadLineForTest("+"));
            Assert.AreEqual("T", interpreter.ReadLineForTest("(primop? +)"));
            Assert.AreEqual("5", interpreter.ReadLineForTest("(+ 2 3)"));
        }
    }
}
