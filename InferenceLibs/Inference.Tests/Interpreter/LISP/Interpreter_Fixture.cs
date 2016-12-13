using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;
using NUnit.Framework;

using Path = System.IO.Path;
using IInterpreterUnitTestInterface = Inference.Interpreter.IInterpreterUnitTestInterface;
using LISPInterpreter = Inference.Interpreter.LISP.LISPInterpreter;

namespace Inference.Tests.Interpreter.LISP
{
    [TestFixture]
    public class Interpreter_Fixture
    {
        private readonly IInterpreterUnitTestInterface interpreter = new LISPInterpreter();

        private string LoadFile(string filename)
        {
            //return interpreter.ReadLineForTest("load " + Path.Combine(interpreter.DefaultDirectoryFromTests, filename));
            return interpreter.LoadFileUsingCompletedPath(Path.Combine(interpreter.DefaultDirectoryFromTests, filename));
        }

        [SetUp]
        public void SetUp()
        {
            interpreter.ReadLineForTest("clear");
        }

        [Test]
        public void AdditionTest()
        {
            Assert.AreEqual("5", interpreter.ReadLineForTest("(+ 2 3)"));
        }

        [Test]
        public void MultiLineQuoteAndBracketTest()
        {
            Assert.AreEqual("quote", interpreter.ReadLines(new List<string>() {
                "(set x '",
                "quote)" }));
            Assert.AreEqual("(sin cos tan)", interpreter.ReadLines(new List<string>() {
                "(set y '(",
                "sin cos tan))" }));
        }

        [Test]
        public void LoadFileTest()
        {
            Assert.AreEqual("(7 5 3 2)", LoadFile("ReverseList.txt"));
        }
    }
}
