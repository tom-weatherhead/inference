using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;
using NUnit.Framework;

using Path = System.IO.Path;
using IInterpreterUnitTestInterface = Inference.Interpreter.IInterpreterUnitTestInterface;
using PrologGlobalInfo = Inference.Interpreter.Prolog.PrologGlobalInfo;
using PrologInterpreter = Inference.Interpreter.Prolog.PrologInterpreter;
using GrammarSelector = Inference.Parser.GrammarSelector;

namespace Inference.Tests.Interpreter.Prolog
{
    [TestFixture]
    public class Prolog2Interpreter_Fixture
    {
        private const string clauseAdded = PrologGlobalInfo.ClauseAdded;
        private const string satisfied = PrologGlobalInfo.Satisfied;
        private const string notSatisfied = PrologGlobalInfo.NotSatisfied;
        private readonly IInterpreterUnitTestInterface interpreter = new PrologInterpreter(GrammarSelector.Prolog2, true);

        private string LoadFile(string filename)
        {
            //return interpreter.ReadLineForTest("load " + Path.Combine(interpreter.DefaultDirectoryFromTests, filename));
            //return interpreter.globalInfo.LoadFile(filename);
            return interpreter.LoadFileUsingCompletedPath(Path.Combine(interpreter.DefaultDirectoryFromTests, filename));
        }

        [SetUp]
        public void SetUp()
        {
            interpreter.ReadLineForTest("clear");
        }

        [Test]
        public void EnglishDCGTest() // Test our Definite Clause Grammar of a subset of the English language.
        {
            LoadFile("EnglishDCG.txt");

            // Use the English parser in recognizer mode:
            Assert.AreEqual(satisfied, interpreter.ReadLineForTest("?- s(_, [the, woman, loves, a, man], [])."));
            Assert.AreEqual(satisfied, interpreter.ReadLineForTest("?- s(_, [she, loves, a, man], [])."));
            Assert.AreEqual(notSatisfied, interpreter.ReadLineForTest("?- s(_, [her, loves, a, man], [])."));
        }

        [Test]
        public void FileReaderTest() // 2014/04/22.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse54
        {
            Assert.AreEqual(clauseAdded, interpreter.ReadLineForTest(@"
    main:-
         open('houses.txt',read,Str),
         read_houses(Str,Houses),
         close(Str),
         write(Houses),  nl."));
            Assert.AreEqual(clauseAdded, interpreter.ReadLineForTest(@"
   read_houses(Stream,[]):-
         at_end_of_stream(Stream)."));
            Assert.AreEqual(clauseAdded, interpreter.ReadLineForTest(@"
   read_houses(Stream,[X|L]):-
         \+  at_end_of_stream(Stream),
         read(Stream,X),
         read_houses(Stream,L)."));

            Assert.AreEqual("[gryffindor, hufflepuff, ravenclaw, slytherin]\r\n\r\n" + satisfied, interpreter.ReadLineForTest("?- main."));
        }

        [Test]
        public void FileWriterTest() // 2014/04/22
        {
            Assert.AreEqual(clauseAdded, interpreter.ReadLineForTest(@"
    main:-
         write_atom(write, 'abc.'),
         write_atom(write, 'def.'),
         write_atom(append, 'ghi.'),
         open('temp.txt',read,Str),
         read_atoms(Str,Atoms),
         close(Str),
         write(Atoms),
         nl."));
            Assert.AreEqual(clauseAdded, interpreter.ReadLineForTest(@"
   write_atom(Mode, Atom) :-
         open('temp.txt',Mode,Stream),
         write(Stream, Atom),
         nl(Stream),
         close(Stream)."));
            Assert.AreEqual(clauseAdded, interpreter.ReadLineForTest(@"
   read_atoms(Stream,[]):-
         at_end_of_stream(Stream)."));
            Assert.AreEqual(clauseAdded, interpreter.ReadLineForTest(@"
   read_atoms(Stream,[X|L]):-
         \+  at_end_of_stream(Stream),
         read(Stream,X),
         read_atoms(Stream,L)."));

            Assert.AreEqual("[def, ghi]\r\n\r\n" + satisfied, interpreter.ReadLineForTest("?- main."));
        }

        [Test]
        public void ModuleTest() // 2014/05/06.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse52
        {
            LoadFile("ModuleTest.pl");
            Assert.AreEqual(clauseAdded, interpreter.ReadLineForTest("bap."));

            Assert.AreEqual(notSatisfied, interpreter.ReadLineForTest("?- bat(X)."));               // The module does not export bat/1.
            Assert.AreEqual("X = 7\r\n" + satisfied, interpreter.ReadLineForTest("?- bar(X)."));    // The module exports bar/1.
            Assert.AreEqual("X = 7\r\n" + satisfied, interpreter.ReadLineForTest("?- bar(X), bap."));
        }

        [Test]
        public void ModuleChainTest() // 2014/05/07.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse52
        {
            LoadFile("ModuleTest2.pl");

            Assert.AreEqual(notSatisfied, interpreter.ReadLineForTest("?- bat(X)."));
            Assert.AreEqual(notSatisfied, interpreter.ReadLineForTest("?- bar(X)."));
            Assert.AreEqual("X = 7\r\n" + satisfied, interpreter.ReadLineForTest("?- blah(X)."));
        }

        [Test]
        public void SubdirectoryTest() // 2014/05/08.
        {
            LoadFile(@"TestSubdirectory\TestFile.txt");

            Assert.AreEqual(satisfied, interpreter.ReadLineForTest("?- subdirectoryTestSuccess."));
        }
    }
}
