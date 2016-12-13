using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.Prolog;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.Prolog
{
    [TestFixture]
    public class Parser_Fixture
    {
        private const string clauseAdded = PrologGlobalInfo.ClauseAdded;
        private const string satisfied = PrologGlobalInfo.Satisfied;
        private const string notSatisfied = PrologGlobalInfo.NotSatisfied;
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly PrologGlobalInfo globalInfo;

        public Parser_Fixture()
        {
            var gs = GrammarSelector.Prolog;

            tokenizer = TokenizerFactory.Create(gs);
            parser = ParserFactory.Create(ParserSelector.SLR1, gs);
            globalInfo = new PrologGlobalInfo(gs, tokenizer, parser);
        }

        [SetUp]
        public void SetUpTest()
        {
            globalInfo.Clear();
        }

        [Test]
        public void RecognizeTest()
        {
            parser.Recognize(tokenizer.Tokenize("(infer (member X (cons Y M)) from (member X M))"));
        }

        [Test]
        public void NoArgumentsTest()
        {
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer pred1)"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (pred2))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (pred3 cons))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (pred4 (cons)))"));

            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? pred1)"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (pred1))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? pred2)"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (pred2))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? predUndefined)"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (pred1 nil))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (pred2 nil))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (pred3 cons))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (pred3 (cons)))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (pred4 cons))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (pred4 (cons)))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (pred3 cons2))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (pred3 (cons nil)))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (pred4 cons2))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (pred4 (cons nil)))"));
        }

        [Test]
        public void ListMemberTest()    // From Kamin, page 354
        {
            globalInfo.LoadPreset("member");

            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (member 3 (cons 2 (cons 3 nil))))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (member 3 (cons 2 (cons 4 nil))))"));
        }

        [Test]
        public void PlusTest()
        {
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (plus 2 3 5))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (plus 1 1 0))"));
            Assert.AreEqual("X = 5\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (plus 2 3 X) (print X))"));
        }

        [Test]
        public void MinusTest()
        {
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (minus 12 3 9))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (minus 8 13 21))"));
            Assert.AreEqual("X = 9\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (minus 12 3 X) (print X))"));
        }

        [Test]
        public void LessTest()
        {
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (less 2 3))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (less 3 2))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (less 2 2))"));
        }

        [Test]
        public void NotEqualTest()
        {
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal X X))"));
#if DEAD_CODE
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal X Y))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal X 1))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal 1 X))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal X red))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal red X))"));
#else
            // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse37
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal X Y))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal X 1))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal 1 X))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal X red))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal red X))"));
#endif
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal 1 1))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal red red))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? not-equal)"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal red))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-equal red green blue))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal 0 1))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal red green))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal 1 red))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-equal red 1))"));
        }

        [Test]
        public void Map3ColouringTest()     // The three-colour problem - From Kamin, page 353
        {
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different yellow blue))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different yellow red))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different blue yellow))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different blue red))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different red yellow))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different red blue))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (mapb-colouring A B C D E F)
                from (different A B) (different A C) (different A D) (different A F)
                     (different B C) (different B E) (different C E) (different C D)
                     (different D E) (different E F))"));

            Assert.IsTrue(globalInfo.ProcessInputString("(infer? (mapb-colouring A B C D E F) (print A B C D E F))").EndsWith("\r\n" + satisfied));
        }

        [Test]
        public void Map3ColouringTest2()     // The three-colour problem - From Kamin, page 353 - Using the not-equal predicate (see Exercise 9)
        {
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (colour yellow))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (colour blue))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (colour red))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (mapb-colouring A B C D E F)
                from (colour A) (colour B) (colour C) (colour D) (colour E) (colour F)
                     (not-equal A B) (not-equal A C) (not-equal A D) (not-equal A F)
                     (not-equal B C) (not-equal B E) (not-equal C E) (not-equal C D)
                     (not-equal D E) (not-equal E F))"));

            Assert.IsTrue(globalInfo.ProcessInputString("(infer? (mapb-colouring A B C D E F) (print A B C D E F))").EndsWith("\r\n" + satisfied));
        }

        [Test]
        public void AddToEndOfListTest()     // From Kamin, page 366
        {
            // (addtoend L X M) means that M is the list obtained by adding X to the end of L.
            globalInfo.LoadPreset("addtoend");

            // Forwards inference:
            Assert.AreEqual("L = (cons 3 (cons 4 nil))\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (addtoend (cons 3 nil) 4 L) (print L))"));

            // Backwards inference:
            Assert.AreEqual("L = (cons 3 nil)\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (addtoend L 4 (cons 3 (cons 4 nil))) (print L))"));
        }

        [Test]
        public void ReverseListTest()     // From Kamin, page 366-367
        {
            globalInfo.LoadPreset("addtoend");

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (reverse nil nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (reverse (cons X L) M) from (reverse L N) (addtoend N X M))"));

            Assert.AreEqual("L = (cons 2 (cons 1 nil))\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (reverse (cons 1 (cons 2 nil)) L) (print L))"));
            Assert.AreEqual("L = (cons 2 (cons 1 nil))\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (reverse L (cons 1 (cons 2 nil))) (print L))"));
        }

        [Test]
        public void AppendListTest()     // From Kamin, page 367
        {
            // (append L M N) means that N is the list obtained by appending M onto the end of L.
            globalInfo.LoadPreset("append");

            Assert.AreEqual("L = nil\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (append nil nil L) (print L))"));
            Assert.AreEqual("L = (cons 1 nil)\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (append (cons 1 nil) nil L) (print L))"));
            Assert.AreEqual("L = (cons 1 nil)\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (append nil (cons 1 nil) L) (print L))"));
            Assert.AreEqual("L = (cons 1 (cons 2 nil))\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (append (cons 1 nil) (cons 2 nil) L) (print L))"));
            Assert.AreEqual("L = (cons 3 (cons 4 (cons 5 (cons 6 nil))))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (append (cons 3 (cons 4 nil)) (cons 5 (cons 6 nil)) L) (print L))"));
            Assert.AreEqual("L = (cons 5 nil)\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (append L (cons 6 (cons 7 nil)) (cons 5 (cons 6 (cons 7 nil)))) (print L))"));
        }

        [Test]
        public void MultiplicationTest()     // From Kamin, page 368
        {
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (mult 0 Y 0))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (mult X Y Z) from (less 0 X) (minus X 1 V) (mult V Y W) (plus W Y Z))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (mult X Y Z) from (less X 0) (plus X 1 V) (mult V Y W) (minus W Y Z))"));

            Assert.AreEqual("X = 12\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (mult 3 4 X) (print X))"));
            Assert.AreEqual("X = -12\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (mult -3 4 X) (print X))"));
            Assert.AreEqual("X = -12\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (mult 3 -4 X) (print X))"));
            Assert.AreEqual("X = 12\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (mult -3 -4 X) (print X))"));

            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (mult X 4 12))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (mult 3 X 12))"));   // See page 369 for an explanation.
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (mult X -4 12))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (mult -3 X 12))"));
        }

        [Test]
        public void FactorialTest()     // From Kamin, page 369
        {
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (mult 0 Y 0))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (mult X Y Z) from (minus X 1 V) (mult V Y W) (plus W Y Z))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (fac 0 1))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (fac N R) from (minus N 1 N1) (fac N1 R1) (mult R1 N R))"));

            Assert.AreEqual("X = 120\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (fac 5 X) (print X))"));
        }

        [Test]
        public void NaiveSortTest()     // From Kamin, page 370
        {
            globalInfo.LoadPreset("append");
            globalInfo.LoadPreset("<=");

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (ordered nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (ordered (cons A nil)))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (ordered (cons A (cons B L))) from (<= A B) (ordered (cons B L)))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (permutation nil nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (permutation L (cons H T))
                from (append V (cons H U) L) (append V U W) (permutation W T))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (naive-sort L M) from (permutation L M) (ordered M))"));

            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (permutation (cons 2 nil) (cons 2 nil)))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (permutation (cons 2 (cons 3 nil)) (cons 2 (cons 3 nil))))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (permutation (cons 2 (cons 3 nil)) (cons 3 (cons 2 nil))))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (permutation (cons 4 (cons 2 (cons 3 nil))) (cons 2 (cons 3 (cons 4 nil)))))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (ordered (cons 2 (cons 3 (cons 4 nil)))))"));
            // How do we get naive-sort to generate all possible permutations (or even more than one permutation)?  Via non-buggy backtracking (2012/12/03).
            Assert.AreEqual("L = (cons 2 (cons 3 nil))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (naive-sort (cons 3 (cons 2 nil)) L) (print L))"));
            Assert.AreEqual("L = (cons 2 (cons 3 (cons 4 nil)))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (naive-sort (cons 4 (cons 2 (cons 3 nil))) L) (print L))"));
            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (naive-sort (cons 4 (cons 2 (cons 3 nil))) (cons 2 (cons 3 (cons 4 nil)))))"));
        }

        [Test]
        public void QuicksortTest()     // From Kamin, page 371
        {
            globalInfo.LoadPreset("append");
            globalInfo.LoadPreset("<=");

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (partition H (cons A X) (cons A Y) Z) from (<= A H) (partition H X Y Z))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (partition H (cons A X) Y (cons A Z)) from (less H A) (partition H X Y Z))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (partition H nil nil nil))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (quicksort nil nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
(infer (quicksort (cons H T) S)
    from
        (partition H T A B)
        (quicksort A A1)
        (quicksort B B1)
        (append A1 (cons H B1) S))"));

            Assert.AreEqual("S = (cons 1 (cons 2 nil))\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (quicksort (cons 2 (cons 1 nil)) S) (print S))"));
            Assert.AreEqual("S = (cons 1 (cons 2 (cons 3 nil)))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (quicksort (cons 2 (cons 3 (cons 1 nil))) S) (print S))"));
            Assert.AreEqual("S = (cons 1 (cons 2 (cons 3 (cons 7 (cons 8 nil)))))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (quicksort (cons 8 (cons 2 (cons 3 (cons 7 (cons 1 nil))))) S) (print S))"));
        }

#if !DEAD_CODE // 2014/04/22 : The addition of stream reading and writing broke this test: it hung.  (Fixed; the function ProveGoalList was becoming too large.)
        [Test]
        public void BlocksTest()     // From Kamin, pages 382-386
        {
            globalInfo.LoadPreset("member");

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (block a))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (block b))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (block c))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different a b))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different a c))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different b a))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different b c))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different c a))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different c b))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different X table) from (block X))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (different table Y) from (block Y))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (clear X nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (clear X (cons (on B Y) State)) from (different X Y) (clear X State))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (on X Y State) from (member (on X Y) State))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (update (move X Y Z) (cons (on X Y) S) (cons (on X Z) S)))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (update (move X Y Z) (cons (on U V) S1) (cons (on U V) S2))
                from (different X U) (update (move X Y Z) S1 S2))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (legal-move (move B P1 table) State)
                from
                    (on B P1 State)
                    (different P1 table)
                    (clear B State))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (legal-move (move B1 P B2) State)
                from
                    (block B2)
                    (on B1 P State)
                    (different P B2)
                    (different B1 B2)
                    (clear B1 State)
                    (clear B2 State))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (different (cons (on A X) State1) (cons (on A Y) State2))
                from (different X Y))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (different (cons (on A X) State1) (cons (on A X) State2))
                from (different State1 State2))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (not-member X nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (not-member X (cons Y L))
                from (different X Y) (not-member X L))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (transform State1 State2 Plan)
                from (transform State1 State2 (cons State1 nil) Plan))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (transform State State Visited nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (transform State1 State2 Visited (cons Move Moves))
                from
                    (choose-move Move State1 State2)
                    (update Move State1 State)
                    (not-member State Visited)
                    (transform State State2 (cons State Visited) Moves))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (choose-move Move State1 State2)
                from (suggest Move State2) (legal-move Move State1))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (choose-move Move State1 State2)
                from (legal-move Move State1))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (suggest (move X Y Z) State)
                from (member (on X Z) State))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (state1 (cons (on a b) (cons (on b table) (cons (on c a) nil)))))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (state2 (cons (on a b) (cons (on b c) (cons (on c table) nil)))))"));

            Assert.AreEqual("Plan = (cons (move c a table) (cons (move a b table) (cons (move b table c) (cons (move a table b) nil))))\r\n" + satisfied,
                globalInfo.ProcessInputString(@"(infer? (state1 S1) (state2 S2) (transform S1 S2 Plan) (print Plan))"));
        }
#endif

        [Test]
        public void BlocksTest2()     // From Kamin, pages 382-386 - using the not-equal predicate (see Exercise 9)
        {
            globalInfo.LoadPreset("member");

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (block a))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (block b))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (block c))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (clear X nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (clear X (cons (on B Y) State)) from (not-equal X Y) (clear X State))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (on X Y State) from (member (on X Y) State))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (update (move X Y Z) (cons (on X Y) S) (cons (on X Z) S)))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (update (move X Y Z) (cons (on U V) S1) (cons (on U V) S2))
                from (not-equal X U) (update (move X Y Z) S1 S2))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (legal-move (move B P1 table) State)
                from
                    (on B P1 State)
                    (not-equal P1 table)
                    (clear B State))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (legal-move (move B1 P B2) State)
                from
                    (block B2)
                    (on B1 P State)
                    (not-equal P B2)
                    (not-equal B1 B2)
                    (clear B1 State)
                    (clear B2 State))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (different (cons (on A X) State1) (cons (on A Y) State2))
                from (not-equal X Y))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (different (cons (on A X) State1) (cons (on A X) State2))
                from (different State1 State2))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (not-member X nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (not-member X (cons Y L))
                from (different X Y) (not-member X L))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (transform State1 State2 Plan)
                from (transform State1 State2 (cons State1 nil) Plan))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (transform State State Visited nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (transform State1 State2 Visited (cons Move Moves))
                from
                    (choose-move Move State1 State2)
                    (update Move State1 State)
                    (not-member State Visited)
                    (transform State State2 (cons State Visited) Moves))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (choose-move Move State1 State2)
                from (suggest Move State2) (legal-move Move State1))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (choose-move Move State1 State2)
                from (legal-move Move State1))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (suggest (move X Y Z) State)
                from (member (on X Z) State))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (state1 (cons (on a b) (cons (on b table) (cons (on c a) nil)))))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"(infer (state2 (cons (on a b) (cons (on b c) (cons (on c table) nil)))))"));
            Assert.AreEqual("Plan = (cons (move c a table) (cons (move a b table) (cons (move b table c) (cons (move a table b) nil))))\r\n" + satisfied,
                globalInfo.ProcessInputString(@"(infer? (state1 S1) (state2 S2) (transform S1 S2 Plan) (print Plan))"));
        }

        // (length L N) is satisfied iff N is the length of list L.

        [Test]
        public void ListLengthTest()     // From Kamin, page 403: Exercise 2a
        {
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (length nil 0))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (length (cons X L) N) from (length L P) (plus P 1 N))"));

            Assert.AreEqual("N = 4\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (length (cons 2 (cons 3 (cons 5 (cons 7 nil)))) N) (print N))"));
        }

        // (remove X L M) is satisfied iff the removal from list L of all elements matching X yields the list M.

        [Test]
        public void ListRemoveTest()     // From Kamin, page 403: Exercise 2b
        {
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (remove X nil nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (remove X (cons X L) M) from (remove X L M))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (remove X (cons Y L) (cons Y M)) from (remove X L M))"));

            Assert.AreEqual("L = nil\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (remove 1 (cons 1 nil) L) (print L))"));
            Assert.AreEqual("L = (cons 2 (cons 3 nil))\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (remove 1 (cons 2 (cons 3 nil)) L) (print L))"));
            Assert.AreEqual("L = (cons 2 (cons 3 (cons 4 nil)))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (remove 1 (cons 1 (cons 2 (cons 3 (cons 1 (cons 4 nil))))) L) (print L))"));
        }

        [Test]
        public void InsertionSortTest()     // From Kamin, page 404: Exercise 2c
        {
            globalInfo.LoadPreset("<=");

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (insert-into-sorted-list X nil (cons X nil)))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (insert-into-sorted-list X (cons Y L) (cons X (cons Y L))) from (<= X Y))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (insert-into-sorted-list X (cons Y L) (cons Y M)) from (less Y X) (insert-into-sorted-list X L M))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (insert-sort nil nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (insert-sort (cons X L) M) from (insert-sort L N) (insert-into-sorted-list X N M))"));

            Assert.AreEqual("S = (cons 1 (cons 2 (cons 3 (cons 7 (cons 8 nil)))))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (insert-sort (cons 8 (cons 2 (cons 3 (cons 7 (cons 1 nil))))) S) (print S))"));
            Assert.AreEqual("S = (cons 1 (cons 3 (cons 3 (cons 7 (cons 8 nil)))))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (insert-sort (cons 8 (cons 3 (cons 1 (cons 7 (cons 3 nil))))) S) (print S))"));
        }

        [Test]
        public void FlattenTest()     // From Kamin, page 404: Exercise 2d
        {
            globalInfo.LoadPreset("append");

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (flatten nil nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (flatten (cons (cons X L) M) N) from (flatten (cons X L) P) (flatten M Q) (append P Q N))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (flatten (cons nil L) M) from (flatten L M))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (flatten (cons X L) (cons X M)) from (flatten L M))"));

            Assert.AreEqual("F = (cons 1 (cons 2 (cons 3 (cons 4 nil))))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (flatten (cons (cons 1 (cons 2 (cons nil nil))) (cons (cons 3 nil) (cons 4 nil))) F) (print F))"));
        }

        [Test]
        public void DifferenceListsTest()     // From Kamin, pages 371-372, and Exercise 6 on page 404
        {
            // simplify
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (simplify (diff X X) nil))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (simplify (diff (cons X Y) Z) (cons X W)) from (simplify (diff Y Z) W))"));

            Assert.AreEqual("L = (cons 3 (cons 4 nil))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (simplify (diff (cons 3 (cons 4 X)) X) L) (print L))"));

            // diffappend
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (diffappend (diff L X) (diff X Y) (diff L Y)))"));

            Assert.AreEqual("W = (cons 3 (cons 4 nil))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (diffappend (diff (cons 3 X) X) (diff (cons 4 Y) Y) Z) (simplify Z W) (print W))"));

            // Exercise 6a) : diffaddtoend
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
(infer (diffaddtoend (diff L X) Y Z)
    from
        (diffappend (diff L X) (diff (cons Y W) W) Z))"));

            Assert.AreEqual("Z = (cons 1 (cons 2 (cons 3 (cons 4 nil))))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (simplify X (cons 1 (cons 2 (cons 3 nil)))) (diffaddtoend X 4 Y) (simplify Y Z) (print Z))"));

            // Exercise 6b) : diffreverse
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (diffreverse (diff X X) (diff Y Y)))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
(infer (diffreverse (diff (cons X L) Y) Z)
    from
        (diffreverse (diff L Y) W)
        (diffaddtoend W X Z))"));

            Assert.AreEqual("Z = (cons 4 (cons 3 (cons 2 (cons 1 nil))))\r\n" + satisfied,
                globalInfo.ProcessInputString("(infer? (simplify X (cons 1 (cons 2 (cons 3 (cons 4 nil))))) (diffreverse X Y) (simplify Y Z) (print Z))"));

            // Exercise 6c) : diffquicksort
            globalInfo.LoadPreset("<=");

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (diffpartition H (diff X X) (diff Y Y) (diff Z Z)))"));  // This one must be first.
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
(infer (diffpartition H (diff (cons A X) X1) (diff (cons A Y) Y1) (diff Z Z1))
    from
        (<= A H)
        (diffpartition H (diff X X1) (diff Y Y1) (diff Z Z1)))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
(infer (diffpartition H (diff (cons A X) X1) (diff Y Y1) (diff (cons A Z) Z1))
    from
        (less H A)
        (diffpartition H (diff X X1) (diff Y Y1) (diff Z Z1)))"));

            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (diffquicksort (diff X X) (diff Y Y)))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
(infer (diffquicksort (diff (cons H T) T1) S)
    from
        (diffpartition H (diff T T1) A B)
        (diffquicksort A A1)
        (diffquicksort B B1)
        (diffaddtoend A1 H C)
        (diffappend C B1 S))"));

            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (diffpartition 2 (diff X X) T V))").EndsWith(satisfied));
            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (diffpartition 2 (diff (cons 2 (cons 1 X)) X) T V))").EndsWith(satisfied));
            Assert.AreEqual("U = (cons 2 (cons 1 nil)), W = nil\r\n" + satisfied, globalInfo.ProcessInputString(@"
(infer?
    (simplify S (cons 2 (cons 1 nil)))
    (diffpartition 2 S T V)
    (simplify T U)
    (simplify V W)
    (print U W))"));
            Assert.AreEqual("U = (cons 1 (cons 2 nil))\r\n" + satisfied, globalInfo.ProcessInputString(@"
(infer?
    (simplify S (cons 2 (cons 1 nil)))
    (diffquicksort S T)
    (simplify T U)
    (print U))"));
            Assert.AreEqual("U = (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))\r\n" + satisfied, globalInfo.ProcessInputString(@"
(infer?
    (simplify S (cons 3 (cons 5 (cons 2 (cons 1 (cons 4 nil))))))
    (diffquicksort S T)
    (simplify T U)
    (print U))"));
        }

        [Test]
        public void SuccZeroIntTest()     // From Kamin, Exercise 8 on page 404
        {
            // to-succ-format
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (to-succ-format 0 zero))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
(infer (to-succ-format M (succ X))
    from
        (less 0 M) ; To prevent infinite backtracking
        (minus M 1 N)
        (to-succ-format N X))"));

            Assert.AreEqual("X = zero\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (to-succ-format 0 X) (print X))"));
            Assert.AreEqual("X = (succ zero)\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (to-succ-format 1 X) (print X))"));
            Assert.AreEqual("X = (succ (succ zero))\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (to-succ-format 2 X) (print X))"));

            // to-int
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (to-int zero 0))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
(infer (to-int (succ X) N)
    from
        (to-int X M)
        (plus M 1 N))"));

            Assert.AreEqual("X = 0\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (to-int zero X) (print X))"));
            Assert.AreEqual("X = 1\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (to-int (succ zero) X) (print X))"));
            Assert.AreEqual("X = 2\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (to-int (succ (succ zero)) X) (print X))"));

            // print-int
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (print-int X) from (to-int X Y) (print Y))"));

            Assert.AreEqual("Y = 0\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (print-int zero))"));
            Assert.AreEqual("Y = 1\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (print-int (succ zero)))"));
            Assert.AreEqual("Y = 2\r\n" + satisfied, globalInfo.ProcessInputString("(infer? (print-int (succ (succ zero))))"));

            // equals
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (equals zero zero))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (equals (succ X) (succ Y)) from (equals X Y))"));

            Assert.IsTrue(globalInfo.ProcessInputString("(infer? (to-succ-format 3 X) (to-succ-format 3 Y) (equals X Y))").EndsWith(satisfied));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (to-succ-format 0 X) (to-succ-format 1 Y) (equals X Y))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (to-succ-format 2 X) (to-succ-format 3 Y) (equals X Y))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (to-succ-format 3 X) (to-succ-format 2 Y) (equals X Y))"));

            // +
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (+ zero X X))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (+ (succ X) Y (succ Z)) from (+ X Y Z))"));

            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (to-succ-format 2 X)
    (to-succ-format 3 Y)
    (+ X Y Z)
    (print-int Z))").EndsWith(" = 5\r\n" + satisfied));

            // -
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (- zero X zero))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (- X zero X))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (- (succ X) (succ Y) Z) from (- X Y Z))"));

            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (to-succ-format 5 X)
    (to-succ-format 2 Y)
    (- X Y Z)
    (print-int Z))").EndsWith(" = 3\r\n" + satisfied));
            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (to-succ-format 2 X)
    (to-succ-format 5 Y)
    (- X Y Z)
    (print-int Z))").EndsWith(" = 0\r\n" + satisfied));

            // <
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (lessThan zero (succ X)))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (lessThan (succ X) (succ Y)) from (lessThan X Y))"));

            // *
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (* zero Y zero))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (* X Y Z) from (lessThan zero X) (- X (succ zero) V) (* V Y W) (+ W Y Z))"));

            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (to-succ-format 3 X)
    (to-succ-format 4 Y)
    (* X Y Z)
    (print-int Z))").EndsWith(" = 12\r\n" + satisfied));

            // /
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (/ X Y zero) from (lessThan X Y))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (/ X Y (succ Z)) from (- X Y W) (/ W Y Z))"));

            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (to-succ-format 11 X)
    (to-succ-format 4 Y)
    (/ X Y Z)
    (print-int Z))").EndsWith(" = 2\r\n" + satisfied));
            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (to-succ-format 12 X)
    (to-succ-format 4 Y)
    (/ X Y Z)
    (print-int Z))").EndsWith(" = 3\r\n" + satisfied));
            Assert.IsTrue(globalInfo.ProcessInputString(@"
(infer?
    (to-succ-format 13 X)
    (to-succ-format 4 Y)
    (/ X Y Z)
    (print-int Z))").EndsWith(" = 3\r\n" + satisfied));
        }

        [Test]
        public void CutRecognitionTest()
        {
            var clause = parser.Parse(tokenizer.Tokenize("(infer !)")) as PrologClause;

            Assert.IsNotNull(clause);
            Assert.IsTrue(clause.Lhs.IsCut);
        }

        [Test]
        public void CutTest()   // From Kamin page 387.
        {
            // We call our predicate "not-eq" because we already have a built-in "not-equal".
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (eq X X))"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (not-eq X Y) from (eq X Y) ! fail)"));
            Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("(infer (not-eq X Y))"));

            Assert.AreEqual(satisfied, globalInfo.ProcessInputString("(infer? (not-eq 1 2))"));
            Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("(infer? (not-eq 2 2))"));
        }
    }
}
