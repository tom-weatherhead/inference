using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Interpreter.APL;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.APL
{
    [TestFixture]
    public class Parser_Fixture
    {
		//string lineEnd = System.Environment.NewLine;
		//private const string lineEnding = "\n";
		private string lineEnding = System.Environment.NewLine;

		private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly APLGlobalInfo globalInfo;

        public Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.APL);
            parser = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.APL);
            globalInfo = new APLGlobalInfo(tokenizer, parser);
        }

        [SetUp]
        public void SetUp()
        {
            globalInfo.Clear();
            globalInfo.LoadPresets();
        }
        private object GetParseResult(string input)
        {
            var parseResult = parser.Parse(tokenizer.Tokenize(input));

            Assert.IsNotNull(parseResult);
            //Assert.AreEqual(input, parseResult.ToString());   // This fails on multi-line or whitespace-formatted input.
            return parseResult;
        }

        private IAPLValue EvaluateToIAPLValue(string input)
        {
            var expr = GetParseResult(input) as IExpression<IAPLValue>;

            Assert.IsNotNull(expr);

            var aplExpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);

            Assert.IsNotNull(aplExpr);

            return aplExpr;
        }

        private string Evaluate(string input)
        {
            return EvaluateToIAPLValue(input).ToString();
        }

        [Test]
        public void RecognizeTest()
        {
            parser.Recognize(tokenizer.Tokenize("(define fac (n) (*/ (indx n)))"));                     // Page 70
            parser.Recognize(tokenizer.Tokenize("(define avg (v) (/ (+/ v) (shape v)))"));
            parser.Recognize(tokenizer.Tokenize("(define neg (v) (- 0 v))"));
            parser.Recognize(tokenizer.Tokenize("(define min (v1 v2) (neg (max (neg v1) (neg v2))))"));
            parser.Recognize(tokenizer.Tokenize("(define min/ (v) (neg (max/ (neg v))))"));
            parser.Recognize(tokenizer.Tokenize("(define not (x) (- 1 x))"));                           // Page 71
            parser.Recognize(tokenizer.Tokenize("(define <> (x y) (not (= x y)))"));
            parser.Recognize(tokenizer.Tokenize("(define signum (x) (+ (* (< x 0) -1) (> x 0)))"));     // Page 72
        }

        [Test]
        public void ScalarTest()
        {
            Assert.AreEqual("7", Evaluate("7"));
        }

        [Test]
        public void VectorTest()
        {
            Assert.AreEqual(string.Empty, Evaluate("'()"));
            Assert.AreEqual("2 3 5 7", Evaluate("'(2 3 5 7)"));
        }

        #region Operator Tests

        [Test]
        public void AdditionTest()
        {
            Assert.AreEqual("5", Evaluate("(+ 2 3)"));
            Assert.AreEqual("3 3 4 5", Evaluate("(+ 2 testvector1)"));
            Assert.AreEqual("4 5 7 9" + lineEnding + "13 15 19 21" + lineEnding + "25 31 33 39", Evaluate("(+ 2 testmatrix2)"));

            Assert.AreEqual("8 11 16 24", Evaluate("(+ testvector2 3)"));
            Assert.AreEqual("6 9 15 24", Evaluate("(+ testvector2 testvector1)"));

            Assert.AreEqual("6 7 8 9" + lineEnding + "10 11 12 13" + lineEnding + "14 15 16 17", Evaluate("(+ testmatrix1 5)"));
            Assert.AreEqual("3 5 8 11" + lineEnding + "16 19 24 27" + lineEnding + "32 39 42 49", Evaluate("(+ testmatrix1 testmatrix2)"));
        }

        [Test]
        public void SubtractionTest()
        {
            Assert.AreEqual("6", Evaluate("(- 13 7)"));
            Assert.AreEqual("4 4 3 2", Evaluate("(- 5 testvector1)"));
            Assert.AreEqual("48 47 45 43" + lineEnding + "39 37 33 31" + lineEnding + "27 21 19 13", Evaluate("(- 50 testmatrix2)"));

            Assert.AreEqual("2 5 10 18", Evaluate("(- testvector2 3)"));
            Assert.AreEqual("4 7 11 18", Evaluate("(- testvector2 testvector1)"));

            Assert.AreEqual("-3 -2 0 2" + lineEnding + "6 8 12 14" + lineEnding + "18 24 26 32", Evaluate("(- testmatrix2 5)"));
            Assert.AreEqual("1 1 2 3" + lineEnding + "6 7 10 11" + lineEnding + "14 19 20 25", Evaluate("(- testmatrix2 testmatrix1)"));
        }

        [Test]
        public void MultiplicationTest()
        {
            Assert.AreEqual("91", Evaluate("(* 13 7)"));
            Assert.AreEqual("5 5 10 15", Evaluate("(* 5 testvector1)"));
            Assert.AreEqual("4 6 10 14" + lineEnding + "22 26 34 38" + lineEnding + "46 58 62 74", Evaluate("(* 2 testmatrix2)"));

            Assert.AreEqual("15 24 39 63", Evaluate("(* testvector2 3)"));
            Assert.AreEqual("5 8 26 63", Evaluate("(* testvector2 testvector1)"));

            Assert.AreEqual("6 9 15 21" + lineEnding + "33 39 51 57" + lineEnding + "69 87 93 111", Evaluate("(* testmatrix2 3)"));
            Assert.AreEqual("2 6 15 28" + lineEnding + "55 78 119 152" + lineEnding + "207 290 341 444", Evaluate("(* testmatrix2 testmatrix1)"));
        }

        [Test]
        public void DivisionTest()
        {
            Assert.AreEqual("1", Evaluate("(/ 13 7)"));
            Assert.AreEqual("20 12 7 4", Evaluate("(/ 100 testvector2)"));
            Assert.AreEqual("50 33 20 14" + lineEnding + "9 7 5 5" + lineEnding + "4 3 3 2", Evaluate("(/ 100 testmatrix2)"));

            Assert.AreEqual("1 2 4 7", Evaluate("(/ testvector2 3)"));
            Assert.AreEqual("5 8 6 7", Evaluate("(/ testvector2 testvector1)"));

            Assert.AreEqual("0 1 1 2" + lineEnding + "3 4 5 6" + lineEnding + "7 9 10 12", Evaluate("(/ testmatrix2 3)"));
            Assert.AreEqual("2 1 1 1" + lineEnding + "2 2 2 2" + lineEnding + "2 2 2 3", Evaluate("(/ testmatrix2 testmatrix1)"));
        }

        [Test]
        public void MaxTest()
        {
            Assert.AreEqual("13", Evaluate("(max 13 7)"));
            Assert.AreEqual("10 10 13 21", Evaluate("(max 10 testvector2)"));
            Assert.AreEqual("15 15 15 15" + lineEnding + "15 15 17 19" + lineEnding + "23 29 31 37", Evaluate("(max 15 testmatrix2)"));

            Assert.AreEqual("10 10 13 21", Evaluate("(max testvector2 10)"));
            Assert.AreEqual("11 11 13 21", Evaluate("(max testvector2 (+ testvector1 10))"));

            Assert.AreEqual("15 15 15 15" + lineEnding + "15 15 17 19" + lineEnding + "23 29 31 37", Evaluate("(max testmatrix2 15)"));
            Assert.AreEqual("11 12 13 14" + lineEnding + "15 16 17 19" + lineEnding + "23 29 31 37", Evaluate("(max testmatrix2 (+ testmatrix1 10))"));
        }

        [Test]
        public void OrTest()
        {
            Assert.AreEqual("0", Evaluate("(or 0 0)"));
            Assert.AreEqual("1", Evaluate("(or 0 1)"));
            Assert.AreEqual("1", Evaluate("(or 1 0)"));
            Assert.AreEqual("1", Evaluate("(or 1 1)"));
            Assert.AreEqual("0 1 1 0", Evaluate("(or 0 logicalvector4)"));
            Assert.AreEqual("1 1 1 1", Evaluate("(or 1 logicalvector4)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "0 0 0 0" + lineEnding + "1 1 1 1", Evaluate("(or 0 logicalmatrix3)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "1 1 1 1" + lineEnding + "1 1 1 1", Evaluate("(or 1 logicalmatrix3)"));

            Assert.AreEqual("0 1 1 0", Evaluate("(or logicalvector4 0)"));
            Assert.AreEqual("1 1 1 1", Evaluate("(or logicalvector4 1)"));
            Assert.AreEqual("0 1 1 1", Evaluate("(or '(0 0 1 1) '(0 1 0 1))"));

            Assert.AreEqual("1 1 1 1" + lineEnding + "0 0 0 0" + lineEnding + "1 1 1 1", Evaluate("(or logicalmatrix3 0)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "1 1 1 1" + lineEnding + "1 1 1 1", Evaluate("(or logicalmatrix3 1)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "0 1 0 1" + lineEnding + "1 1 1 1", Evaluate("(or logicalmatrix3 logicalmatrix4)"));
        }

        [Test]
        public void AndTest()
        {
            Assert.AreEqual("0", Evaluate("(and 0 0)"));
            Assert.AreEqual("0", Evaluate("(and 0 1)"));
            Assert.AreEqual("0", Evaluate("(and 1 0)"));
            Assert.AreEqual("1", Evaluate("(and 1 1)"));
            Assert.AreEqual("0 0 0 0", Evaluate("(and 0 logicalvector4)"));
            Assert.AreEqual("0 1 1 0", Evaluate("(and 1 logicalvector4)"));
            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 0 0", Evaluate("(and 0 logicalmatrix3)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "0 0 0 0" + lineEnding + "1 1 1 1", Evaluate("(and 1 logicalmatrix3)"));

            Assert.AreEqual("0 0 0 0", Evaluate("(and logicalvector4 0)"));
            Assert.AreEqual("0 1 1 0", Evaluate("(and logicalvector4 1)"));
            Assert.AreEqual("0 0 0 1", Evaluate("(and '(0 0 1 1) '(0 1 0 1))"));

            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 0 0", Evaluate("(and logicalmatrix3 0)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "0 0 0 0" + lineEnding + "1 1 1 1", Evaluate("(and logicalmatrix3 1)"));
            Assert.AreEqual("0 1 0 1" + lineEnding + "0 0 0 0" + lineEnding + "0 1 0 1", Evaluate("(and logicalmatrix3 logicalmatrix4)"));
        }

        [Test]
        public void EqualTest()
        {
            Assert.AreEqual("0", Evaluate("(= 7 13)"));
            Assert.AreEqual("0", Evaluate("(= 13 7)"));
            Assert.AreEqual("1", Evaluate("(= 7 7)"));
            Assert.AreEqual("0 0 1 0", Evaluate("(= 2 testvector1)"));
            Assert.AreEqual("0 0 0 0" + lineEnding + "0 1 0 0" + lineEnding + "0 0 0 0", Evaluate("(= 13 testmatrix2)"));

            Assert.AreEqual("0 0 1 0", Evaluate("(= testvector1 2)"));
            Assert.AreEqual("0 1 0 1", Evaluate("(= testvector2 '(8 8 21 21))"));

            Assert.AreEqual("0 0 0 0" + lineEnding + "0 1 0 0" + lineEnding + "0 0 0 0", Evaluate("(= testmatrix2 13)"));
            Assert.AreEqual("0 1 1 0" + lineEnding + "0 0 0 1" + lineEnding + "0 1 0 0", Evaluate("(= testmatrix2 (restruct '(3 4) '(1 3 5 9 10 17 13 19 22 29 30 30)))"));
        }

        [Test]
        public void LessThanTest()
        {
            Assert.AreEqual("1", Evaluate("(< 7 13)"));
            Assert.AreEqual("0", Evaluate("(< 13 7)"));
            Assert.AreEqual("0", Evaluate("(< 7 7)"));
            Assert.AreEqual("0 0 1 1", Evaluate("(< 1 testvector1)"));
            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 1 1" + lineEnding + "1 1 1 1", Evaluate("(< 13 testmatrix2)"));

            Assert.AreEqual("1 1 0 0", Evaluate("(< testvector1 2)"));
            Assert.AreEqual("1 0 1 0", Evaluate("(< testvector2 '(18 8 21 20))"));

            Assert.AreEqual("1 1 1 1" + lineEnding + "1 0 0 0" + lineEnding + "0 0 0 0", Evaluate("(< testmatrix2 13)"));
            Assert.AreEqual("0 0 0 1" + lineEnding + "0 1 0 0" + lineEnding + "0 0 0 1", Evaluate("(< testmatrix2 (restruct '(3 4) '(1 3 5 9 10 17 13 19 22 29 30 38)))"));
        }

        [Test]
        public void GreaterThanTest()
        {
            Assert.AreEqual("0", Evaluate("(> 7 13)"));
            Assert.AreEqual("1", Evaluate("(> 13 7)"));
            Assert.AreEqual("0", Evaluate("(> 7 7)"));
            Assert.AreEqual("1 1 0 0", Evaluate("(> 2 testvector1)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "1 0 0 0" + lineEnding + "0 0 0 0", Evaluate("(> 13 testmatrix2)"));

            Assert.AreEqual("0 0 0 1", Evaluate("(> testvector1 2)"));
            Assert.AreEqual("1 0 0 1", Evaluate("(> testvector2 '(3 8 21 20))"));

            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 1 1" + lineEnding + "1 1 1 1", Evaluate("(> testmatrix2 13)"));
            Assert.AreEqual("1 0 0 0" + lineEnding + "1 0 1 0" + lineEnding + "1 0 1 1", Evaluate("(> testmatrix2 (restruct '(3 4) '(1 3 5 9 10 17 13 19 22 29 30 36)))"));
        }

        [Test]
        public void AdditionReductionTest()
        {
            Assert.AreEqual("7", Evaluate("(+/ testvector1)"));
            Assert.AreEqual("4 0 4", Evaluate("(+/ logicalmatrix3)"));
        }

        [Test]
        public void SubtractionReductionTest()
        {
            Assert.AreEqual("-11", Evaluate("(-/ testvector2)"));
            Assert.AreEqual("-3 -4 -12", Evaluate("(-/ testmatrix2)"));
        }

        [Test]
        public void MultiplicationReductionTest()
        {
            Assert.AreEqual("6", Evaluate("(*/ testvector1)"));
            Assert.AreEqual("6 120 504", Evaluate("(*/ (restruct '(3 3) '(1 2 3 4 5 6 7 8 9)))"));
        }

        [Test]
        public void DivisionReductionTest()
        {
            Assert.AreEqual("2", Evaluate("(// '(4 4 4 4 4 4 2))"));
            Assert.AreEqual("0 10 3", Evaluate("(// (restruct '(3 3) '(1 4 2 100 20 2 9 9 3)))"));
        }

        [Test]
        public void MaxReductionTest()
        {
            Assert.AreEqual("30", Evaluate("(max/ '(1 15 3 30 22 2))"));
            Assert.AreEqual("4 100 9", Evaluate("(max/ (restruct '(3 3) '(1 4 2 100 20 2 9 9 3)))"));
        }

        [Test]
        public void OrReductionTest()
        {
            Assert.AreEqual("0", Evaluate("(or/ '(0 0 0 0 0 0 0 0))"));
            Assert.AreEqual("1", Evaluate("(or/ '(0 0 1 0 0 0 0 1))"));
            Assert.AreEqual("1", Evaluate("(or/ '(1 1 1 1 1 1 1 1))"));
            Assert.AreEqual("0 1 1", Evaluate("(or/ (restruct '(3 3) '(0 0 0 1 1 1 1 0 1)))"));
        }

        [Test]
        public void AndReductionTest()
        {
            Assert.AreEqual("0", Evaluate("(and/ '(0 0 0 0 0 0 0 0))"));
            Assert.AreEqual("0", Evaluate("(and/ '(0 0 1 0 0 0 0 1))"));
            Assert.AreEqual("1", Evaluate("(and/ '(1 1 1 1 1 1 1 1))"));
            Assert.AreEqual("0 1 0", Evaluate("(and/ (restruct '(3 3) '(0 0 0 1 1 1 1 0 1)))"));
        }

        [Test]
        public void CompressTest()
        {
            Assert.AreEqual("", Evaluate("(compress '(0 0 0 0) testvector2)"));
            Assert.AreEqual("8 13", Evaluate("(compress logicalvector4 testvector2)"));
            Assert.AreEqual("", Evaluate("(compress '(0 0 0) testmatrix2)"));
            Assert.AreEqual("2 3 5 7" + lineEnding + "23 29 31 37", Evaluate("(compress logicalvector3 testmatrix2)"));
        }

        [Test]
        public void ShapeTest()
        {
            Assert.AreEqual("", Evaluate("(shape 7)"));
            Assert.AreEqual("4", Evaluate("(shape testvector1)"));
            Assert.AreEqual("3 4", Evaluate("(shape testmatrix1)"));
        }

        [Test]
        public void RavelTest()
        {
            Assert.AreEqual("7", Evaluate("(ravel 7)"));
            Assert.AreEqual("2 3 5 7", Evaluate("(ravel '(2 3 5 7))"));
            Assert.AreEqual("2 3 5 7 11 13 17 19 23 29 31 37", Evaluate("(ravel testmatrix2)"));
        }

        [Test]
        public void RestructTest()
        {
            Assert.AreEqual("5", Evaluate("(restruct '() testvector2)"));
            Assert.AreEqual("5 8 13", Evaluate("(restruct 3 testvector2)"));
            Assert.AreEqual("5 8 13", Evaluate("(restruct '(3) testvector2)"));
            Assert.AreEqual("5 8" + lineEnding + "13 21", Evaluate("(restruct '(2 2) testvector2)"));

            // A three-dimensional matrix:
            Assert.AreEqual(@"Slice (0) :" + lineEnding +
				"1 2 3 4" + lineEnding +
				"5 6 7 8" + lineEnding +
				"9 10 11 12" + lineEnding + lineEnding +
				"Slice (1) :" + lineEnding +
				"13 14 15 16" + lineEnding +
				"17 18 19 20" + lineEnding +
				"21 22 23 24", Evaluate("(restruct '(2 3 4) (indx 24))"));

            // A four-dimensional matrix:
            Assert.AreEqual(@"Slice (0, 0) :" + lineEnding +
				"1 2 3 4" + lineEnding +
				"5 6 7 8" + lineEnding +
				"9 10 11 12" + lineEnding + lineEnding +
				"Slice (0, 1) :" + lineEnding +
				"13 14 15 16" + lineEnding +
				"17 18 19 20" + lineEnding +
				"21 22 23 24" + lineEnding + lineEnding +
				"Slice (1, 0) :" + lineEnding +
				"25 26 27 28" + lineEnding +
				"29 30 31 32" + lineEnding +
				"33 34 35 36" + lineEnding + lineEnding +
				"Slice (1, 1) :" + lineEnding +
				"37 38 39 40" + lineEnding +
				"41 42 43 44" + lineEnding +
				"45 46 47 48", Evaluate("(restruct '(2 2 3 4) (indx 48))"));

            // Another four-dimensional matrix:
            Assert.AreEqual(@"Slice (0, 0) :" + lineEnding +
				"1 2 3 4" + lineEnding +
				"5 6 7 8" + lineEnding + lineEnding +
				"Slice (0, 1) :" + lineEnding +
				"9 10 11 12" + lineEnding +
				"13 14 15 16" + lineEnding + lineEnding +
				"Slice (0, 2) :" + lineEnding +
				"17 18 19 20" + lineEnding +
				"21 22 23 24" + lineEnding + lineEnding +
				"Slice (1, 0) :" + lineEnding +
				"25 26 27 28" + lineEnding +
				"29 30 31 32" + lineEnding + lineEnding +
				"Slice (1, 1) :" + lineEnding +
				"33 34 35 36" + lineEnding +
				"37 38 39 40" + lineEnding + lineEnding +
				"Slice (1, 2) :" + lineEnding +
				"41 42 43 44" + lineEnding +
				"45 46 47 48", Evaluate("(restruct '(2 3 2 4) (indx 48))"));
        }

        [Test]
        public void CatTest()
        {
            Assert.AreEqual("7 13", Evaluate("(cat 7 13)"));
            Assert.AreEqual("1 1 2 3 5 8 13 21", Evaluate("(cat testvector1 testvector2)"));
        }

        [Test]
        public void IndxTest()
        {
            Assert.AreEqual(string.Empty, Evaluate("(indx 0)"));
            Assert.AreEqual("1 2 3 4 5 6 7", Evaluate("(indx 7)"));
            Assert.AreEqual("1 2 3 4 5 6", Evaluate("(indx '(6 12 18))"));
            Assert.AreEqual("1 2 3 4 5", Evaluate("(indx (restruct '(2 2) '(5 7 2 3)))"));

            Assert.Throws<Exception>(() => Evaluate("(indx 7.5)"));
            Assert.Throws<Exception>(() => Evaluate("(indx '(6.25 12.5 18.75))"));
            Assert.Throws<Exception>(() => Evaluate("(indx (restruct '(2 2) '(5.0 7.0 2.0 3.0)))"));
        }

        [Test]
        public void TransTest()     // Matrix transposition
        {
            // A scalar (no-op):
            Assert.AreEqual("7", Evaluate("(trans 7)"));

            // A vector (no-op):
            Assert.AreEqual("2 3 5 7", Evaluate("(trans '(2 3 5 7))"));

            // A two-dimensional matrix:
            Assert.AreEqual("2 11 23" + lineEnding + "3 13 29" + lineEnding + "5 17 31" + lineEnding + "7 19 37", Evaluate("(trans testmatrix2)"));

            // A three-dimensional matrix:
            var m3string = Evaluate("(set m3 (restruct '(2 3 4) (indx 24)))");

            Assert.AreNotEqual(m3string, Evaluate("(set m3a (trans m3))"));
            Assert.AreNotEqual(m3string, Evaluate("(set m3b (trans m3a))"));
            Assert.AreEqual(m3string, Evaluate("(trans m3b)"));

            // A four-dimensional matrix:
            var m4string = Evaluate("(set m4 (restruct '(2 3 5 7) (indx 210)))");

            Assert.AreNotEqual(m4string, Evaluate("(set m4a (trans m4))"));
            Assert.AreNotEqual(m4string, Evaluate("(set m4b (trans m4a))"));
            Assert.AreNotEqual(m4string, Evaluate("(set m4c (trans m4b))"));
            Assert.AreEqual(m4string, Evaluate("(trans m4c)"));
        }

        [Test]
        public void SubscriptingTest()
        {
            Assert.AreEqual("13", Evaluate("([] testvector2 3)"));
            Assert.AreEqual("8 5 21", Evaluate("([] testvector2 '(2 1 4))"));
            Assert.AreEqual("11 13 17 19", Evaluate("([] testmatrix2 2)"));
            Assert.AreEqual("23 29 31 37" + lineEnding + "2 3 5 7" + lineEnding + "2 3 5 7", Evaluate("([] testmatrix2 '(3 1 1))"));
        }

        #endregion // Operator Tests

        [Test]
        public void EvenSumTest()    // See pages 65 and 70
        {
            Evaluate("(define even? (n) (= (mod n 2) 0))");
            // ThAW 2014/01/23 : The "cat 0" avoids an exception in the case where none of the elements in v is even.
            Evaluate("(define even-sum (v) (+/ (cat 0 (compress (even? v) v))))");

            Assert.AreEqual("0", Evaluate("(even-sum '())"));
            Assert.AreEqual("0", Evaluate("(even-sum '(1 3 5 7))"));
            Assert.AreEqual("2", Evaluate("(even-sum '(2 3 5 7))"));
            Assert.AreEqual("20", Evaluate("(even-sum (indx 8))"));
        }

        [Test]
        public void PrimeTest1()    // See page 73
        {
            Evaluate("(define not (x) (- 1 x))");
            Evaluate("(define <> (x y) (not (= x y)))");
            Evaluate("(define prime (n) (and/ (<> 0 (mod n (+1 (indx (- n 2)))))))");

            Assert.AreEqual("1", Evaluate("(prime 13)"));
            Assert.AreEqual("0", Evaluate("(prime 14)"));
        }

        [Test]
        public void PrimeTest2()    // See pages 74-75
        {
            Evaluate("(define mod-outer-prod (v1 v2) (mod (trans (restruct (cat (shape v2) (shape v1)) v1)) (restruct (cat (shape v1) (shape v2)) v2)))");
            //Evaluate("(define primes<= (n) (compress (= 2 (+/ (= 0 (mod-outer-prod (set s (indx n)) s)))) s))");  // Note the use of "set".
            Evaluate(@"
(define primes<= (n)
    (let ((s (indx n)))
        (compress (= 2 (+/ (= 0 (mod-outer-prod s s)))) s)))");

            Assert.AreEqual("2 3 5 7", Evaluate("(primes<= 7)"));
        }

        [Test]
        public void IdentityMatrixTest()
        {
            Evaluate("(define idmatrix (n) (restruct (cat n n) (= 1 (indx (+ n 1)))))");

            Assert.AreEqual("1 0 0 0" + lineEnding + "0 1 0 0" + lineEnding + "0 0 1 0" + lineEnding + "0 0 0 1", Evaluate("(idmatrix 4)"));
        }

        [Test]
        public void IfTest()
        {
            // "if" and "while" must use GetFirstScalar(); see the "not=" example on pages 70-71.
            Assert.AreEqual("1", Evaluate("(if (= '(2 3 5 7) '(2 3 5 7)) 1 0)"));
            Assert.AreEqual("1", Evaluate("(if (= '(2 3 5 7) '(2 4 6 8)) 1 0)"));
            Assert.AreEqual("0", Evaluate("(if (= '(1 3 5 7) '(2 4 6 8)) 1 0)"));

            Assert.AreEqual("13", Evaluate("(if 0 7 13)"));
            Assert.AreEqual("7", Evaluate("(if 21 7 13)"));
            Assert.AreEqual("13", Evaluate("(if '(0 1 2) 7 13)"));
            Assert.AreEqual("7", Evaluate("(if '(2 1 0) 7 13)"));
            Assert.AreEqual("13", Evaluate("(if (restruct '(2 2) '(0 1 2 3)) 7 13)"));
            Assert.AreEqual("7", Evaluate("(if (restruct '(2 2) '(3 2 1 0)) 7 13)"));
        }

        [Test]
        public void WhileTest()
        {
            Evaluate(@"
(define whileTest (x decrement)
    (let ((counter 0))
        (begin
            (while x
                (begin
                    (set x (- x decrement))
                    (set counter (+1 counter))
                )
            )
            counter
        )
    )
)");

            Assert.AreEqual("3", Evaluate("(whileTest 12 4)"));
            Assert.AreEqual("6", Evaluate("(whileTest '(12 13 14) 2)"));
            Assert.AreEqual("4", Evaluate("(whileTest (restruct '(2 2) '(20 15 10 5)) 5)"));
        }

        [Test]
        public void MatrixMultiplicationTest()      // See page 90 (exercise 8)
        {
            /*
            Evaluate(@"(define matrix-mult (matrix1 matrix2) (begin
                (set m ([] (shape matrix1) 1))
                (set n ([] (shape matrix1) 2))
                (set p ([] (shape matrix2) 2))
                (set mp (* m p))
                (set trans2 (trans matrix2))
                (set indices1 (+ (/ (- (indx mp) 1) p) 1))
                (set indices2 (+ (mod (- (indx mp) 1) p) 1))
                (set mpbynmatrix1 ([] matrix1 indices1))
                (set mpbynmatrix2 ([] trans2 indices2))
                (set productmatrix (* mpbynmatrix1 mpbynmatrix2))
                (set finalvector (+/ productmatrix))
                (restruct (cat m p) finalvector)))");
             */
            Evaluate(@"
(define matrix-mult (matrix1 matrix2)
    (let ((m ([] (shape matrix1) 1))
          (p ([] (shape matrix2) 2)))
        (begin
            (restruct (cat m p) (+/ (* ([] matrix1 (+ (/ (- (indx (* m p)) 1) p) 1)) ([] (trans matrix2) (+ (mod (- (indx (* m p)) 1) p) 1))))))))");

            Evaluate("(set m1 (restruct '(2 2) '(1 2 3 4)))");
            Evaluate("(set m2 (restruct '(2 2) '(5 6 7 8)))");
            Assert.AreEqual("19 22" + lineEnding + "43 50", Evaluate("(matrix-mult m1 m2)"));

            // Test with non-square matrices; e.g. a 2*3 matrix multiplied by a 3*4 matrix.
            Evaluate("(set m3 (restruct '(2 3) '(1 2 3 4 5 6)))");
            Evaluate("(set m4 (restruct '(3 4) '(1 2 3 4 2 3 4 5 3 4 5 6)))");
            Assert.AreEqual("14 20 26 32" + lineEnding + "32 47 62 77", Evaluate("(matrix-mult m3 m4)"));
        }

        [Test]
        public void CountInMatrixTest()  // See page 88 (exercise 1)
        {
            Evaluate("(define count (x m) (+/ (+/ (= x m))))");

            Assert.AreEqual("4", Evaluate("(count 3 (restruct '(4 4) '(1 2 3 4 3 6 3 9 0 1 2 4 2 3 5 7)))"));
        }

        [Test]
        public void SafeFindTest()  // See page 89 (exercise 2)
        {
            Evaluate("(define find-page72 (x v) ([] (compress (= x v) (indx (shape v))) 1))");

            // This version of (find x v) returns 0 if x does not occur in v.
            Evaluate("(define find (x v) (mod (find-page72 x (cat v x)) (+1 (shape v))))");

            Assert.AreEqual("4", Evaluate("(find 3 '(1 1 2 3 5 8))"));
            Assert.AreEqual("0", Evaluate("(find 4 '(1 1 2 3 5 8))"));
        }

        [Test]
        public void SparseVectorTest1()  // See page 89 (exercise 3)
        {
            Evaluate("(define skip (n v) (compress (< n (indx (shape v))) v))");
            Evaluate(@"
(define expand-rep (v)
    (let* ((m ([] v 1))
           (v2 (skip 1 v))
           (n (shape v2))
           (matrix1 (trans (restruct (cat n m) (indx m))))
           (matrix2 (restruct (cat m n) v2)))
        (+/ (= matrix1 matrix2))
    )
)");

            Assert.AreEqual("1 0 0 1 0", Evaluate("(expand-rep '(5 1 4))"));

            Evaluate("(define contract-rep (v) (cat (shape v) (compress v (indx (shape v)))))");

            Assert.AreEqual("5 1 4", Evaluate("(contract-rep '(1 0 0 1 0))"));
        }

        [Test]
        public void SparseVectorTest2()  // See page 89 (exercise 4)
        {
            Evaluate(@"
(define assign (v i x)
    (cat ([] v (indx (- i 1)))
        (cat x ([] v (+ i (indx (- (shape v) i)))))))");
            Evaluate("(define drop-n (n v) ([] v (+ n (indx (- (shape v) n)))))");
            Evaluate(@"
(define vecassign (v ix)
    (if (= (shape ix) 0) v
        (vecassign (assign v ([] ix 1) ([] ix 2)) (drop-n 2 ix))))");
            Evaluate(@"
(define expand-rep (v)
    (vecassign (restruct ([] v 1) 0) (drop-n 1 v)))");

            Assert.AreEqual("23 0 0 15 0", Evaluate("(expand-rep '(5 1 23 4 15))"));
        }

        [Test]
        public void Exercises5to9Test()  // See pages 89-90
        {
            Evaluate("(define 1-to-n-matrix (n) (restruct (cat n n) (indx n)))");

            Assert.AreEqual("1 2 3 4" + lineEnding + "1 2 3 4" + lineEnding + "1 2 3 4" + lineEnding + "1 2 3 4", Evaluate("(1-to-n-matrix 4)"));

            // Exercise 5a : The identity matrix
            Evaluate("(define identity-matrix (n) (= (1-to-n-matrix n) (trans (1-to-n-matrix n))))");

            Assert.AreEqual("1 0 0 0" + lineEnding + "0 1 0 0" + lineEnding + "0 0 1 0" + lineEnding + "0 0 0 1", Evaluate("(identity-matrix 4)"));

            // Exercise 5b : The secondary diagonal matrix
            Evaluate("(define n-to-1-matrix (n) (- (+1 n) (1-to-n-matrix n)))");
            Evaluate("(define sec-diag-matrix (n) (= (n-to-1-matrix n) (trans (1-to-n-matrix n))))");

            Assert.AreEqual("0 0 0 1" + lineEnding + "0 0 1 0" + lineEnding + "0 1 0 0" + lineEnding + "1 0 0 0", Evaluate("(sec-diag-matrix 4)"));

            // Exercise 5c : The upper triangular logical matrix
            Evaluate("(define >= (x y) (or (> x y) (= x y)))");
            Evaluate("(define upper-triangular-logical (n) (>= (1-to-n-matrix n) (trans (1-to-n-matrix n))))");

            Assert.AreEqual("1 1 1 1" + lineEnding + "0 1 1 1" + lineEnding + "0 0 1 1" + lineEnding + "0 0 0 1", Evaluate("(upper-triangular-logical 4)"));

            // Exercise 5d : The lower triangular 1-to-n matrix
            Evaluate("(define lower-triangular-logical (n) (trans (upper-triangular-logical n)))");
            Evaluate("(define lower-triangular-1-to-n (n) (* (1-to-n-matrix n) (lower-triangular-logical n)))");

            Assert.AreEqual("1 0 0 0" + lineEnding + "1 2 0 0" + lineEnding + "1 2 3 0" + lineEnding + "1 2 3 4", Evaluate("(lower-triangular-1-to-n 4)"));

            // Exercise 5e
            Evaluate("(define filter-out-negtives (m) (* m (> m 0)))");
            Evaluate("(define exercise5e (n) (filter-out-negtives (+1 (- (trans (1-to-n-matrix n)) (1-to-n-matrix n)))))");

            Assert.AreEqual("1 0 0 0" + lineEnding + "2 1 0 0" + lineEnding + "3 2 1 0" + lineEnding + "4 3 2 1", Evaluate("(exercise5e 4)"));

            // Exercise 6 : Diagonal product
            Evaluate("(define diag-prod (m) (*/ (+/ (* m (identity-matrix ([] (shape m) 1))))))");

            Assert.AreEqual("1056", Evaluate("(diag-prod (restruct '(4 4) (indx 16)))"));

            // Exercise 7 : +-scan (+\)
            Evaluate(@"(define matrix-vector-mult (matrix vector)
                (+/ (* matrix (restruct (cat ([] (shape matrix) 1) (shape vector)) vector))))");
            Evaluate(@"(define +\ (v) (matrix-vector-mult (lower-triangular-logical (shape v)) v))");

            Evaluate("(set m (restruct '(3 4) '(1 2 3 4 2 3 4 5 3 4 5 6)))");
            Assert.AreEqual("51 68 85", Evaluate("(matrix-vector-mult m '(2 3 5 7))"));

            Assert.AreEqual("1 4 9 16", Evaluate(@"(+\ '(1 3 5 7))"));  // Page 74

            // Exercise 8 : Matrix multiplication : Implemented and tested elsewhere

            // Exercise 9 : Expansion (see the entry in Table 3.3 on page 86)
            Evaluate(@"
(define expand-vector (c v)
    (if (= 0 (shape v))
        (restruct (shape c) 0)
        (* c ([] v (max 1 (+\ c))))))");
            Evaluate("(define make-rows (v n) (trans (restruct (cat n (shape v)) v)))");
            Evaluate(@"
(define expand-matrix (c v)
    (if (= 0 ([] (shape v) 1))
        (restruct (cat (shape c) ([] (shape v) 2)) 0)
        (* (make-rows c ([] (shape v) 2)) ([] v (max 1 (+\ c))))))");
            Evaluate("(define expand (c v) (if (= 2 (shape (shape v))) (expand-matrix c v) (expand-vector c v)))");

            Assert.AreEqual("0 0 0 0 0", Evaluate("(expand '(0 0 0 0 0) '())"));
            Assert.AreEqual("2 0 4 0 8", Evaluate("(expand '(1 0 1 0 1) '(2 4 8))"));   // The example from page 86
            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 0 0",
                Evaluate("(expand '(0 0 0 0 0) (restruct '(0 4) 0))"));
            Assert.AreEqual("1 2 3 4" + lineEnding + "0 0 0 0" + lineEnding + "5 6 7 8" + lineEnding + "0 0 0 0" + lineEnding + "9 10 11 12",
                Evaluate("(expand '(1 0 1 0 1) (restruct '(3 4) (indx 12)))"));
        }

        [Test]
        public void HistogramTest()  // See section 3.3 on pages 77-83, and exercise 10 on page 90
        {
            globalInfo.LoadPreset(@"+\");
            globalInfo.LoadPreset("find");
            globalInfo.LoadPreset("<=");
            globalInfo.LoadPreset("reverse");

            // Functions from figure 3.2 on page 79
            Evaluate(@"
(define dup-cols (v n)
    (trans (restruct (cat n (shape v)) v)))");
            Evaluate(@"
(define dup-rows (v n)
    ([] (restruct (cat 1 (shape v)) v) (restruct n 1)))");
            Evaluate(@"
(define freqvec (scores lo hi)
    (let ((width (+ (- hi lo) 1)))
        (+/ (trans (=
            (dup-cols scores width)
            (dup-rows (+ (indx width) (- lo 1)) (shape scores)))))))");
            Evaluate(@"(define cumfreqvec (freqs) (+\ freqs))");    // @ because of the backslash

            // Functions from figure 3.3 on page 81
            Evaluate("(define range (scores) (cat (min/ scores) (max/ scores)))");
            Evaluate("(define mode (freqs lo) (+ (find (max/ freqs) freqs) (- lo 1)))");
            Evaluate(@"
(define median (cumfreqs lo)
    (+ (- lo 1) (find-closest (max/ cumfreqs) (* 2 cumfreqs))))");

            // Functions from figure 3.4 on page 81
            Evaluate(@"
(define addelt (e i v)
    (cat ([] v (indx (- i 1)))
        (cat e ([] v (+ (indx (- (+1 (shape v)) i)) (- i 1))))))");
            Evaluate(@"
(define addrow (v i m)
    ([] (restruct (+ '(1 0) (shape m)) (cat v m))
        (addelt 1 i (+1 (indx ([] (shape m) 1))))))");
            Evaluate(@"
(define addcol (v i m)
    (trans (addrow v i (trans m))))");
            Evaluate(@"
(define histo (freqs lo hi)
    (let* ((width (+1 (- hi lo)))
           (length (max/ freqs))
           (hist (<= (restruct (cat width length) (indx length)) (dup-cols freqs length))))
        (addcol (- (indx width) (- 1 lo)) 1 hist)))");

            // Function from figure 3.5 on page 83
            Evaluate(@"
(define graph (freqs lo)
    (let* ((length (max/ freqs))
           (lines (restruct (cat (+ length 1) length)
                            (cat (restruct length 0) 1)))
           (thegraph (reverse (trans ([] lines (+ freqs 1))))))
        (addrow (- (indx (shape freqs)) (- 1 lo)) (+ length 1) thegraph)))");

            // From page 78
            Evaluate("(set SCORES '(-2 1 -1 0 0 2 1 1))");
            Assert.AreEqual("1 1 2 3 1", Evaluate("(set FREQS (freqvec SCORES -2 2))"));
            Assert.AreEqual("1 2 4 7 8", Evaluate("(set CUMFREQS (cumfreqvec FREQS))"));
            Assert.AreEqual("-2 2", Evaluate("(range SCORES)"));
            Assert.AreEqual("1", Evaluate("(mode FREQS -2)"));
            Assert.AreEqual("0", Evaluate("(median CUMFREQS -2)"));
            Assert.AreEqual("-2 1 0 0" + lineEnding + "-1 1 0 0" + lineEnding + "0 1 1 0" + lineEnding + "1 1 1 1" + lineEnding + "2 1 0 0",
                Evaluate("(histo FREQS -2 2)"));
            Assert.AreEqual("0 0 0 1 0" + lineEnding + "0 0 1 0 0" + lineEnding + "1 1 0 0 1" + lineEnding + "-2 -1 0 1 2",
                Evaluate("(graph FREQS -2)"));
            Assert.AreEqual("0 0 0 0 1" + lineEnding + "0 0 0 1 0" + lineEnding + "0 0 0 0 0" + lineEnding + "0 0 0 0 0" + lineEnding + "0 0 1 0 0" + lineEnding + "0 0 0 0 0" + lineEnding + "0 1 0 0 0" + lineEnding + "1 0 0 0 0" + lineEnding + "-2 -1 0 1 2",
                Evaluate("(graph CUMFREQS -2)"));

            // Exercise 10a) on page 90
            Evaluate(@"
(define trans-dup-cols (v n)
    (restruct (cat n (shape v)) v))");
            Evaluate(@"
(define freqvec-10a (scores lo hi)
    (let ((width (+ (- hi lo) 1)))
        (+/ (=
            (trans-dup-cols scores width)
            (trans (dup-rows (+ (indx width) (- lo 1)) (shape scores)))))))");

            Assert.AreEqual("1 1 2 3 1", Evaluate("(freqvec-10a SCORES -2 2)"));

            // Exercise 10b) part 1: implement histogram using subscripting of a "lines" matrix
            Evaluate(@"
(define histo-10b (freqs lo hi)
    (let* ((width (+1 (- hi lo)))
           (length (max/ freqs))
           (matrix1 (dup-rows (indx length) (+1 length)))
           (matrix2 (dup-cols (cat 0 (indx length)) length))
           (lines (<= matrix1 matrix2))
           (hist ([] lines (+1 freqs))))
        (addcol (- (indx width) (- 1 lo)) 1 hist)))");

            Assert.AreEqual("-2 1 0 0" + lineEnding + "-1 1 0 0" + lineEnding + "0 1 1 0" + lineEnding + "1 1 1 1" + lineEnding + "2 1 0 0",
                Evaluate("(histo-10b FREQS -2 2)"));

            // Exercise 10b) part 2: implement graph using a comparison of two matrices
            Evaluate(@"
(define graph-10b (freqs lo)
    (let* ((width (shape freqs))
           (length (max/ freqs))
           (matrix1 (dup-rows (indx length) width))
           (matrix2 (dup-cols freqs length))
           (thegraph (reverse (trans (= matrix1 matrix2)))))
        (addrow (- (indx (shape freqs)) (- 1 lo)) (+ length 1) thegraph)))");

            Assert.AreEqual("0 0 0 1 0" + lineEnding + "0 0 1 0 0" + lineEnding + "1 1 0 0 1" + lineEnding + "-2 -1 0 1 2",
                Evaluate("(graph-10b FREQS -2)"));
            Assert.AreEqual("0 0 0 0 1" + lineEnding + "0 0 0 1 0" + lineEnding + "0 0 0 0 0" + lineEnding + "0 0 0 0 0" + lineEnding + "0 0 1 0 0" + lineEnding + "0 0 0 0 0" + lineEnding + "0 1 0 0 0" + lineEnding + "1 0 0 0 0" + lineEnding + "-2 -1 0 1 2",
                Evaluate("(graph CUMFREQS -2)"));

            // Exercise 10c) A grouped histogram
            Evaluate(@"
(define interval-sums (v interval)
    (let ((sum v)
          (vector-length-limit (- (+ (shape v) interval) 1)))
        (begin
            (while (< (shape v) vector-length-limit)
                (begin
                    (set sum (cat sum 0))
                    (set v (cat 0 v))
                    (set sum (+ sum v))))
            (let ((indexes (* interval (indx (/ vector-length-limit interval)))))
                ([] sum indexes)))))");
            Evaluate(@"
(define interval-labels (lo hi interval)
    (+ (* (- (indx (/ (- (+ hi interval) lo) interval)) 1) interval) lo))");
            Evaluate(@"
(define grouped-histogram (freqs lo hi interval)
    (let* ((grouped-freqs (interval-sums freqs interval))
           (width (/ (- (+ hi interval) lo) interval))
           (length (max/ grouped-freqs))
           (hist (<= (restruct (cat width length) (indx length))
                     (dup-cols grouped-freqs length))))
        (addcol (interval-labels lo hi interval) 1 hist)))");

            Assert.AreEqual("6 15 24 10", Evaluate("(interval-sums (indx 10) 3)"));
            Assert.AreEqual("1 4 7 10", Evaluate("(interval-labels 1 10 3)"));
            Assert.AreEqual("-2 1 1 0" + lineEnding + "1 1 1 1" + lineEnding + "4 0 0 0" + lineEnding + "7 1 0 0",
                Evaluate("(grouped-histogram '(1 0 1 2 1 0 0 0 0 1) -2 7 3)"));
        }

        [Test]
        public void VectorAssignmentTest()  // See exercise 12 on page 90
        {
            Evaluate("(set vector1 '(10 20 30 40 50))");
            Evaluate("(:= vector1 4 13)");
            Assert.AreEqual("10 20 30 13 50", Evaluate("vector1"));

            Evaluate("(set vector2 '(10 20 30 40 50))");
            Evaluate("(:= vector2 '(3 5 1) '(7 9 11))");
            Assert.AreEqual("11 20 7 40 9", Evaluate("vector2"));
        }

        [Test]
        public void DoubleSubscriptingTest()  // See exercise 13 on page 90
        {
            Assert.AreEqual("7 9" + lineEnding + "17 19", Evaluate("([;] (restruct '(5 5) (indx 25)) '(2 4) '(2 4))"));
            Assert.AreEqual("1 4 5" + lineEnding + "6 9 10", Evaluate("([;] (restruct '(4 5) (indx 20)) '(1 2) '(1 4 5))"));
        }

        #region Tests with Floating-Point Numbers

        [Test]
        public void FloatAdditionTest()
        {
            // Test combinations of adding int and float scalars, vectors, and matrices.
            Assert.AreEqual("5.5", Evaluate("(+ 2 3.5)"));
            Assert.AreEqual("5.25", Evaluate("(+ 2.25 3)"));
            Assert.AreEqual("6.0", Evaluate("(+ 2.5 3.5)"));

            Assert.AreEqual("3.0 3.25 3.5 3.75", Evaluate("(+ 2 floatvector5)"));
            Assert.AreEqual("1.5 1.5 2.5 3.5", Evaluate("(+ 0.5 testvector1)"));
            Assert.AreEqual("1.5 1.75 2.0 2.25", Evaluate("(+ 0.5 floatvector5)"));

            Assert.AreEqual("4.0 5.0 7.5 9.5" + lineEnding + "13.0 15.0 19.5 21.5" + lineEnding + "25.0 31.0 33.5 39.5", Evaluate("(+ 2 floatmatrix6)"));
            Assert.AreEqual("2.5 3.5 5.5 7.5" + lineEnding + "11.5 13.5 17.5 19.5" + lineEnding + "23.5 29.5 31.5 37.5", Evaluate("(+ 0.5 testmatrix2)"));
            Assert.AreEqual("2.5 3.5 6.0 8.0" + lineEnding + "11.5 13.5 18.0 20.0" + lineEnding + "23.5 29.5 32.0 38.0", Evaluate("(+ 0.5 floatmatrix6)"));

            Assert.AreEqual("4.0 4.25 4.5 4.75", Evaluate("(+ floatvector5 3)"));
            Assert.AreEqual("5.5 8.5 13.5 21.5", Evaluate("(+ testvector2 0.5)"));
            Assert.AreEqual("1.5 1.75 2.0 2.25", Evaluate("(+ floatvector5 0.5)"));

            Assert.AreEqual("2.0 2.25 3.5 4.75", Evaluate("(+ floatvector5 testvector1)"));
            Assert.AreEqual("7.0 11.5 18.0 28.5", Evaluate("(+ testvector2 floatvector6)"));
            Assert.AreEqual("3.0 4.75 6.5 9.25", Evaluate("(+ floatvector5 floatvector6)"));

            Assert.AreEqual("6.0 7.5 8.0 9.5" + lineEnding + "10.0 11.5 12.0 13.5" + lineEnding + "14.0 15.5 16.0 17.5", Evaluate("(+ floatmatrix5 5)"));
            Assert.AreEqual("1.25 2.25 3.25 4.25" + lineEnding + "5.25 6.25 7.25 8.25" + lineEnding + "9.25 10.25 11.25 12.25", Evaluate("(+ testmatrix1 0.25)"));
            Assert.AreEqual("1.25 2.75 3.25 4.75" + lineEnding + "5.25 6.75 7.25 8.75" + lineEnding + "9.25 10.75 11.25 12.75", Evaluate("(+ floatmatrix5 0.25)"));

            Assert.AreEqual("3.0 5.5 8.0 11.5" + lineEnding + "16.0 19.5 24.0 27.5" + lineEnding + "32.0 39.5 42.0 49.5", Evaluate("(+ floatmatrix5 testmatrix2)"));
            Assert.AreEqual("3.0 5.0 8.5 11.5" + lineEnding + "16.0 19.0 24.5 27.5" + lineEnding + "32.0 39.0 42.5 49.5", Evaluate("(+ testmatrix1 floatmatrix6)"));
            Assert.AreEqual("3.0 5.5 8.5 12.0" + lineEnding + "16.0 19.5 24.5 28.0" + lineEnding + "32.0 39.5 42.5 50.0", Evaluate("(+ floatmatrix5 floatmatrix6)"));
        }

        [Test]
        public void FloatEqualTest()
        {
            Assert.AreEqual("0", Evaluate("(= 7.5 13)"));
            Assert.AreEqual("1", Evaluate("(= 7.0 7)"));
            Assert.AreEqual("0", Evaluate("(= 7 13.5)"));
            Assert.AreEqual("1", Evaluate("(= 7 7.0)"));
            Assert.AreEqual("0", Evaluate("(= 7.0 7.1)"));
            Assert.AreEqual("1", Evaluate("(= 7.5 7.5)"));

            Assert.AreEqual("0 0 1 0", Evaluate("(= 2.0 testvector1)"));
            Assert.AreEqual("1 0 0 0", Evaluate("(= 1 floatvector5)"));
            Assert.AreEqual("0 1 0 0", Evaluate("(= 1.25 floatvector5)"));

            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 1 0", Evaluate("(= 31.0 testmatrix2)"));
            Assert.AreEqual("0 1 0 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 0 0", Evaluate("(= 3 floatmatrix6)"));
            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 0 1" + lineEnding + "0 0 0 0", Evaluate("(= 8.5 floatmatrix5)"));

            Assert.AreEqual("0 0 1 0", Evaluate("(= floatvector6 5)"));
            Assert.AreEqual("0 0 0 1", Evaluate("(= testvector1 3.0)"));
            Assert.AreEqual("0 0 0 1", Evaluate("(= floatvector5 1.75)"));

            Assert.AreEqual("0 0 1 0", Evaluate("(= floatvector6 '(3 4 5 6))"));
            Assert.AreEqual("0 1 1 0", Evaluate("(= testvector2 '(8.0 8.0 13.0 13.0))"));
            Assert.AreEqual("1 0 0 1", Evaluate("(= floatvector5 '(1.0 1.5 1.25 1.75))"));

            Assert.AreEqual("0 0 0 0" + lineEnding + "1 0 0 0" + lineEnding + "0 0 0 0", Evaluate("(= floatmatrix6 11)"));
            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 0 1", Evaluate("(= testmatrix2 37.0)"));
            Assert.AreEqual("0 0 1 0" + lineEnding + "0 0 0 0" + lineEnding + "0 0 0 0", Evaluate("(= floatmatrix6 5.5)"));

            Assert.AreEqual("0 1 0 0" + lineEnding + "0 0 0 0" + lineEnding + "1 1 0 0", Evaluate("(= floatmatrix6 (restruct '(3 4) '(1 3 5 9 10 17 13 19 23 29 30 30)))"));
            Assert.AreEqual("0 0 1 0" + lineEnding + "1 0 0 0" + lineEnding + "1 0 0 0", Evaluate("(= testmatrix2 (restruct '(3 4) '(1.5 3.5 5.0 9.5 11.0 17.0 13.5 19.5 23.0 29.5 30.0 30.5)))"));
            Assert.AreEqual("0 1 1 0" + lineEnding + "0 1 0 1" + lineEnding + "1 0 0 1", Evaluate("(= floatmatrix6 (restruct '(3 4) '(1.0 3.0 5.5 9.0 10.5 13.0 13.5 19.5 23.0 29.5 30.0 37.5)))"));
        }

        [Test]
        public void FloatAdditionReductionTest()
        {
            Assert.AreEqual("5.5", Evaluate("(+/ floatvector5)"));
            Assert.AreEqual("11.0 27.0 43.0", Evaluate("(+/ floatmatrix5)"));
        }

        [Test]
        public void FloatLessThanTest() // This test tests a non-commutative operator (<), to ensure that args are passed in the correct order.
        {
            Assert.AreEqual("1", Evaluate("(< 7.0 13)"));
            Assert.AreEqual("0", Evaluate("(< 13.0 7)"));
            Assert.AreEqual("0", Evaluate("(< 7.0 7)"));

            Assert.AreEqual("1", Evaluate("(< 7 13.0)"));
            Assert.AreEqual("0", Evaluate("(< 13 7.0)"));
            Assert.AreEqual("0", Evaluate("(< 7 7.0)"));

            Assert.AreEqual("1", Evaluate("(< 7.0 7.5)"));
            Assert.AreEqual("0", Evaluate("(< 7.5 7.0)"));
            Assert.AreEqual("0", Evaluate("(< 7.0 7.0)"));

            Assert.AreEqual("0 0 1 1", Evaluate("(< 9.5 testvector2)"));
            Assert.AreEqual("0 1 1 1", Evaluate("(< 2 floatvector6)"));
            Assert.AreEqual("0 0 0 1", Evaluate("(< 6.25 floatvector6)"));

            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 1 1" + lineEnding + "1 1 1 1", Evaluate("(< 13.5 testmatrix2)"));
            Assert.AreEqual("0 0 0 0" + lineEnding + "0 0 0 1" + lineEnding + "1 1 1 1", Evaluate("(< 7 floatmatrix5)"));
            Assert.AreEqual("0 0 0 0" + lineEnding + "1 1 1 1" + lineEnding + "1 1 1 1", Evaluate("(< 4.75 floatmatrix5)"));

            Assert.AreEqual("1 1 0 0", Evaluate("(< floatvector6 5)"));
            Assert.AreEqual("1 1 1 0", Evaluate("(< testvector1 2.25)"));
            Assert.AreEqual("1 0 0 0", Evaluate("(< floatvector6 3.5)"));

            Assert.AreEqual("1 0 1 0", Evaluate("(< floatvector6 '(3 3 6 7))"));
            Assert.AreEqual("0 0 1 0", Evaluate("(< testvector2 '(4.75 8.0 21.5 20.25))"));
            Assert.AreEqual("0 1 1 0", Evaluate("(< floatvector6 '(1.25 5.0 5.125 0.0))"));

            Assert.AreEqual("1 1 1 1" + lineEnding + "1 1 1 1" + lineEnding + "1 1 0 0", Evaluate("(< floatmatrix6 30)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "1 0 0 0" + lineEnding + "0 0 0 0", Evaluate("(< testmatrix2 12.75)"));
            Assert.AreEqual("1 1 1 1" + lineEnding + "1 1 0 0" + lineEnding + "0 0 0 0", Evaluate("(< floatmatrix6 17.5)"));

            Assert.AreEqual("0 0 0 1" + lineEnding + "0 1 0 0" + lineEnding + "0 0 0 1", Evaluate("(< floatmatrix6 (restruct '(3 4) '(1 3 5 9 10 17 13 19 22 29 30 38)))"));
            Assert.AreEqual("1 0 1 1" + lineEnding + "0 1 0 1" + lineEnding + "0 0 1 0", Evaluate("(< testmatrix2 (restruct '(3 4) '(2.5 3.0 5.5 9.0 10.5 17.0 13.5 19.5 22.5 29.0 31.5 37.0)))"));
            Assert.AreEqual("1 0 1 0" + lineEnding + "1 0 0 1" + lineEnding + "1 0 0 1", Evaluate("(< floatmatrix6 (restruct '(3 4) '(2.5 3.0 6.0 7.5 11.5 12.0 13.5 20.0 23.5 29.0 31.5 38.0)))"));
        }

        [Test]
        public void FloatOrReductionTest()
        {
            Assert.AreEqual("0", Evaluate("(or/ '(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))"));
            Assert.AreEqual("1", Evaluate("(or/ '(0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0))"));
            Assert.AreEqual("1", Evaluate("(or/ '(0.0 0.0 1.5 0.0 0.0 0.0 0.0 7.0))"));
            Assert.AreEqual("1", Evaluate("(or/ '(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0))"));
            Assert.AreEqual("0 1 1", Evaluate("(or/ (restruct '(3 3) '(0.0 0.0 0.0 1.0 1.0 1.0 1.0 0.0 1.0)))"));
        }

        [Test]
        public void FloatCompressTest()
        {
            Assert.AreEqual("", Evaluate("(compress '(0 0 0 0) floatvector6)"));
            Assert.AreEqual("3.5 5.0", Evaluate("(compress logicalvector4 floatvector6)"));
            Assert.AreEqual("", Evaluate("(compress '(0 0 0) floatmatrix6)"));
            Assert.AreEqual("2.0 3.0 5.5 7.5" + lineEnding + "23.0 29.0 31.5 37.5", Evaluate("(compress logicalvector3 floatmatrix6)"));
        }

        [Test]
        public void FloatShapeTest()
        {
            Assert.AreEqual("", Evaluate("(shape 7.5)"));
            Assert.AreEqual("4", Evaluate("(shape floatvector5)"));
            Assert.AreEqual("3 4", Evaluate("(shape floatmatrix5)"));
        }

        [Test]
        public void FloatRavelTest()
        {
            Assert.AreEqual("7.5", Evaluate("(ravel 7.5)"));
            Assert.AreEqual("2.0 3.25 5.5 7.75", Evaluate("(ravel '(2.0 3.25 5.5 7.75))"));
            Assert.AreEqual("2.0 3.0 5.5 7.5 11.0 13.0 17.5 19.5 23.0 29.0 31.5 37.5", Evaluate("(ravel floatmatrix6)"));
        }

        [Test]
        public void FloatRestructTest()
        {
            Assert.AreEqual("2.0", Evaluate("(restruct '() floatvector6)"));
            Assert.AreEqual("2.0 3.5 5.0", Evaluate("(restruct 3 floatvector6)"));
            Assert.AreEqual("2.0 3.5 5.0", Evaluate("(restruct '(3) floatvector6)"));
            Assert.AreEqual("2.0 3.5" + lineEnding + "5.0 7.5", Evaluate("(restruct '(2 2) floatvector6)"));
        }

        [Test]
        public void FloatCatTest()
        {
            Assert.AreEqual("7.5 13.25", Evaluate("(cat 7.5 13.25)"));
            Assert.AreEqual("1.0 1.25 1.5 1.75 2.0 3.5 5.0 7.5", Evaluate("(cat floatvector5 floatvector6)"));
        }

        [Test]
        public void FloatTransTest()     // Matrix transposition
        {
            Assert.AreEqual("2.0 11.0 23.0" + lineEnding + "3.0 13.0 29.0" + lineEnding + "5.5 17.5 31.5" + lineEnding + "7.5 19.5 37.5", Evaluate("(trans floatmatrix6)"));
        }

        [Test]
        public void FloatSubscriptingTest()
        {
            Assert.AreEqual("1.5", Evaluate("([] floatvector5 3)"));
            Assert.AreEqual("3.5 2.0 7.5", Evaluate("([] floatvector6 '(2 1 4))"));
            Assert.AreEqual("11.0 13.0 17.5 19.5", Evaluate("([] floatmatrix6 2)"));
            Assert.AreEqual("23.0 29.0 31.5 37.5" + lineEnding + "2.0 3.0 5.5 7.5" + lineEnding + "2.0 3.0 5.5 7.5", Evaluate("([] floatmatrix6 '(3 1 1))"));
        }

        [Test]
        public void FloatIfTest()
        {
            // "if" and "while" must use GetFirstScalar(); see the "not=" example on pages 70-71.
            Assert.AreEqual("7.5", Evaluate("(if (= '(2.75 3.5 5.25 7.0) '(2.75 3.5 5.25 7.0)) 7.5 13.25)"));
            Assert.AreEqual("7.5", Evaluate("(if (= '(2.5 3.5 5.5 7.5) '(2.5 4.0 6.0 8.5)) 7.5 13.25)"));
            Assert.AreEqual("13.25", Evaluate("(if (= '(1.25 3.0 5.0 7.0) '(2.5 4.5 6.5 8.5)) 7.5 13.25)"));

            Assert.AreEqual("13.5", Evaluate("(if 0.0 7.25 13.5)"));
            Assert.AreEqual("7.25", Evaluate("(if 21.25 7.25 13.5)"));
            Assert.AreEqual("13.5", Evaluate("(if '(0.0 1.25 2.5) 7.25 13.5)"));
            Assert.AreEqual("7.25", Evaluate("(if '(2.5 1.25 0.0) 7.25 13.5)"));
            Assert.AreEqual("13.5", Evaluate("(if (restruct '(2 2) '(0.0 1.5 2.0 3.5)) 7.25 13.5)"));
            Assert.AreEqual("7.25", Evaluate("(if (restruct '(2 2) '(3.5 2.0 1.5 0.0)) 7.25 13.5)"));
        }

        [Test]
        public void FloatWhileTest()
        {
            Evaluate(@"
(define whileTest (x decrement)
    (let ((counter 0))
        (begin
            (while x
                (begin
                    (set x (- x decrement))
                    (set counter (+1 counter))
                )
            )
            counter
        )
    )
)");

            Assert.AreEqual("3", Evaluate("(whileTest 13.5 4.5)"));
            Assert.AreEqual("5", Evaluate("(whileTest '(12.5 13.0 14.5) 2.5)"));
            Assert.AreEqual("13", Evaluate("(whileTest (restruct '(2 2) '(19.5 15.0 10.5 5.0)) 1.5)"));
        }

        [Test]
        public void FloatVectorAssignmentTest()  // See exercise 12 on page 90
        {
            Evaluate("(set vector1 '(10.0 20.25 30.5 40.75 50.0))");
            Evaluate("(:= vector1 4 13.125)");
            Assert.AreEqual("10.0 20.25 30.5 13.125 50.0", Evaluate("vector1"));

            Evaluate("(set vector2 '(10.0 20.25 30.5 40.75 50.0))");
            Evaluate("(:= vector2 '(3 5 1) '(7.5 9.5 11.5))");
            Assert.AreEqual("11.5 20.25 7.5 40.75 9.5", Evaluate("vector2"));
        }

        [Test]
        public void FloatDoubleSubscriptingTest()  // See exercise 13 on page 90
        {
            Assert.AreEqual("3.5 4.5" + lineEnding + "8.5 9.5", Evaluate("([;] (restruct '(5 5) (/ (indx 25) 2.0)) '(2 4) '(2 4))"));
            Assert.AreEqual("0.5 2.0 2.5" + lineEnding + "3.0 4.5 5.0", Evaluate("([;] (restruct '(4 5) (/ (indx 20) 2.0)) '(1 2) '(1 4 5))"));
        }

        #endregion

        [Test]
        public void CondTest()
        {
            Evaluate("(define condtest (n) (cond ((= n 1) 101) ((= n 2) 102) ((= n 3) 103) (1 107)))");

            Assert.AreEqual("107", Evaluate("(condtest 0)"));
            Assert.AreEqual("101", Evaluate("(condtest 1)"));
            Assert.AreEqual("102", Evaluate("(condtest 2)"));
            Assert.AreEqual("103", Evaluate("(condtest 3)"));
            Assert.AreEqual("107", Evaluate("(condtest 4)"));
        }

        [Test]
        public void LetTest()
        {
            Assert.AreEqual("5", Evaluate("(let ((n (+ 2 3))) n)"));
        }

        [Test]
        public void LetStarTest()
        {
            Assert.AreEqual("25", Evaluate("(let* ((x (+ 2 3)) (y (* x x))) y)"));
        }
    }
}
