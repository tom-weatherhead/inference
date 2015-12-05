using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.CLU;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.CLU
{
    [TestFixture]
    public class Parser_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly CLUGlobalInfo globalInfo;

        public Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.CLU);
            parser = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.CLU);
            globalInfo = new CLUGlobalInfo(tokenizer, parser);
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

        private ICLUValue EvaluateToICLUValue(string input)
        {
            var expr = GetParseResult(input) as ICLUExpression;

            Assert.IsNotNull(expr);

            var aplExpr = expr.Evaluate(globalInfo.GlobalEnvironment, null, globalInfo);

            Assert.IsNotNull(aplExpr);

            return aplExpr;
        }

        private string Evaluate(string input)
        {
            return EvaluateToICLUValue(input).ToString();
        }

        [Test]
        public void RecognizeTest()
        {
            // From Kamin page 211.
            parser.Recognize(tokenizer.Tokenize(@"(cluster Point
                (export new abscissa ordinate)
                (rep x-coord y-coord)
                (define new (x y) (Point x y))
                (define abscissa (p) (x-coord p))
                (define ordinate (p) (y-coord p))
                )"));
            parser.Recognize(tokenizer.Tokenize("(Point$compare p1 p2)"));  // Page 212
            parser.Recognize(tokenizer.Tokenize("(enclosed-area p1 p2)"));
        }

        [Test]
        public void PointTest()
        {
            const string pointCluster = @"
; From Kamin, page 211 (not page 214)
;
(cluster Point
    (export new abscissa ordinate reflect rotate compare quadrant)
    (rep x-coord y-coord)
    (define new (x y) (Point x y))
    (define abscissa (p) (x-coord p))
    (define ordinate (p) (y-coord p))
    (define reflect (p)
        (begin
            (set-x-coord p (- 0 (x-coord p)))
            (set-y-coord p (- 0 (y-coord p)))))
    (define rotate (p)
        (begin
            (set temp (x-coord p))
            (set-x-coord p (y-coord p))
            (set-y-coord p (- 0 temp))))
    (define compare (p1 p2) (< (sqrdist p1) (sqrdist p2)))
    (define quadrant (p)
        (if (>= (x-coord p) 0)
            (if (>= (y-coord p) 0) 1 2)
            (if (< (y-coord p) 0) 3 4)))
    ; sqrdist is not exported
    (define sqrdist (p) (+ (sqr (x-coord p)) (sqr (y-coord p))))
)";

            Assert.AreEqual("1", Evaluate("(define not (x) (if x 0 1))"));
            Assert.AreEqual("1", Evaluate("(define >= (x y) (not (< x y)))"));
            Assert.AreEqual("1", Evaluate("(define sqr (x) (* x x))"));
            Assert.AreEqual("1", Evaluate("(define abs (x) (if (< x 0) (- 0 x) x))"));
            Assert.AreEqual("1", Evaluate(pointCluster));
            Evaluate("(set p1 (Point$new 3 4))");
            Assert.AreEqual("3\r\n4", Evaluate("p1"));
            Evaluate("(Point$rotate p1)");
            Assert.AreEqual("4", Evaluate("(Point$abscissa p1)"));
            Assert.AreEqual("-3", Evaluate("(Point$ordinate p1)"));
            Evaluate("(Point$reflect p1)");
            Assert.AreEqual("-4", Evaluate("(Point$abscissa p1)"));
            Assert.AreEqual("3", Evaluate("(Point$ordinate p1)"));
            Evaluate("(set p2 (Point$new 1 5))");
            Assert.AreEqual("1", Evaluate("(Point$compare p1 p2)"));
            Assert.AreEqual("1", Evaluate(@"(define enclosed-area (p1 p2) (abs (* 
                (- (Point$abscissa p1) (Point$abscissa p2))
                (- (Point$ordinate p1) (Point$ordinate p2)))))"));
            Assert.AreEqual("10", Evaluate("(enclosed-area p1 p2)"));
        }

        [Test]
        public void ListTest()
        {
            const string listCluster = @"
; From Kamin, pages 216-217
;
(cluster List
    (export nil null? cons car cdr rplaca rplacd)
    (rep type a d)
    (define nil() (List 0 0 0))
    (define null? (l) (= (type l) 0))
    (define cons (item l) (List 1 item l))
    (define car (l) (a l)) ; apply selector a to l
    (define cdr (l) (d l)) ; apply selector d to l
    (define rplaca (l a) (set-a l a))
    (define rplacd (l d) (set-d l d))
)";
            Assert.AreEqual("1", Evaluate("(define +1 (n) (+ n 1))"));
            Assert.AreEqual("1", Evaluate(listCluster));
            Evaluate("(set x (List$cons 1 (List$cons 2 (List$nil))))");
            Evaluate("(set y x)");
            Assert.AreEqual("1", Evaluate("(List$car x)"));
            Assert.AreEqual("1", Evaluate("(List$car y)"));
            Assert.AreEqual("2", Evaluate("(List$car (List$cdr x))"));
            Evaluate("(List$rplaca y 3)");
            Assert.AreEqual("3", Evaluate("(List$car x)"));
            Assert.AreEqual("3", Evaluate("(List$car y)"));
            Assert.AreEqual("1", Evaluate("(define length (l) (if (List$null? l) 0 (+1 (length (List$cdr l)))))"));
            Assert.AreEqual("2", Evaluate("(length x)"));

            Assert.Throws<FunctionNotExportedException>(() => Evaluate("(List$type x)"));
            Assert.Throws<FunctionNotExportedException>(() => Evaluate("(List$set-type x 0)"));
        }

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

        [Test]
        public void AddBubbleDownTest() // 2013/12/04
        {
            // This tests a bug fix to CLUEnvironmentFrame<T>.AddBubbleDown(), where Add() used to be called unconditionally at the end of the function.
            Evaluate("(define test2 () foo)");
            Evaluate(@"
(define test1 () (begin
    (set foo 7) ; The buggy code was adding this variable to both the global and local frames.
    (set foo 13) ; The buggy code was modifying only the 'foo' in the local frame, since that's where AddBubbleDown() found 'foo' first.
    (test2) ; This returns the value of the 'foo' in the global frame.
))");

            Assert.AreEqual("13", Evaluate("(test1)"));
        }

        [Test]
        public void TwoLocalEnvironmentsTest() // 2013/12/05
        {
            Evaluate("(define test (x) (let ((y 13)) x))");

            Assert.AreEqual("7", Evaluate("(test 7)"));
        }

        [Test]
        public void TwoLocalEnvironmentsSetTest() // 2013/12/06
        {
            // To understand why this function contains two consecutive sets of the same variable, see the test AddBubbleDownTest().
            Evaluate("(define test (x) (begin (let ((y 13)) (begin (set x 19) (set x 20))) x))");

            Assert.AreEqual("20", Evaluate("(test 7)"));
        }
    }
}
