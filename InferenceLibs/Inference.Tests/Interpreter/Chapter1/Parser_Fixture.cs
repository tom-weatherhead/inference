using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Interpreter.Chapter1;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.Chapter1
{
    [TestFixture]
    public class Parser_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly GlobalInfo globalInfo;

        public Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.InterpreterChapter1);
            parser = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.InterpreterChapter1);
            globalInfo = new GlobalInfo(tokenizer, parser);
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

        private int Evaluate(string input)
        {
            var expr = GetParseResult(input) as IExpression<int>;

            Assert.IsNotNull(expr);

            return expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);
        }

        [Test]
        public void RecognizeTest()
        {
            parser.Recognize(tokenizer.Tokenize("(define double (x) (+ x x))"));
            parser.Recognize(tokenizer.Tokenize("(double 5)"));

            parser.Recognize(tokenizer.Tokenize("(define +1 (n) (+ n 1))"));
            parser.Recognize(tokenizer.Tokenize("(+1 5)"));

            parser.Recognize(tokenizer.Tokenize("(define not (boolval) (if boolval 0 1))"));
            parser.Recognize(tokenizer.Tokenize("(define <> (x y) (not (= x y)))"));

            parser.Recognize(tokenizer.Tokenize("(define mod (m n) (- m (* n (/ m n))))"));
            parser.Recognize(tokenizer.Tokenize("(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"));
        }

        [Test]
        public void ParseIntegerLiteralTest()
        {
            var value = 13;
            var expr = parser.Parse(tokenizer.Tokenize(value.ToString())) as IExpression<int>;

            Assert.IsNotNull(expr);
            Assert.AreEqual(value, expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo));
        }

        [Test]
        public void AdditionTest()
        {
            var value = "(+ 2 3)";
            var tokenList = tokenizer.Tokenize(value);

            Assert.AreEqual(6, tokenList.Count);
            Assert.AreEqual(TokenType.T_LeftBracket, tokenList[0].TokenType);
            Assert.AreEqual(TokenType.T_Ident, tokenList[1].TokenType);
            Assert.IsTrue(tokenList[1].TokenValue is string);
            Assert.AreEqual("+", tokenList[1].TokenValue as string);
            Assert.AreEqual(TokenType.T_IntLit, tokenList[2].TokenType);
            Assert.IsTrue(tokenList[2].TokenValue is int);
            Assert.AreEqual(2, (int)tokenList[2].TokenValue);
            Assert.AreEqual(TokenType.T_IntLit, tokenList[3].TokenType);
            Assert.IsTrue(tokenList[3].TokenValue is int);
            Assert.AreEqual(3, (int)tokenList[3].TokenValue);
            Assert.AreEqual(TokenType.T_RightBracket, tokenList[4].TokenType);
            Assert.AreEqual(TokenType.T_EOF, tokenList[5].TokenType);

            var parseResult = parser.Parse(tokenizer.Tokenize(value));

            Assert.IsNotNull(parseResult);
            Assert.IsTrue(parseResult is OperatorUsage<int>);

            var ou = parseResult as OperatorUsage<int>;

            Assert.IsNotNull(ou.OperatorName);
            Assert.IsNotNull(ou.OperatorName.Value);
            Assert.AreEqual("+", ou.OperatorName.Value);
            Assert.AreEqual(2, ou.ExpressionList.Value.Count);
            Assert.AreEqual(value, parseResult.ToString());

            var expr = parseResult as IExpression<int>;

            Assert.IsNotNull(expr);

            var result = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);

            Assert.AreEqual(5, result);
        }

        [Test]
        public void MultiOperatorTest1()
        {
            Assert.AreEqual(100, Evaluate("(- (* 8 (+ 12 1)) (/ (+ 16 4) 5))"));
        }

        [Test]
        public void InequalityTest()
        {
            Assert.AreEqual(1, Evaluate("(< 1 2)"));
            Assert.AreEqual(0, Evaluate("(< 2 2)"));
            Assert.AreEqual(0, Evaluate("(< 3 2)"));

            Assert.AreEqual(0, Evaluate("(> 1 2)"));
            Assert.AreEqual(0, Evaluate("(> 2 2)"));
            Assert.AreEqual(1, Evaluate("(> 3 2)"));
        }

        [Test]
        public void IfTest()
        {
            Assert.AreEqual(7, Evaluate("(if (> 2 1) 7 13)"));
            Assert.AreEqual(13, Evaluate("(if (< 2 1) 7 13)"));
        }

        [Test]
        public void EqualsTest()
        {
            Assert.AreEqual(1, Evaluate("(= 0 0)"));
            Assert.AreEqual(1, Evaluate("(= 1 1)"));
            Assert.AreEqual(0, Evaluate("(= 0 1)"));
            Assert.AreEqual(0, Evaluate("(= 1 0)"));
            Assert.AreEqual(1, Evaluate("(= (+ 2 3) 5)"));
        }

        [Test]
        public void WhileTest()
        {
            const string whileTest = @"
(begin
    (set x 1)
    (set acc 0)
    (while (< x 5) (begin
        (set acc (+ acc x))
        (set x (+ x 1))
    ))
    acc
)";

            Assert.AreEqual(10, Evaluate(whileTest));
        }

        [Test]
        public void SetTest()
        {
            Assert.AreEqual(2, Evaluate("(set globalN 2)"));
            Assert.AreEqual(2, Evaluate("globalN"));
            Assert.AreEqual(3, Evaluate("(+ globalN 1)"));
        }

        [Test]
        public void BeginTest()
        {
            Assert.AreEqual(13, Evaluate("(begin 1 1 2 3 5 8 13)"));
        }

        [Test]
        public void FuncDefTest()
        {
            Assert.AreEqual(1, Evaluate("(define increment (n) (+ n 1))"));
            Assert.AreEqual(14, Evaluate("(increment 13)"));
        }

        [Test]
        public void GCDFunctionTest()
        {
            Assert.AreEqual(3, Evaluate("(mod 101 7)"));

            Assert.AreEqual(0, Evaluate("(gcd 0 0)"));
            Assert.AreEqual(7, Evaluate("(gcd 0 7)"));
            Assert.AreEqual(7, Evaluate("(gcd 7 0)"));
            Assert.AreEqual(7, Evaluate("(gcd 343 91)"));
            Assert.AreEqual(1, Evaluate("(gcd 31 29)"));
        }

        [Test]
        public void SigmaTest()     // Exercise 1, on page 18
        {
            Evaluate("(define sigma (m n) (if (> m n) 0 (+ m (sigma (+ m 1) n))))");

            Assert.AreEqual(0, Evaluate("(sigma 2 1)"));
            Assert.AreEqual(10, Evaluate("(sigma 1 4)"));
            Assert.AreEqual(75, Evaluate("(sigma 10 15)"));
        }

        [Test]
        public void ExpLogTest()     // Exercise 2, on page 18
        {
            Evaluate("(define exp (m n) (if (= n 0) 1 (* m (exp m (- n 1)))))");

            Evaluate("(define log-aux (l x m n) (if (> x n) l (log-aux (+ l 1) (* x m) m n)))");    // x == m ^ (l + 1), m > 1, n > 0
            Evaluate("(define log (m n) (log-aux 0 m m n))");

            Assert.AreEqual(1, Evaluate("(exp 10 0)"));
            Assert.AreEqual(1000, Evaluate("(exp 10 3)"));
            Assert.AreEqual(1024, Evaluate("(exp 2 10)"));

            Assert.AreEqual(2, Evaluate("(log 10 999)"));
            Assert.AreEqual(3, Evaluate("(log 10 1000)"));
            Assert.AreEqual(3, Evaluate("(log 10 1001)"));
        }

        [Test]
        public void BinomialTest()     // Exercise 3, on page 18
        {
            Evaluate("(define or (x y) (if x x y))");
            Evaluate(@"
(define choose (n k)
    (if (or (= k 0) (= k n)) 1
        (+ (choose (- n 1) k) (choose (- n 1) (- k 1)))))");

            Assert.AreEqual(1, Evaluate("(choose 4 0)"));
            Assert.AreEqual(4, Evaluate("(choose 4 1)"));
            Assert.AreEqual(6, Evaluate("(choose 4 2)"));
            Assert.AreEqual(4, Evaluate("(choose 4 3)"));
            Assert.AreEqual(1, Evaluate("(choose 4 4)"));
        }

        [Test]
        public void FibonacciTest()     // Exercise 4, on page 18
        {
            Evaluate("(define fib (m) (if (< m 2) m (+ (fib (- m 1)) (fib (- m 2)))))");

            Assert.AreEqual(0, Evaluate("(fib 0)"));
            Assert.AreEqual(1, Evaluate("(fib 1)"));
            Assert.AreEqual(1, Evaluate("(fib 2)"));
            Assert.AreEqual(2, Evaluate("(fib 3)"));
            Assert.AreEqual(3, Evaluate("(fib 4)"));
            Assert.AreEqual(5, Evaluate("(fib 5)"));
            Assert.AreEqual(8, Evaluate("(fib 6)"));
        }

        [Test]
        public void PrimeTest()     // Exercise 5, on page 18
        {
            Evaluate(@"
(define prime-aux (m n)
    (if (> (* m m) n) 1
        (if (= 0 (mod n m)) 0
            (prime-aux (+ m 1) n))))");
            Evaluate("(define prime (n) (prime-aux 2 n))");

            Assert.AreEqual(1, Evaluate("(prime 2)"));
            Assert.AreEqual(1, Evaluate("(prime 3)"));
            Assert.AreEqual(0, Evaluate("(prime 4)"));
            Assert.AreEqual(1, Evaluate("(prime 5)"));
            Assert.AreEqual(0, Evaluate("(prime 6)"));
            Assert.AreEqual(1, Evaluate("(prime 7)"));
            Assert.AreEqual(0, Evaluate("(prime 8)"));
            Assert.AreEqual(0, Evaluate("(prime 9)"));
            Assert.AreEqual(0, Evaluate("(prime 10)"));

            Evaluate(@"
(define nthprime-aux (m n)
    (if (prime m)
        (if (= n 1) m
            (nthprime-aux (+ m 1) (- n 1)))
        (nthprime-aux (+ m 1) n)))");
            Evaluate("(define nthprime (n) (nthprime-aux 2 n))");

            Assert.AreEqual(2, Evaluate("(nthprime 1)"));
            Assert.AreEqual(3, Evaluate("(nthprime 2)"));
            Assert.AreEqual(5, Evaluate("(nthprime 3)"));
            Assert.AreEqual(7, Evaluate("(nthprime 4)"));
            Assert.AreEqual(11, Evaluate("(nthprime 5)"));

            Evaluate("(define sumprimes (n) (if (< n 1) 0 (+ (nthprime n) (sumprimes (- n 1)))))");

            Assert.AreEqual(2, Evaluate("(sumprimes 1)"));
            Assert.AreEqual(5, Evaluate("(sumprimes 2)"));
            Assert.AreEqual(10, Evaluate("(sumprimes 3)"));
            Assert.AreEqual(17, Evaluate("(sumprimes 4)"));
            Assert.AreEqual(28, Evaluate("(sumprimes 5)"));

            Evaluate("(define relprime (m n) (= 1 (gcd m n)))");

            Assert.AreEqual(0, Evaluate("(relprime 12 12)"));
            Assert.AreEqual(0, Evaluate("(relprime 12 14)"));
            Assert.AreEqual(0, Evaluate("(relprime 14 12)"));
            Assert.AreEqual(1, Evaluate("(relprime 12 25)"));
        }

        [Test]
        public void BinaryTest()     // Exercise 6, on page 19
        {
            Evaluate("(define binary (m) (if (= m 0) 0 (+ (mod m 2) (* 10 (binary (/ m 2))))))");

            Assert.AreEqual(1100, Evaluate("(binary 12)"));
        }

        [Test]
        public void CondTest()
        {
            Evaluate("(define condtest (n) (cond ((= n 1) 101) ((= n 2) 102) ((= n 3) 103) (1 107)))");

            Assert.AreEqual(107, Evaluate("(condtest 0)"));
            Assert.AreEqual(101, Evaluate("(condtest 1)"));
            Assert.AreEqual(102, Evaluate("(condtest 2)"));
            Assert.AreEqual(103, Evaluate("(condtest 3)"));
            Assert.AreEqual(107, Evaluate("(condtest 4)"));
        }

        [Test]
        public void LetTest()
        {
            Assert.AreEqual(5, Evaluate("(let ((n (+ 2 3))) n)"));
        }

        [Test]
        public void LetStarTest()
        {
            Assert.AreEqual(25, Evaluate("(let* ((x (+ 2 3)) (y (* x x))) y)"));
        }

        [Test]
        public void AddBubbleDownTest() // 2013/12/04
        {
            // This tests a bug fix to EnvironmentFrame<T>.AddBubbleDown(), where Add() used to be called unconditionally at the end of the function.
            Evaluate("(define test2 () foo)");
            Evaluate(@"
(define test1 () (begin
    (set foo 7) ; The buggy code was adding this variable to both the global and local frames.
    (set foo 13) ; The buggy code was modifying only the 'foo' in the local frame, since that's where AddBubbleDown() found 'foo' first.
    (test2) ; This returns the value of the 'foo' in the global frame.
))");

            Assert.AreEqual(13, Evaluate("(test1)"));
        }

        [Test]
        public void TwoLocalEnvironmentsTest() // 2013/12/05
        {
            Evaluate("(define test (x) (let ((y 13)) x))");

            Assert.AreEqual(7, Evaluate("(test 7)"));
        }

        [Test]
        public void TwoLocalEnvironmentsSetTest() // 2013/12/06
        {
            // To understand why this function contains two consecutive sets of the same variable, see the test AddBubbleDownTest().
            Evaluate("(define test (x) (begin (let ((y 13)) (begin (set x 19) (set x 20))) x))");

            Assert.AreEqual(20, Evaluate("(test 7)"));
        }
    }
}
