using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Interpreter.LISP;
using Inference.Interpreter.SASL;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.SASL
{
    [TestFixture]
    public class Parser_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly SASLGlobalInfo globalInfo;

        public Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.SASL);
            parser = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.SASL);
            globalInfo = new SASLGlobalInfo(tokenizer, parser);
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

        private ISExpression EvaluateToISExpression(string input)
        {
            var expr = GetParseResult(input) as IExpression<ISExpression>;

            Assert.IsNotNull(expr);

            var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);

            Assert.IsNotNull(sexpr);

            return sexpr;
        }

        private string Evaluate(string input)
        {
            return EvaluateToISExpression(input).ToString();
        }

        private string EvaluateAndForce(string variable, string expression)
        {
            Evaluate(string.Format("(set {0} {1})", variable, expression));
            Evaluate(string.Format("(force {0})", variable));
            return Evaluate(variable);
        }

        [Test]
        public void RecognizeTest()
        {
            parser.Recognize(tokenizer.Tokenize("+"));
        }

        [Test]
        public void InfiniteListTest()      // From Kamin, pages 160-161
        {
            Assert.AreEqual("(<thunk> <thunk>)", Evaluate("ints"));
            Assert.AreEqual("0", Evaluate("(car ints)"));
            Assert.AreEqual("(0 <thunk>)", Evaluate("ints"));
            Assert.AreEqual("1", Evaluate("(car (cdr ints))"));
            Assert.AreEqual("(0 1 <thunk>)", Evaluate("ints"));
        }

        [Test]
        public void FirstNTest()      // From Kamin, pages 161-162
        {
            Assert.AreEqual("(0 1 2 3 4)", EvaluateAndForce("ints5", "(first-n 5 ints)"));
        }

        [Test]
        public void NewtonRaphsonSquareRootTest()      // From Kamin, pages 162-163
        {
            Evaluate("(set next (lambda (n xi) (/ (+ xi (/ n xi)) 2)))");
            Evaluate("(set xlist (lambda (xi n) (cons xi (xlist (next n xi) n))))");
            Evaluate("(set mk-xlist (lambda (n) (xlist 1 n)))");
            Evaluate("(set abs-conv (lambda (epsilon) (lambda (l) (< (abs (- (cadr l) (car l))) epsilon))))");
            Evaluate(@"(set find-list (lambda (pred extract l)
                            (if (null? l) '() (if (pred l) (extract l)
                                (find-list pred extract (cdr l))))))");
            Evaluate("(set abs-sqrt (lambda (n) (find-list (abs-conv 3) cadr (mk-xlist n))))");

            Assert.AreEqual("10", Evaluate("(abs-sqrt 100)"));
        }

        [Test]
        public void PrimesTest()      // From Kamin, page 163
        {
            Evaluate("(set divides (lambda (a b) (= 0 (mod b a))))");
            Evaluate(@"(set remove-multiples (lambda (n l)
                            (if (null? l) '()
                                (if (divides n (car l))
                                    (remove-multiples n (cdr l))
                                    (cons (car l) (remove-multiples n (cdr l)))))))");
            Evaluate(@"(set sieve (lambda (l)
                            (if (null? l) '()
                                (cons (car l)
                                    (sieve (remove-multiples (car l) (cdr l)))))))");
            Evaluate(@"(set interval (lambda (i j)
                            (if (> i j) '() (cons i (interval (+1 i) j)))))");
            Evaluate("(set primes<= (lambda (n) (sieve (interval 2 n))))");
            Evaluate("(set primes (sieve (ints-from 2)))");
            Evaluate("(set first-n-primes (lambda (n) (first-n n primes)))");

            Assert.AreEqual("()", EvaluateAndForce("x", "(divides 7 13)"));
            Assert.AreEqual("T", EvaluateAndForce("x", "(divides 7 14)"));
            Assert.AreEqual("()", EvaluateAndForce("x", "(divides 14 7)"));
            Assert.AreEqual("(1 2 4 5 7 8)", EvaluateAndForce("x", "(remove-multiples 3 '(1 2 3 4 5 6 7 8 9))"));
            Assert.AreEqual("(2 3 4 5 6 7)", EvaluateAndForce("x", "(interval 2 7)"));
            Assert.AreEqual("(1 2 4 5 7 8)", EvaluateAndForce("x", "(remove-multiples 3 (interval 1 9))"));
            Assert.AreEqual("(2 3 5 7)", EvaluateAndForce("x", "(primes<= 7)"));
            //Assert.AreEqual("2", Evaluate("(car primes)"));
            //Assert.AreEqual("3", Evaluate("(cadr primes)"));
            //Assert.AreEqual("(2)", EvaluateAndForce("x", "(first-n-primes 1)"));
            Assert.AreEqual("(2 3 5 7)", EvaluateAndForce("x", "(first-n-primes 4)"));
            //Assert.AreEqual("(2 3 5 7 11 13 17 19 23 29)", EvaluateAndForce("x", "(first-n-primes 10)"));     // Very slow.
        }

        [Test]
        public void PermutationsTest()      // From Kamin, page 165
        {
            Evaluate(@"(set append* (lambda (l)
                            (if (null? l) '() (append (car l) (append* (cdr l))))))");
            Evaluate(@"(set filter (lambda (pred l)
                            (if (null? l) '()
                                (if (pred (car l)) (cons (car l) (filter pred (cdr l)))
                                    (filter pred (cdr l))))))");
            Evaluate(@"(set remove (lambda (item l)
                            (filter (lambda (x) (not (= x item))) l)))");
            Evaluate(@"(set permutations (lambda (l)
                            (if (= (length l) 1) (list l)
                                (append* (mapcar (lambda (x)
                                    (mapcar (lambda (z) (cons x z))
                                        (permutations (remove x l)))) l)))))");

            Assert.AreEqual("((1))", EvaluateAndForce("x", "(permutations '(1))"));
            Assert.AreEqual("((1 2) (2 1))", EvaluateAndForce("x", "(permutations '(1 2))"));
            Assert.AreEqual("((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))", EvaluateAndForce("x", "(permutations '(1 2 3))"));
        }

        [Test]
        public void RecursionSequencesTest()      // From Kamin, pages 165-166
        {
            //Evaluate("(set ints (cons 0 (mapcar +1 ints)))");
            Evaluate("(set double (lambda (n) (* n 2)))");
            Evaluate("(set powersof2 (cons 1 (mapcar double powersof2)))");
            Evaluate(@"(set mapcar2 (lambda (f l1 l2)
                            (cons (f (car l1) (car l2))
                                (mapcar2 f (cdr l1) (cdr l2)))))");
            Evaluate("(set posints (cdr ints))");
            Evaluate("(set facs (cons 1 (mapcar2 * facs posints)))");

            Assert.AreEqual("(1 2 4 8 16)", EvaluateAndForce("x", "(first-n 5 powersof2)"));
            Assert.AreEqual("(1 1 2 6 24)", EvaluateAndForce("x", "(first-n 5 facs)"));
        }

        [Test]
        public void SatisfiabilityTest()      // From Kamin, pages 170-172
        {
            Evaluate("(set cddr (lambda (l) (cdr (cdr l))))"); // cddr
            Evaluate("(set caddr (lambda (l) (car (cddr l))))"); // caddr
            Evaluate(@"
(set find (lambda (pred lis)
    (if (null? lis) '()
        (if (pred (car lis)) 'T (find pred (cdr lis))))))"); // find  : Page 104
            Evaluate("(set member? (lambda (x s) (find ((curry equal) x) s)))"); // member? : Page 105
            Evaluate("(set addelt (lambda (x s) (if (member? x s) s (cons x s))))"); // addelt : Page 105
            Evaluate("(set union (lambda (s1 s2) ((combine id addelt s1) s2)))"); // union : Page 105

            Evaluate("(set isTrue? member?)");
            Evaluate(@"
(set evalBoolexp (lambda (e a)
    (if (symbol? e) (isTrue? e a)
    (if (= (car e) 'not)
        (not (evalBoolexp (cadr e) a))
    (if (= (car e) 'or)
        (or (evalBoolexp (cadr e) a)
            (evalBoolexp (caddr e) a))
    (and (evalBoolexp (cadr e) a)
        (evalBoolexp (caddr e) a)))))))");
            Evaluate(@"
(set mapaddx (lambda (x l) ; add x to each list in l, then append to l
    (append l (mapcar (lambda (y) (cons x y)) l))))");
            Evaluate(@"
(set gensubsets (lambda (l) ; create a list containing all subsets of l
    (if (null? (cdr l)) (list l '())
        (mapaddx (car l) (gensubsets (cdr l))))))");
            Evaluate(@"
(set variables (lambda (e) ; All variables occurring in e
    (if (symbol? e) (cons e '())
        (if (= (car e) 'not) (variables (cadr e))
            (union (variables (cadr e)) (variables (caddr e)))))))");
            Evaluate("(set assignments (lambda (e) (gensubsets (variables e))))");
            Evaluate(@"
(set findTruth (lambda (e alist)
    ; Find if any assignment on alist satisfies e
    (if (null? alist) '() ; No assignments left to try
        (if (evalBoolexp e (car alist)) 'T
            (findTruth e (cdr alist))))))");
            Evaluate(@"
(set SAT (lambda (e)
    (if (findTruth e (assignments e))
        'Satisfiable
        'Unsatisfiable)))");

            Assert.AreEqual("Satisfiable", Evaluate("(SAT '(and p q))"));
            Assert.AreEqual("Satisfiable", Evaluate("(SAT '(or p q))"));
            Assert.AreEqual("Satisfiable", Evaluate("(SAT '(not p))"));
            Assert.AreEqual("Unsatisfiable", Evaluate("(SAT '(and p (not p)))"));
        }

        [Test]
        public void ReachabilityTest()      // From Kamin, pages 172-174
        {
            globalInfo.LoadPreset("queue");

            Evaluate(@"
(set add-points (lambda (p q)
    (list (+ (car p) (car q)) (+ (cadr p) (cadr q)))))");
            Evaluate(@"
(set gen-paths (lambda (p points)
    (cons p
        (mapcar (lambda (r) (gen-paths r points))
            (mapcar (lambda (q) (add-points q p)) points)))))");
            Evaluate("(set == (lambda (p q) (and (= (car p) (car q)) (= (cadr p) (cadr q)))))");
            Evaluate("(set << (lambda (p q) (or (< (car p) (car q)) (< (cadr p) (cadr q)))))");
            Evaluate(@"
(set dfs (lambda (t pred term) ; Depth-first search
    (if (pred (car t)) 'T ; success
        (if (term (car t)) '() ; failure on this branch
            (dfs* (cdr t) pred term)))))");
            Evaluate(@"
(set dfs* (lambda (l pred term)
    (if (null? l) '() ; failure
        (if (dfs (car l) pred term) 'T
            (dfs* (cdr l) pred term)))))");
            Evaluate(@"
(set reaches-dfs (lambda (p0 paths)
    (dfs paths
        (lambda (q) (== p0 q))
        (lambda (q) (<< p0 q)))))");

            Evaluate(@"
(set enqueue* (lambda (q items)
    (if (null? items) q (enqueue* (enqueue (car items) q) (cdr items)))))");

            Evaluate(@"
(set bfs-queue (lambda (q pred term)
    (if (empty? q) '()
        (if (pred (car (front q))) 'T
            (if (term (car (front q))) (bfs-queue (rm-front q) pred term)
                (bfs-queue (enqueue* (rm-front q) (cdr (front q)))
                    pred term))))))");
            Evaluate(@"
(set bfs (lambda (t pred term)
    (bfs-queue (enqueue t empty-queue) pred term)))");
            Evaluate(@"
(set reaches-bfs (lambda (p0 paths)
    (bfs paths
        (lambda (q) (== p0 q))
        (lambda (q) (<< p0 q)))))");

            //Evaluate("(set P '((2 2) (0 1) (3 0)))");
            Evaluate("(set P '((2 2) (0 1)))");     // Use this for the slower tests.
            Evaluate("(set PATHS (gen-paths '(0 0) P))");

            // Depth-first search.
            Assert.AreEqual("T", Evaluate("(reaches-dfs '(2 2) PATHS)"));   // A trivial test.
            Assert.AreEqual("T", Evaluate("(reaches-dfs '(4 6) PATHS)"));
            Assert.AreEqual("()", Evaluate("(reaches-dfs '(4 3) PATHS)"));

            // Queue tests:
            Evaluate("(set testQ1 empty-queue)");
            Evaluate("(set testQ2 (enqueue 1 testQ1))");
            Evaluate("(set testQ3 (enqueue 2 testQ2))");
            Evaluate("(set testQ4 (enqueue 3 testQ3))");
            Assert.AreEqual("()", Evaluate("(empty? testQ4)"));
            Assert.AreEqual("(1 2 3)", EvaluateAndForce("x", "testQ4"));
            Assert.AreEqual("1", Evaluate("(front testQ4)"));
            Evaluate("(set testQ5 (rm-front testQ4))");
            Assert.AreEqual("2", Evaluate("(front testQ5)"));
            Evaluate("(set testQ6 (rm-front testQ5))");
            Assert.AreEqual("3", Evaluate("(front testQ6)"));
            Evaluate("(set testQ7 (rm-front testQ6))");
            Assert.AreEqual("T", Evaluate("(empty? testQ7)"));

            // Breadth-first search.
            Assert.AreEqual("T", Evaluate("(reaches-bfs '(2 2) PATHS)"));       // A trivial test.
            Assert.AreEqual("T", Evaluate("(reaches-bfs '(2 3) PATHS)"));
            Assert.AreEqual("T", Evaluate("(reaches-bfs '(4 4) PATHS)"));
            //Assert.AreEqual("T", Evaluate("(reaches-bfs '(6 6) PATHS)"));       // Slow; about 27 seconds with only two vectors in P.
            //Assert.AreEqual("T", Evaluate("(reaches-bfs '(4 5) PATHS)"));       // Very slow; about 60 seconds with only two vectors in P.
            //Assert.AreEqual("T", Evaluate("(reaches-bfs '(4 6) PATHS)"));       // Did not complete after about 4.5 hours, even with only two vectors in P.
            Assert.AreEqual("()", Evaluate("(reaches-bfs '(1 1) PATHS)"));
            //Assert.AreEqual("()", Evaluate("(reaches-bfs '(4 3) PATHS)"));      // Extremely slow; about 15 minutes with only two vectors in P.
        }

        [Test]
        public void EvenNumberTest()      // See Kamin, page 199 (exercise 1a)
        {
            Evaluate("(set add2 (lambda (n) (+ n 2)))");
            Evaluate("(set evennumbers (cons 2 (mapcar add2 evennumbers)))");

            Assert.AreEqual("(2 4 6 8 10)", EvaluateAndForce("x", "(first-n 5 evennumbers)"));
        }

        [Test]
        public void XlistTest()      // See Kamin, page 199 (exercise 1b)
        {
            // xlist is used by the Newton-Raphson square root computation; see page 162
            Evaluate(@"
(set xlist (lambda (n)
    (let ((next (lambda (xi) (/ (+ xi (/ n xi)) 2))))
        (cons 1 (mapcar next (xlist n))))))");

            Assert.AreEqual("(1 50 26 14 10)", EvaluateAndForce("x", "(first-n 5 (xlist 100))"));
        }

        [Test]
        public void BinomialTest()      // See Kamin, page 199 (exercise 1c)
        {
            // Generate the list of all lists of binomial coefficients.
            Evaluate(@"
(set mapcar2 (lambda (f l1 l2)
    (if (or (null? l1) (null? l2)) '()
        (cons (f (car l1) (car l2))
            (mapcar2 f (cdr l1) (cdr l2))))))");
            Evaluate("(set binomial (cons '(1) (mapcar (lambda (l) (mapcar2 + (cons 0 l) (append l '(0)))) binomial)))");

            Assert.AreEqual("((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1))", EvaluateAndForce("x", "(first-n 5 binomial)"));
        }

        [Test]
        public void FibonacciTest()      // See Kamin, page 199 (exercise 1d)
        {
            Evaluate(@"
(set mapcar2 (lambda (f l1 l2)
    (cons (f (car l1) (car l2))
        (mapcar2 f (cdr l1) (cdr l2)))))");
            Evaluate("(set fibonacci (cons 0 (cons 1 (mapcar2 + fibonacci (cdr fibonacci)))))");

            Assert.AreEqual("(0 1 1 2 3 5 8 13 21 34 55 89 144)", EvaluateAndForce("x", "(first-n 13 fibonacci)"));
        }

        [Test]
        public void IdentityTest()  // Ensure that (id 0) returns 0, not a thunk.
        {
            Assert.AreEqual("0", Evaluate("(id 0)"));
        }

        [Test]
        public void ListTest()
        {
            Assert.AreEqual("()", Evaluate("(list)"));
            Assert.AreEqual("(1)", EvaluateAndForce("l", "(list 1)"));
            Assert.AreEqual("(1 2 3)", EvaluateAndForce("l", "(list 1 2 3)"));
            Assert.AreEqual("(1 + T)", EvaluateAndForce("l", "(list 1 + 'T)"));
        }

        [Test]
        public void Any2Test()
        {
            Assert.AreEqual("()", Evaluate("(any2 '(() () () ()))"));
            Assert.AreEqual("T", Evaluate("(any2 '(() T () T))"));
            Assert.AreEqual("T", Evaluate("(any2 '(T T T T))"));
        }

        [Test]
        public void All2Test()
        {
            Assert.AreEqual("()", Evaluate("(all2 '(() () () ()))"));
            Assert.AreEqual("()", Evaluate("(all2 '(() T () T))"));
            Assert.AreEqual("T", Evaluate("(all2 '(T T T T))"));
        }

        [Test]
        public void CartesianTest()
        {
            // Compute the Cartesian product of two lists.
            Evaluate("(set mapcar-aux1 (lambda (f) (lambda (e1 l2) (if (null? l2) '() (cons (f e1 (car l2)) ((mapcar-aux1 f) e1 (cdr l2)))))))");
            Evaluate("(set combine-aux1 (lambda (f sum zero) (lambda (l1 l2) (if (null? l1) zero (sum (f (car l1) l2) ((combine-aux1 f sum zero) (cdr l1) l2))))))");
            Evaluate("(set cartesian (combine-aux1 (mapcar-aux1 list) append '()))");

            Assert.AreEqual("((1 3) (1 4) (2 3) (2 4))", EvaluateAndForce("c", "(cartesian '(1 2) '(3 4))"));
        }

        [Test]
        public void DotTest()
        {
            Assert.AreEqual("(1 2 . 3)", EvaluateAndForce("x", "(cons 1 (cons 2 3))"));
            Assert.AreEqual("(1 2 . 3)", EvaluateAndForce("x", "'(1 2 . 3)"));
            Assert.AreEqual("(1 2 3 4)", EvaluateAndForce("x", "'(1 2 . (3 4))"));
        }

        [Test]
        public void AndTest()
        {
            Assert.AreEqual("()", Evaluate("(and '() '())"));
            Assert.AreEqual("()", Evaluate("(and '() 'T)"));
            Assert.AreEqual("()", Evaluate("(and 'T '())"));
            Assert.AreEqual("T", Evaluate("(and 'T 'T)"));
        }

        [Test]
        public void OrTest()
        {
            Assert.AreEqual("()", Evaluate("(or '() '())"));
            Assert.AreEqual("T", Evaluate("(or '() 'T)"));
            Assert.AreEqual("T", Evaluate("(or 'T '())"));
            Assert.AreEqual("T", Evaluate("(or 'T 'T)"));
        }

        [Test]
        public void GlobalVsLocalVariableTest()
        {
            Evaluate("(set a 1)");
            Evaluate("(set afunc (lambda () a))");
            Evaluate("(set func2 (lambda (a) (afunc)))");

            Assert.AreEqual("1", Evaluate("(func2 0)"));
        }

        [Test]
        public void LetRecTest1()    // From Kamin page 126.
        {
            // A test of letrec using a recursively-defined function.
            const string str = @"(letrec
                                    ((countones (lambda (l)
                                        (if (null? l) 0
                                            (if (= (car l) 1) (+ 1 (countones (cdr l)))
                                                (countones (cdr l)))))))
                                    (countones '(1 2 3 1 0 1 1 5)))";

            Assert.AreEqual("4", Evaluate(str));
        }

        [Test]
        public void LetRecTest2()
        {
            // A test of letrec using a recursively-defined sequence.
            const string str = @"(letrec
                                    ((ints10 (cons 10 (mapcar +1 ints10))))
                                    (first-n 5 ints10))";

            Assert.AreEqual("(10 11 12 13 14)", EvaluateAndForce("x", str));
        }

        [Test]
        public void Exercise2aTest()    // See Kamin page 199
        {
            Evaluate(@"
(set filter-fun (lambda (f pred l)
    (if (null? l) '()
        (if (pred (car l)) (cons (f (car l)) (filter-fun f pred (cdr l)))
            (filter-fun f pred (cdr l))))))");
            Evaluate("(set sqr (lambda (x) (* x x)))");
            Evaluate("(set even? (lambda (x) (= 0 (mod x 2))))");
            Evaluate("(set z (filter-fun sqr even? ints))");

            Assert.AreEqual("(0 4 16 36 64 100)", EvaluateAndForce("x", "(first-n 6 z)"));
        }

        // Exercise 2b) : mkfinitepairs : See CartesianTest()

        [Test]
        public void Exercise2cTest()    // See Kamin page 200
        {
            Evaluate("(set mapcar-aux1 (lambda (f) (lambda (e1 l2-list) (if (null? l2-list) '() (cons (f e1 (car l2-list)) ((mapcar-aux1 f) e1 (cdr l2-list)))))))");
            Evaluate("(set combine-aux1 (lambda (f sum zero) (lambda (l1 l2) (if (null? l1) zero (sum (f (car l1) (l2 (car l1))) ((combine-aux1 f sum zero) (cdr l1) l2))))))");
            Evaluate("(set mkfinitepairs* (combine-aux1 (mapcar-aux1 list) append '()))");
            Evaluate("(set m-to-n (lambda (m n) (if (> m n) '() (cons m (m-to-n (+1 m) n)))))");

            Assert.AreEqual("((1 1) (1 2) (2 2) (2 3) (3 3) (3 4) (4 4) (4 5) (5 5) (5 6))",
                EvaluateAndForce("c", "(mkfinitepairs* (m-to-n 1 5) (lambda (m) (m-to-n m (+1 m))))"));
        }

        [Test]
        public void Exercise2defgTest()    // See Kamin pages 200-201
        {
            // Exercise 2d)
            // 1)
            Evaluate("(set mklistofpairs (lambda (l1 l2) (mapcar (lambda (e1) (mapcar ((curry list) e1) l2)) l1)))");

            // 2)
            Evaluate(@"
(set merge (lambda (l1 l2)
    (if (null? l1) l2
        (if (null? l2) l1
            (cons (car l1) (cons (car l2) (merge (cdr l1) (cdr l2))))))))");

            // 3)
            Evaluate(@"
(set interleave (lambda (l)
    (if (null? l) '()
        (if (null? (car l))
            (interleave (cdr l))
            (cons (car (car l)) (merge (interleave (cdr l)) (cdr (car l))))))))");

            Evaluate("(set mkpairs (lambda (l1 l2) (interleave (mklistofpairs l1 l2))))");

            Assert.AreEqual("((1 3) (2 3) (1 4) (2 4))", EvaluateAndForce("x", "(mkpairs '(1 2) '(3 4))"));
            Assert.AreEqual("((0 0) (1 0) (0 1) (2 0) (0 2) (1 1) (0 3) (3 0) (0 4) (1 2))",
                EvaluateAndForce("x", "(first-n 10 (mkpairs ints ints))"));

            // Exercise 2e)
            Evaluate("(set mklistofpairs* (lambda (l1 l2) (mapcar (lambda (e1) (mapcar ((curry list) e1) (l2 e1))) l1)))");
            Evaluate("(set mkpairs* (lambda (l1 l2) (interleave (mklistofpairs* l1 l2))))");

            Evaluate("(set ints-from1 (ints-from 1))");
            Evaluate("(set multiples (lambda (n) (mapcar ((curry *) n) ints-from1)))");
            Assert.AreEqual("((1 1) (2 2) (1 2) (3 3) (1 3) (2 4) (1 4) (4 4) (1 5) (2 6))",
                EvaluateAndForce("x", "(first-n 10 (mkpairs* ints-from1 multiples))"));

            // Exercise 2f) : This is the slow part of the test.
            Evaluate(@"
(set filter (lambda (pred l)
    (if (null? l) '()
        (if (pred (car l)) (cons (car l) (filter pred (cdr l)))
            (filter pred (cdr l))))))");    // From page 165.
            Evaluate("(set filter-pair (lambda (pred l1 l2) (filter pred (interleave (mklistofpairs l1 l2)))))");

            Evaluate("(set relatively-prime? (lambda (x) (= 1 (gcd (car x) (cadr x)))))");
            Evaluate("(set ints-from2 (ints-from 2))");
            Evaluate("(set relative-primes (filter-pair relatively-prime? ints-from2 ints-from2))");
#if DEAD_CODE
            Assert.AreEqual("((3 2) (2 3) (2 5) (5 2) (3 4) (2 7) (4 3) (3 5) (2 9) (2 11))",
                EvaluateAndForce("x", "(first-n 10 relative-primes)")); // Slow; several seconds.
#else
            Assert.AreEqual("((3 2) (2 3) (2 5) (5 2) (3 4))",
                EvaluateAndForce("x", "(first-n 5 relative-primes)"));
#endif

            Evaluate("(set filter-pair* (lambda (pred l1 l2) (filter pred (interleave (mklistofpairs* l1 l2)))))");

            Evaluate("(set relative-primes* (filter-pair* relatively-prime? ints-from2 ints-from))");
#if DEAD_CODE
            Assert.AreEqual("((2 3) (3 4) (2 5) (3 5) (2 7) (4 5) (2 9) (3 7) (2 11) (3 8))",
                EvaluateAndForce("x", "(first-n 10 relative-primes*)")); // Slow; several seconds.
#else
            Assert.AreEqual("((2 3) (3 4) (2 5) (3 5) (2 7))",
                EvaluateAndForce("x", "(first-n 5 relative-primes*)"));
#endif

            // Exercise 2g)
            Evaluate("(set mklistofpairsf (lambda (f l1 l2) (mapcar (lambda (e1) (mapcar ((curry f) e1) l2)) l1)))");
            Evaluate("(set filter-pair-fun (lambda (f pred l1 l2) (filter pred (interleave (mklistofpairsf f l1 l2)))))");

            Evaluate("(set sorted-pairs (filter-pair-fun list (lambda (x) (< (car x) (cadr x))) ints-from1 ints-from1))");
#if !DEAD_CODE
            Assert.AreEqual("((1 2) (1 3) (1 4) (1 5) (2 3) (1 6) (1 7) (2 4) (1 8) (1 9))",
                EvaluateAndForce("x", "(first-n 10 sorted-pairs)")); // It takes about 0.4 seconds longer to generate the first 10 than the first 5.
#else
            Assert.AreEqual("((1 2) (1 3) (1 4) (1 5) (2 3))",
                EvaluateAndForce("x", "(first-n 5 sorted-pairs)"));
#endif

            Evaluate("(set mklistofpairsf* (lambda (f l1 l2) (mapcar (lambda (e1) (mapcar ((curry f) e1) (l2 e1))) l1)))");
            Evaluate("(set filter-pair-fun* (lambda (f pred l1 l2) (filter pred (interleave (mklistofpairsf* f l1 l2)))))");

            Evaluate(@"
(set permutations (lambda (x)
    (if (null? x) '(())
        (filter-pair-fun*
            cons
            (lambda (a) 'T)
            x
            (lambda (a)
                (permutations
                    (filter
                        ;(lambda (b) (not (= a b))) ; Experiment: We are trying = instead of equal
                        ((curry <>) a) ; Experiment: We are trying <> (which uses =) instead of (not (equal ...))
                        x)))))))");
            Assert.AreEqual("((1 2 3) (2 1 3) (1 3 2) (3 1 2) (2 3 1) (3 2 1))",
                EvaluateAndForce("x", "(permutations '(1 2 3))"));
#if DEAD_CODE
            Evaluate("(set first-n* (lambda (m n l) (mapcar ((curry first-n) n) (first-n m l))))");
            Assert.AreEqual("((1))", EvaluateAndForce("x", "(first-n* 1 1 (permutations ints-from1))"));  // Very slow; did not finish even after about an hour.
#endif
        }

        [Test]
        public void CondTest()  // Test of cond as an SASL value op.
        {
            Evaluate("(set condtest (lambda (n) (cond ((= n 1) 'First) ((= n 2) 'Second) ((= n 3) 'Third) ('T 'Other))))");

            Assert.AreEqual("Other", Evaluate("(condtest 0)"));
            Assert.AreEqual("First", Evaluate("(condtest 1)"));
            Assert.AreEqual("Second", Evaluate("(condtest 2)"));
            Assert.AreEqual("Third", Evaluate("(condtest 3)"));
            Assert.AreEqual("Other", Evaluate("(condtest 4)"));
        }

        [Test]
        public void Find2Test() // 2013/11/29
        {
            // **** find2 and contains2 ****
            // find2 works because "and" and "or" are short-circuiting, which is because they are defined using "if",
            // which converts its arguments into thunks before evaluation.
            Evaluate("(set find2 (lambda (pred lis) (and (not (null? lis)) (or (pred (car lis)) (find2 pred (cdr lis))))))");
            //Evaluate("(set contains2 (lambda (l a) (find2 (lambda (b) (= a b)) l)))");
            Evaluate("(set contains2 (lambda (l a) (find2 ((curry =) a) l)))");

            Assert.AreEqual("T", Evaluate("(contains2 '(2 3 5 7) 5)"));
            Assert.AreEqual("()", Evaluate("(contains2 '(2 3 5 7) 4)"));

            //Evaluate("(set curried_reversed_contains2 (lambda (a) ((curry find2) (lambda (b) (= a b)))))");
            Evaluate("(set curried_reversed_contains2 (lambda (a) ((curry find2) ((curry =) a))))");

            Assert.AreEqual("T", Evaluate("((curried_reversed_contains2 5) '(2 3 5 7))"));
            Assert.AreEqual("()", Evaluate("((curried_reversed_contains2 4) '(2 3 5 7))"));

            // **** find3 and contains3 ****
            Evaluate("(set find3 (lambda (pred lis) ((combine pred or '()) lis)))");
            Evaluate("(set contains3 (lambda (l a) (find3 ((curry =) a) l)))");

            Assert.AreEqual("T", Evaluate("(contains3 '(2 3 5 7) 5)"));
            Assert.AreEqual("()", Evaluate("(contains3 '(2 3 5 7) 4)"));

            // **** find4 and contains4 ****
            Evaluate("(set find4 (lambda (pred lis) (any (mapcar pred lis))))");
            Evaluate("(set contains4 (lambda (l a) (find4 ((curry =) a) l)))");

            Assert.AreEqual("T", Evaluate("(contains4 '(2 3 5 7) 5)"));
            Assert.AreEqual("()", Evaluate("(contains4 '(2 3 5 7) 4)"));
        }

        [Test]
        public void AlternativeIfTest() // 2013/12/03
        {
            Evaluate("(set true-alt (lambda (t f) t))");
            Evaluate("(set false-alt (lambda (t f) f))");
            Evaluate("(set if-alt (lambda (x y z) (x y z)))");
            Evaluate("(set printable-boolean (lambda (x) (if-alt x 'T '())))");
            Evaluate(@"
(set find-factory (lambda (found-value not-found-value)
    (letrec
        ((loop (lambda (pred lis)
            (cond
                ((null? lis) not-found-value)
                ((pred (car lis)) found-value)
                ('T (loop pred (cdr lis)))
            )
        )))
        loop
    )
))");
            Evaluate("(set find-alt (find-factory true-alt false-alt))");
            Evaluate("(set contains-alt (lambda (x l) (find-alt ((curry =) x) l)))");

            Assert.AreEqual("T", Evaluate("(printable-boolean (contains-alt 5 '(2 3 5 7)))"));
            Assert.AreEqual("()", Evaluate("(printable-boolean (contains-alt 4 '(2 3 5 7)))"));

            Evaluate("(set and-alt (lambda (x y) (if-alt x y x)))");
            Evaluate("(set or-alt (lambda (x y) (if-alt x x y)))");

            Assert.AreEqual("()", Evaluate("(printable-boolean (and-alt false-alt false-alt))"));
            Assert.AreEqual("()", Evaluate("(printable-boolean (and-alt false-alt true-alt))"));
            Assert.AreEqual("()", Evaluate("(printable-boolean (and-alt true-alt false-alt))"));
            Assert.AreEqual("T", Evaluate("(printable-boolean (and-alt true-alt true-alt))"));

            Assert.AreEqual("()", Evaluate("(printable-boolean (or-alt false-alt false-alt))"));
            Assert.AreEqual("T", Evaluate("(printable-boolean (or-alt false-alt true-alt))"));
            Assert.AreEqual("T", Evaluate("(printable-boolean (or-alt true-alt false-alt))"));
            Assert.AreEqual("T", Evaluate("(printable-boolean (or-alt true-alt true-alt))"));
        }
    }
}
