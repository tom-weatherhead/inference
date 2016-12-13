using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.LISP;
using Inference.Parser;

namespace Inference.Interpreter.Scheme
{
    #region Interfaces

    public interface ICallableSExpression : ISExpression
    {
        int ExpectedNumArgs { get; }
        int Line { get; }
        int Column { get; }
        ISExpression Call(ExpressionList<ISExpression> arguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo);
    }

    #endregion

    #region PrimOp

    public class PrimOp : LISPOperatorUsage, ICallableSExpression // We cannot inherit from SExpressionBase here bacause we already inherit from LISPOperatorUsage.
    {
        public PrimOp(Name operatorName)
            : base(operatorName, new ExpressionList<ISExpression>())
        {
            int expectedNumArgs = 0;
            bool gotExpectedNumArgs = false;

            try
            {
                // We can pass in null for the globalInfo because no PrimOp is a global function or a macro; the grammar protects us from crashes.
                // TODO: Or we could check for a null globalInfo in OperatorUsage<T>.
                gotExpectedNumArgs = TryGetExpectedNumArgs(null, out expectedNumArgs);
            }
            catch
            {
            }

            if (!gotExpectedNumArgs)
            {
                throw new Exception(string.Format("PrimOp constructor: Failed to get ExpectedNumArgs for operator '{0}'.", operatorName.Value));
            }

            ExpectedNumArgs = expectedNumArgs;
        }

        public int ExpectedNumArgs { get; private set; }

        public int Line
        {
            get
            {
                return OperatorName.Line;
            }
        }

        public int Column
        {
            get
            {
                return OperatorName.Column;
            }
        }

        public override string ToString()
        {
            return OperatorName.ToString();
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            PrimOp otherPrimOp = obj as PrimOp;

            return otherPrimOp != null && OperatorName.Equals(otherPrimOp.OperatorName);
        }

        public override int GetHashCode()
        {
            return OperatorName.GetHashCode();
        }

        public bool IsNumber()
        {
            return false;
        }

        public bool IsSymbol()
        {
            return false;
        }

        public bool IsList()
        {
            return false;
        }

        public bool IsNull()
        {
            return false;
        }

        public bool IsPrimOp()
        {
            return true;
        }

        public bool IsClosure()
        {
            return false;
        }

        public bool IsString()
        {
            return false;
        }

        public override ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            return this;
        }

        protected override bool TryGetExpectedNumArgs(IGlobalInfo<ISExpression> globalInfo, out int result)
        {

            switch (OperatorName.Value)
            {
                case "primop?":
                case "closure?":
                    result = 1;
                    return true;

                default:
                    return base.TryGetExpectedNumArgs(globalInfo, out result);
            }
        }

        protected override ISExpression EvaluateAux(List<ISExpression> evaluatedArguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {

            switch (OperatorName.Value)
            {
                case "primop?":
                    return evaluatedArguments[0].IsPrimOp() ? globalInfo.TrueValue : globalInfo.FalseValue;

                case "closure?":
                    return evaluatedArguments[0].IsClosure() ? globalInfo.TrueValue : globalInfo.FalseValue;

                default:
                    return base.EvaluateAux(evaluatedArguments, localEnvironment, globalInfo);
            }
        }

        public virtual ISExpression Call(ExpressionList<ISExpression> arguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            // TODO: This function looks a lot like OperatorUsage<T>.Evaluate(), except for the macro handling.  See if we can unify them.
            // (First, we would need to set the PrimOp's arguments list.)
            var actualNumArgs = arguments.Value.Count;
#if DEAD_CODE
            int expectedNumArgs;

            if (!TryGetExpectedNumArgs(globalInfo, out expectedNumArgs))
            {
                throw new EvaluationException(
                    string.Format("PrimOp : Unknown operator name '{0}'", OperatorName.Value),
                    OperatorName.Line, OperatorName.Column);
            }
            else if (expectedNumArgs >= 0 && actualNumArgs != expectedNumArgs)
            {
                throw new EvaluationException(
                    string.Format("PrimOp : Expected {0} argument(s) for operator '{1}', instead of the actual {2} argument(s)",
                        expectedNumArgs, OperatorName.Value, actualNumArgs),
                    OperatorName.Line, OperatorName.Column);
            }
#else
            if (ExpectedNumArgs >= 0 && actualNumArgs != ExpectedNumArgs)
            {
                throw new EvaluationException(
                    string.Format("PrimOp : Expected {0} argument(s) for operator '{1}', instead of the actual {2} argument(s)",
                        ExpectedNumArgs, OperatorName.Value, actualNumArgs),
                    OperatorName.Line, OperatorName.Column);
            }
#endif

            var evaluatedArguments = arguments.Value.Select(expr => expr.Evaluate(localEnvironment, globalInfo)).ToList();
            var argTypesErrorMessage = CheckArgTypes(evaluatedArguments);

            if (!string.IsNullOrEmpty(argTypesErrorMessage))
            {
                throw new EvaluationException(
                    string.Format("Operator '{0}': {1}", OperatorName.Value, argTypesErrorMessage),
                    OperatorName.Line, OperatorName.Column);
            }

            // It is safe to pass a null localEnvironment to EvaluateAux(), since it only uses localEnvironment to evaluate user-defined LISP functions.
            // TODO: Could we pass in a null globalInfo too?  See the PrimOp constructor.
            return EvaluateAux(evaluatedArguments, null, globalInfo);
        }
    }

    #endregion

    #region LambdaExpression

    public class LambdaExpression : IExpression<ISExpression>
    {
        public readonly VariableList<ISExpression> ArgList;
        public readonly IExpression<ISExpression> Body;
        public readonly int Line;
        public readonly int Column;

        public LambdaExpression(VariableList<ISExpression> argList, IExpression<ISExpression> body, int line, int column)
        {
            ArgList = argList;
            Body = body;
            Line = line;
            Column = column;
        }

        public override string ToString()
        {
            return string.Format("(lambda {0} {1})", ArgList, Body);
        }

        public virtual ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            return new Closure(ArgList, Body, localEnvironment, Line, Column);
        }
    }

    #endregion

    #region Closure

    public class Closure : SExpressionBareBase, ICallableSExpression
    {
        public readonly VariableList<ISExpression> ArgList;
        public readonly IExpression<ISExpression> Body;
        public readonly EnvironmentFrame<ISExpression> ClosureEnvironment;

        public int Line { get; private set; }
        public int Column { get; private set; }

        public Closure(VariableList<ISExpression> argList, IExpression<ISExpression> body, EnvironmentFrame<ISExpression> closureEnvironment,
            int line, int column)
        {
            ArgList = argList;
            Body = body;
            ClosureEnvironment = closureEnvironment;
            Line = line;
            Column = column;
        }

        public int ExpectedNumArgs
        {
            get
            {
                return ArgList.Value.Count;
            }
        }

        public override string ToString()
        {
            return "<closure>";
        }

        public override bool Equals(object obj)
        {
            return object.ReferenceEquals(this, obj);
        }

        public override int GetHashCode()   // We must override this, since we overrode Equals().
        {
            return 0;
        }

        public override bool IsClosure()
        {
            return true;
        }

        protected virtual List<ISExpression> CallHelper_EvaluateArguments(ExpressionList<ISExpression> arguments,
            EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            return arguments.Value.Select(expr => expr.Evaluate(localEnvironment, globalInfo)).ToList();
        }

        protected virtual ISExpression CallHelper_PrepareReturnValue(ISExpression result, IGlobalInfo<ISExpression> globalInfo)
        {
            return result;
        }

        public virtual ISExpression Call(ExpressionList<ISExpression> arguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var actualNumArgs = arguments.Value.Count;
#if DEAD_CODE
            var expectedNumArgs = ArgList.Value.Count;

            if (actualNumArgs != expectedNumArgs)
            {
                throw new EvaluationException(
                    string.Format("Closure.Call : Expected {0} argument(s), instead of the actual {1} argument(s); body = {2}",
                        expectedNumArgs, actualNumArgs, Body),
                    Line, Column);
            }
#else

            if (actualNumArgs != ExpectedNumArgs)
            {
                throw new EvaluationException(
                    string.Format("Closure.Call : Expected {0} argument(s), instead of the actual {1} argument(s); body = {2}",
                        ExpectedNumArgs, actualNumArgs, Body),
                    Line, Column);
            }
#endif

            //var evaluatedArguments = arguments.Value.Select(expr => expr.Evaluate(localEnvironment, globalInfo)).ToList();
            var evaluatedArguments = CallHelper_EvaluateArguments(arguments, localEnvironment, globalInfo);
            var newEnvironment = new EnvironmentFrame<ISExpression>(ClosureEnvironment);

            if (globalInfo.Debug)
            {
                LISPGlobalInfo.CreateStackTraceInNewEnvironmentFrame(localEnvironment, newEnvironment, Line, Column);
            }

            newEnvironment.Compose(ArgList.Value, evaluatedArguments);
            //return Body.Evaluate(newEnvironment, globalInfo);

            var result = Body.Evaluate(newEnvironment, globalInfo);

            return CallHelper_PrepareReturnValue(result, globalInfo);
        }
    }

    #endregion

    #region EvaluableExpression

    public class EvaluableExpression : IExpression<ISExpression>
    {
        public readonly IExpression<ISExpression> FirstExpression;
        public readonly ExpressionList<ISExpression> ExpressionList;

        public EvaluableExpression(IExpression<ISExpression> firstExpression, ExpressionList<ISExpression> expressionList)
        {
            FirstExpression = firstExpression;
            ExpressionList = expressionList;
        }

        public override string ToString()
        {

            if (ExpressionList.Value.Count == 0)
            {
                return string.Format("({0})", FirstExpression);
            }

            return string.Format("({0} {1})", FirstExpression, ExpressionList);
        }

        protected virtual ISExpression DeThunkSExpression(ISExpression sexpression, IGlobalInfo<ISExpression> globalInfo)
        {
            return sexpression;
        }

        public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var firstExprAsVariable = FirstExpression as Variable<ISExpression>;

            if (firstExprAsVariable == null || localEnvironment.IsDefined(firstExprAsVariable))
            {
                var firstExprValue = FirstExpression.Evaluate(localEnvironment, globalInfo);

                firstExprValue = DeThunkSExpression(firstExprValue, globalInfo);

                var callableSExpr = firstExprValue as ICallableSExpression;

                if (callableSExpr == null)
                {
                    throw new Exception(string.Format("EvaluableExpression.Evaluate : FirstExpression is not a callable S-Expression; it is a {0}: {1}",
                        firstExprValue.GetType().FullName, firstExprValue));
                }

                return callableSExpr.Call(ExpressionList, localEnvironment, globalInfo);
            }
            else
            {
                // Is FirstExpression the name of a macro?
                var name = new Name(firstExprAsVariable.Name, firstExprAsVariable.Line, firstExprAsVariable.Column);

                if (!globalInfo.MacroDefinitions.ContainsKey(name))
                {
                    throw new EvaluationException(
                        string.Format("Could not find '{0}' in the MacroDefinitions", name.Value),
                        name.Line, name.Column);
                }

                var macro = globalInfo.MacroDefinitions[name];

                if (ExpressionList.Value.Count != macro.ArgumentCount)
                {
                    throw new EvaluationException(
                        string.Format("The macro '{0}' expects {1} argument(s); {2} were passed in",
                            name.Value, macro.ArgumentCount, ExpressionList.Value.Count),
                        name.Line, name.Column);
                }

                return macro.InvokeMacro(ExpressionList.Value, localEnvironment, globalInfo);
            }
        }
    }

    #endregion

    #region LetRecUsage

    public class LetRecUsage : IExpression<ISExpression>
    {
        public readonly List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>> Bindings;
        public readonly IExpression<ISExpression> Expression;

        public LetRecUsage(List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>> bindings, IExpression<ISExpression> expression)
        {
            Bindings = bindings;
            Expression = expression;
        }

        public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var falseValue = globalInfo.FalseValue;
            var newEnvFrame = new EnvironmentFrame<ISExpression>(localEnvironment);

            foreach (var binding in Bindings)   // Add all variables that are bound in Bindings to newEnvFrame before any closures are created in the next loop.
            {
                newEnvFrame.Add(binding.Key, falseValue);
            }

            foreach (var binding in Bindings)
            {
                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, globalInfo));
            }

            return Expression.Evaluate(newEnvFrame, globalInfo);
        }
    }

    #endregion

    #region ContinuationException

    public class ContinuationException : Exception
    {
        public readonly Guid CCGuid;
        public readonly ISExpression ReturnValue;

        public ContinuationException(Guid ccGuid, ISExpression returnValue)
            : base()
        {
            CCGuid = ccGuid;
            ReturnValue = returnValue;
        }
    }

    #endregion

    #region Continuation

    public class Continuation : SExpressionBase, ICallableSExpression
    {
        public readonly Guid CCGuid;

        public int Line { get; private set; }
        public int Column { get; private set; }

        public Continuation(Guid ccGuid, int line, int column)
        {
            CCGuid = ccGuid;
            Line = line;
            Column = column;
        }

        public int ExpectedNumArgs
        {
            get
            {
                return 1;
            }
        }

        public override string ToString()
        {
            return "<closure>";
        }

        public override bool IsClosure()
        {
            return true;
        }

        public ISExpression Call(ExpressionList<ISExpression> arguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var actualNumArgs = arguments.Value.Count;

            if (actualNumArgs != 1)
            {
                throw new EvaluationException(
                    string.Format("Continuation.Call : Expected 1 argument, instead of the actual {1} arguments", actualNumArgs),
                    Line, Column);
            }

            throw new ContinuationException(CCGuid, arguments.Value[0].Evaluate(localEnvironment, globalInfo));
        }
    }

    #endregion

    #region CallCCUsage

    public class CallCCUsage : IExpression<ISExpression>
    {
        public readonly IExpression<ISExpression> Body;

        public CallCCUsage(IExpression<ISExpression> body)
        {
            Body = body;
        }

        public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var closure = Body.Evaluate(localEnvironment, globalInfo) as Closure;

            if (closure == null)
            {
                throw new Exception("CallCCUsage.Evaluate : Body does not evaluate to a Closure.");
            }
            else if (closure.ArgList.Value.Count != 1)
            {
                throw new EvaluationException(
                    string.Format("CallCCUsage.Evaluate : The Closure takes {0} arguments instead of the expected 1", closure.ArgList.Value.Count),
                    closure.Line, closure.Column);
            }

            var ccGuid = Guid.NewGuid();
            var exprList = new ExpressionList<ISExpression>();

            exprList.Value.Add(new Continuation(ccGuid, closure.Line, closure.Column));

            try
            {
                return closure.Call(exprList, localEnvironment, globalInfo);
            }
            catch (ContinuationException ex)
            {

                if (!ex.CCGuid.Equals(ccGuid))
                {
                    throw;
                }

                return ex.ReturnValue;
            }
        }
    }

    #endregion

    #region SchemeGlobalInfo

    public class SchemeGlobalInfo : LISPGlobalInfo
    {
        public SchemeGlobalInfo(ITokenizer tokenizer, IParser parser)
            : base(tokenizer, parser)
        {
        }

        protected void LoadSASLSafePresets()   // These presets do not use side effects: set, begin, while, etc.
        {
            GlobalEnvironment.Add(varStackTrace, new NullSExpression());
            GlobalEnvironment.Add(new Variable<ISExpression>("e", 0, 0), new FloatLiteral(Math.E));
            GlobalEnvironment.Add(new Variable<ISExpression>("pi", 0, 0), new FloatLiteral(Math.PI));

            // Define commonly-used lambda expressions here.
            // Of particular importance are combine, compose, and curry.

            //Evaluate("(set combine (lambda (f sum zero) (lambda (l) (if (null? l) zero (sum (f (car l)) ((combine f sum zero) (cdr l)))))))"); // Version 1: see page 102
            Evaluate("(set combine (lambda (f sum zero) (letrec ((loop (lambda (l) (if (null? l) zero (sum (f (car l)) (loop (cdr l))))))) loop)))"); // Version 2, using letrec: see page 126
            Evaluate("(set compose (lambda (f g) (lambda (x) (g (f x)))))");
            Evaluate("(set curry (lambda (f) (lambda (x) (lambda (y) (f x y)))))");

            Evaluate("(set compose2args (lambda (f g) (lambda (x y) (g (f x y)))))");
            Evaluate("(set reverse2args (lambda (f) (lambda (x y) (f y x))))");

            Evaluate("(set > (reverse2args <))");
            Evaluate("(set not (lambda (x) (if x '() 'T)))");
            Evaluate("(set and (lambda (x y) (if x y x)))");
            Evaluate("(set or (lambda (x y) (if x x y)))");
            Evaluate("(set mod (lambda (m n) (- m (* n (/ m n)))))");
            Evaluate("(set gcd (lambda (m n) (if (= n 0) m (gcd n (mod m n)))))");
            //Evaluate("(set atom? (lambda (x) (or (null? x) (or (number? x) (or (symbol? x) (string? x))))))"); // What about primop? and closure? ?
            Evaluate("(set atom? (compose list? not))"); // Version 2
            //Evaluate("(set equal (lambda (l1 l2) (if (atom? l1) (= l1 l2) (if (atom? l2) '() (if (equal (car l1) (car l2)) (equal (cdr l1) (cdr l2)) '())))))"); // Version 1
            Evaluate(@"
(set equal (lambda (l1 l2)
    (cond
        ((atom? l1) (= l1 l2))
        ((atom? l2) '())
        ((equal (car l1) (car l2)) (equal (cdr l1) (cdr l2)))
        ('T '())
)))"); // Version 2
            //Evaluate("(set >= (lambda (x y) (not (< x y))))");
            Evaluate("(set >= (compose2args < not))");
            //Evaluate("(set <= (lambda (x y) (not (> x y))))");
            Evaluate("(set <= (compose2args > not))");
            //Evaluate("(set <> (lambda (x y) (not (= x y))))");
            Evaluate("(set <> (compose2args = not))");
            Evaluate("(set any (lambda (l) (if (null? l) '() (if (car l) 'T (any (cdr l))))))");
            Evaluate("(set all (lambda (l) (if (null? l) 'T (if (not (car l)) '() (all (cdr l))))))");
            //Evaluate("(set mapcar (lambda (f l) (if (null? l) '() (cons (f (car l)) (mapcar f (cdr l))))))"); // Original definition.
            Evaluate("(set id (lambda (x) x))");
            Evaluate("(set mapc (lambda (f) (combine f cons '())))");   // Second definition.
            Evaluate("(set mapcar (lambda (f l) ((mapc f) l)))");       // Second definition.
            //Evaluate("(set mapc (curry mapcar))");  // Original definition.  From page 101.
            Evaluate("(set any2 (combine id or '()))");
            Evaluate("(set all2 (combine id and 'T))");
            //Evaluate("(set +1 (lambda (n) (+ n 1)))"); // Version 1
            Evaluate("(set +1 ((curry +) 1))"); // Version 2
            //Evaluate("(set append (lambda (l1 l2) (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2)))))"); // Version 1
            Evaluate("(set append (lambda (l1 l2) ((combine id cons l2) l1)))"); // Version 2
            Evaluate("(set reverse (lambda (l) (letrec ((rev-aux (lambda (l1 l2) (if (null? l1) l2 (rev-aux (cdr l1) (cons (car l1) l2)))))) (rev-aux l '()))))");
            Evaluate("(set skip (lambda (n l) (if (or (null? l) (= n 0)) l (skip (- n 1) (cdr l)))))");
            Evaluate("(set take (lambda (n l) (if (or (null? l) (= n 0)) '() (cons (car l) (take (- n 1) (cdr l))))))");
            Evaluate("(set abs (lambda (n) (if (< n 0) (- 0 n) n)))");
            //Evaluate("(set cadr (lambda (l) (car (cdr l))))"); // Version 1
            Evaluate("(set cadr (compose cdr car))"); // Version 2
            Evaluate("(set length (lambda (l) (if (null? l) 0 (+1 (length (cdr l))))))");    // Adapted from page 29.
            /*
            Evaluate(@"
(set find (lambda (pred lis) ; From page 104
    (if (null? lis) '()
        (if (pred (car lis)) 'T (find pred (cdr lis))))))"); // Version 1
             */
            Evaluate(@"
(set find (lambda (pred lis)
    (cond
        ((null? lis) '())
        ((pred (car lis)) 'T)
        ('T (find pred (cdr lis)))
    )
))"); // Version 2
            Evaluate("(set nth (lambda (n l) (if (= n 0) (car l) (nth (- n 1) (cdr l)))))");  // Adapted from page 43.
            /* TODO:
            Evaluate("");
             */
        }

        public override string LoadPreset(string presetName)
        {

            switch (presetName.ToLower())
            {
                case "assoc":
                    // Association list functions (adapted from page 32)
                    Evaluate("(set caar (compose car car))");
                    Evaluate("(set cadar (compose car cadr))");
                    Evaluate(@"
(set assoc (lambda (x alist)
    (cond
        ((null? alist) '())
        ((= x (caar alist)) (cadar alist))
        ('T (assoc x (cdr alist)))
    )
))");
                    Evaluate(@"
(set mkassoc (lambda (x y alist)
    (cond
        ((null? alist) (list (list x y)))
        ((= x (caar alist)) (cons (list x y) (cdr alist)))
        ('T (cons (car alist) (mkassoc x y (cdr alist))))
    )
))");
                    // Additional function
                    /*
                    Evaluate(@"
(set assoc-contains-key (lambda (x alist)
    (if (null? alist) '()
        (if (= x (caar alist)) 'T
            (assoc-contains-key x (cdr alist))))))");
                     */
                    Evaluate("(set assoc-contains-key (lambda (x alist) (find (compose car ((curry =) x)) alist)))");
                    // Adapted from page 55
                    Evaluate(@"
(set rplac-assoc (lambda (x y alist)
    (cond
        ((null? alist) '())
        ((= x (caar alist)) (rplacd (car alist) (list y)))
        ((null? (cdr alist)) (rplacd alist (list (list x y))))
        ('T (rplac-assoc x y (cdr alist)))
    )
))");
                    break;

                case "queue":
                    // Queue functions (adapted from page 37)
                    Evaluate("(set empty-queue '())");
                    Evaluate("(set front car)");
                    Evaluate("(set rm-front cdr)");
                    //Evaluate("(set enqueue (lambda (t q) (if (null? q) (list t) (cons (car q) (enqueue t (cdr q))))))"); // Version 1
                    Evaluate("(set enqueue (lambda (t q) (append q (list t))))"); // Version 2; 2013/11/30
                    Evaluate("(set empty? null?)");
                    break;

                case "compose": // From page 104
                    //Evaluate("(set compose (lambda (f g) (lambda (x) (g (f x)))))");
                    break;

                case "set":
                    // Scheme set functions; from pages 104-105
                    Evaluate("(set nullset '())");
                    Evaluate("(set member? (lambda (x s) (find ((curry equal) x) s)))");
                    Evaluate("(set addelt (lambda (x s) (if (member? x s) s (cons x s))))");
                    Evaluate("(set union (lambda (s1 s2) ((combine id addelt s1) s2)))");
                    break;

                case "select":
                    Evaluate(@"
(set select (lambda (indices l)
    (letrec ((select* (lambda (n indices l)
                        (cond
                            ((or (null? indices) (null? l)) '())
                            ((= n (car indices)) (cons (car l) (select* (+1 n) (cdr indices) (cdr l))))
                            ('T (select* (+1 n) indices (cdr l)))))))
        (select* 0 indices l))))");
                    break;

                case "flatten":
            Evaluate(@"
(set flatten (lambda (tree)
    (if (null? tree) '()
        (if (atom? tree) (list tree)
            (append (flatten (car tree)) (flatten (cdr tree)))))))");
                    break;

                case "sublist":
                    Evaluate(@"
(set sublist (lambda (l start len)
    (cond
        ((or (<= len 0) (null? l)) '())
        ((> start 0) (sublist (cdr l) (- start 1) len))
        ('T (cons (car l) (sublist (cdr l) 0 (- len 1)))))))");
                    Evaluate(@"
(set removesublist (lambda (l start len)
    (cond
        ((or (<= len 0) (null? l)) l)
        ((> start 0) (cons (car l) (removesublist (cdr l) (- start 1) len)))
        ('T (removesublist (cdr l) 0 (- len 1))))))");
                    break;

                case "substring":
                    LoadPreset("sublist");
                    Evaluate("(set substring (lambda (str start len) (listtostring (sublist (stringtolist str) start len))))");
                    Evaluate("(set removesubstring (lambda (str start len) (listtostring (removesublist (stringtolist str) start len))))");
                    break;

                case "stack":
                    Evaluate("(set empty-stack '())");
                    Evaluate("(set push cons)");
                    Evaluate("(set peek car)");
                    Evaluate("(set pop cdr)");
                    Evaluate("(set empty-stack? null?)");
                    break;

                case "filter":
                    Evaluate(@"
(set filter (lambda (pred l) ; Returns only the elements of l for which pred is true.
	(cond
		((null? l) '())
		((pred (car l)) (cons (car l) (filter pred (cdr l))))
		('T (filter pred (cdr l)))
	)
))");
                    Evaluate(@"
(set remove (lambda (x l) ; Returns a copy of l that has had all occurrences of x removed.
	(filter (compose ((curry =) x) not) l)
))");
                    break;

                case "sort":
            Evaluate(@"
(set insertion-sort (lambda (lessthan)
    (letrec
        (
            (insert (lambda (x l)
                (cond
                    ((null? l) (list x))
                    ((lessthan x (car l)) (cons x l))
                    ('T (cons (car l) (insert x (cdr l))))
                )
            ))
        )
        (combine id insert '())
    )
))");
            Evaluate(@"
(set quicksort (lambda (lessthan)
    (letrec
        (
            (partition (lambda (pivot-element l lessthanlist notlessthanlist)
                (cond
                    ((null? l) (list lessthanlist notlessthanlist))
                    ((lessthan (car l) pivot-element) (partition pivot-element (cdr l) (cons (car l) lessthanlist) notlessthanlist))
                    ('T (partition pivot-element (cdr l) lessthanlist (cons (car l) notlessthanlist)))
                )
            ))
            (qs (lambda (l)
                (if (< (length l) 2) l
                    (let ((partitioned-lists (partition (car l) (cdr l) '() '())))
                        (append (qs (car partitioned-lists)) (cons (car l) (qs (cadr partitioned-lists))))
                    )
                )
            ))
        )
        qs
    )
))");
            Evaluate(@"
(set merge-sort (lambda (lessthan)
    (letrec
        (
            (merge (lambda (l1 l2 reversed-result)
                (cond
                    ((null? l1) (append (reverse reversed-result) l2))
                    ((null? l2) (append (reverse reversed-result) l1))
                    ((lessthan (car l1) (car l2)) (merge (cdr l1) l2 (cons (car l1) reversed-result)))
                    ('T (merge l1 (cdr l2) (cons (car l2) reversed-result)))
                )
            ))
            (cut-list (lambda (l)
                (let ((len (/ (length l) 2)))
                    (list (take len l) (skip len l))
                )
            ))
            (ms (lambda (l)
                (if (< (length l) 2) l
                    (let ((lists (cut-list l)))
                        (merge (ms (car lists)) (ms (cadr lists)) '())
                    )
                )
            ))
        )
        ms
    )
))");
                    break;

                default:
                    throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
            }

            return string.Format("The preset '{0}' has been successfully loaded.", presetName);
        }

        public override void LoadPresets()
        {
            // Define commonly-used lambda expressions here.
            LoadSASLSafePresets();

            // And now we can load any SASL-unsafe, Scheme-only presets below.
            /* TODO:
            Evaluate("");
             */
        }

        public override Dictionary<Name, FunctionDefinition<ISExpression>> FunctionDefinitions
        {
            get
            {
                throw new Exception("The FunctionDefinitions dictionary should not be used by Scheme or SASL.");
            }
        }

        public override bool SetScoping(bool dynamicScoping)
        {
            return false;
        }
    }

    #endregion
}
