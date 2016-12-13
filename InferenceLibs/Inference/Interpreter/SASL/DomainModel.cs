using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.LISP;
using Inference.Interpreter.Scheme;
using Inference.Parser;

namespace Inference.Interpreter.SASL
{
    #region SASLPrimOp

    public class SASLPrimOp : PrimOp, IConvertibleToGraph
    {
        public SASLPrimOp(Name operatorName)
            : base(operatorName)
        {
        }

        protected override bool TryGetExpectedNumArgs(IGlobalInfo<ISExpression> globalInfo, out int result)
        {

            switch (OperatorName.Value)
            {
                case "cond":
                    result = -1;
                    return true;

                case "if":
                    result = 3;
                    return true;

                default:
                    return base.TryGetExpectedNumArgs(globalInfo, out result);
            }
        }

        private ISExpression ExecuteIf(List<ISExpression> argumentsAsSExpressions, IGlobalInfo<ISExpression> globalInfo)
        {
            ISExpression result;
            ISExpression conditionValue = argumentsAsSExpressions[0];

            SASLGlobalInfo.DeThunk(ref conditionValue, globalInfo);

            if (!conditionValue.IsNull())
            {
                result = argumentsAsSExpressions[1];
            }
            else
            {
                result = argumentsAsSExpressions[2];
            }

            return SASLGlobalInfo.DeThunk(ref result, globalInfo);
        }

        private ISExpression ExecuteCond(List<ISExpression> argumentsAsSExpressions, IGlobalInfo<ISExpression> globalInfo)
        {

            foreach (var argumentAsSExpression in argumentsAsSExpressions)
            {

                if (!(argumentAsSExpression is Thunk))
                {
                    throw new Exception("ExecuteCond : argumentAsSExpression is not a Thunk.");
                }

                var thunk = (Thunk)argumentAsSExpression;

                while (thunk.Body is Thunk)
                {
                    thunk = (Thunk)thunk.Body;
                }

                if (!(thunk.Body is SASLEvaluableExpression))   // The thunk body is not really an evaluable expression, but that's how the parser sees it.
                {
                    throw new Exception("ExecuteCond : thunk.Body is not an SASLEvaluableExpression.");
                }

                var evExp = (SASLEvaluableExpression)thunk.Body;
                var conditionExpression = evExp.FirstExpression;
                var conditionValue = conditionExpression.Evaluate(thunk.ThunkEnvironment, globalInfo);

                if (conditionValue.IsNull())
                {
                    continue;
                }

                if (evExp.ExpressionList.Value.Count != 1)
                {
                    throw new Exception(string.Format("ExecuteCond : evExp.ExpressionList length is {0} rather than 1.", evExp.ExpressionList.Value.Count));
                }

                return evExp.ExpressionList.Value[0].Evaluate(thunk.ThunkEnvironment, globalInfo);
            }

            return new NullSExpression();
        }

        public override ISExpression Call(ExpressionList<ISExpression> arguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var actualNumArgs = arguments.Value.Count;
            int expectedNumArgs;

            if (!TryGetExpectedNumArgs(globalInfo, out expectedNumArgs))
            {
                throw new Exception(string.Format("SASLPrimOp : Unknown operator name '{0}'.", OperatorName.Value));
            }
            else if (expectedNumArgs >= 0 && actualNumArgs != expectedNumArgs)
            {
                throw new Exception(string.Format("SASLPrimOp : Expected {0} arguments for operator '{1}', instead of the actual {2} arguments.",
                    expectedNumArgs, OperatorName.Value, actualNumArgs));
            }

            // ThAW 2012/12/07 : Do not create thunks out of arguments that are already S-expressions : See page 202, exercise 6.
            var argumentsAsSExpressions = arguments.Value.Select(expr => (expr is ISExpression) ? (ISExpression)expr : new Thunk(expr, localEnvironment)).ToList();

            switch (OperatorName.Value)
            {
                case "cons":
                    return new SExpressionList(argumentsAsSExpressions[0], argumentsAsSExpressions[1]);

                case "list":
                    return SExpressionList.MakeFromList(argumentsAsSExpressions);

                case "if":
                    return ExecuteIf(argumentsAsSExpressions, globalInfo);

                case "cond":
                    return ExecuteCond(argumentsAsSExpressions, globalInfo);

                default:
                    break;
            }

            var evaluatedArguments = argumentsAsSExpressions.Select(sexpr => (sexpr is Thunk) ? ((Thunk)sexpr).Evaluate(null, globalInfo) : sexpr).ToList();
            SExpressionList list;

            switch (OperatorName.Value)
            {
                case "car":
                    list = evaluatedArguments[0] as SExpressionList;

                    if (list == null)
                    {
                        throw new Exception(string.Format(
                            "car: first arg is not an SExpressionList; it is an '{0}' with value '{1}'.",
                            evaluatedArguments[0].GetType().FullName, evaluatedArguments[0]));
                    }

                    return SASLGlobalInfo.DeThunk(ref list.Head, globalInfo);

                case "cdr":
                    list = evaluatedArguments[0] as SExpressionList;

                    if (list == null)
                    {
                        throw new Exception(string.Format(
                            "cdr: first arg is not an SExpressionList; it is an '{0}' with value '{1}'.",
                            evaluatedArguments[0].GetType().FullName, evaluatedArguments[0]));
                    }

                    return SASLGlobalInfo.DeThunk(ref list.Tail, globalInfo);

                default:
                    // It is safe to pass a null localEnvironment to EvaluateAux(), since it only uses localEnvironment to evaluate user-defined LISP functions.
                    var result = EvaluateAux(evaluatedArguments, null, globalInfo);

                    if (result is Thunk)
                    {
                        throw new Exception("SASLPrimOp.Call was about to return a Thunk.");
                    }

                    return result;
            }
        }

        public IExpression<ISExpression> ConvertToGraph()
        {
            return this;
        }
    }

    #endregion

    #region SASLLambdaExpression

    public class SASLLambdaExpression : LambdaExpression, IConvertibleToGraph
    {
        public SASLLambdaExpression(VariableList<ISExpression> argList, IExpression<ISExpression> body, int line, int column)
            : base(argList, body, line, column)
        {
        }

        public override ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            return new SASLClosure(ArgList, Body, localEnvironment, Line, Column);
        }

        private IExpression<ISExpression> MakeAbstraction(IExpression<ISExpression> n, Variable<ISExpression> variable)
        {
            var nAsGraphReductionNode = n as GraphReductionNode;

            if (nAsGraphReductionNode != null)
            {
                var n1 = new GraphReductionNode(
                    new SKIOp("S"),
                    MakeAbstraction(nAsGraphReductionNode.LeftChild, variable));

                return new GraphReductionNode(n1, MakeAbstraction(nAsGraphReductionNode.RightChild, variable));
            }

            var nAsVariable = n as Variable<ISExpression>;

            if (variable.Equals(nAsVariable))
            {
                return new SKIOp("I");
            }

            return new GraphReductionNode(new SKIOp("K"), n);
        }

        public IExpression<ISExpression> ConvertToGraph()
        {

            if (ArgList.Value.Count != 1)
            {
                throw new Exception(string.Format("SASLLambdaExpression.ConvertToGraph() : There are {0} arguments; expected 1.",
                    ArgList.Value.Count));
            }

            var convertibleBody = Body as IConvertibleToGraph;

            if (convertibleBody == null)
            {
                throw new Exception("SASLLambdaExpression.ConvertToGraph() : Body is not IConvertibleToGraph.");
            }

            return MakeAbstraction(convertibleBody.ConvertToGraph(), ArgList.Value[0]);
        }
    }

    #endregion

    #region SASLClosure

    public class SASLClosure : Closure
    {
        public SASLClosure(VariableList<ISExpression> argList, IExpression<ISExpression> body, EnvironmentFrame<ISExpression> closureEnvironment,
            int line, int column)
            : base(argList, body, closureEnvironment, line, column)
        {
        }

#if !DEAD_CODE
        protected override List<ISExpression> CallHelper_EvaluateArguments(ExpressionList<ISExpression> arguments,
            EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            return arguments.Value.Select(expr => (ISExpression)((expr is Thunk) ? expr : new Thunk(expr, localEnvironment))).ToList();
        }

        protected override ISExpression CallHelper_PrepareReturnValue(ISExpression result, IGlobalInfo<ISExpression> globalInfo)
        {
            return SASLGlobalInfo.DeThunk(ref result, globalInfo);
        }
#else
        public override ISExpression Call(ExpressionList<ISExpression> arguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var actualNumArgs = arguments.Value.Count;
            var expectedNumArgs = ArgList.Value.Count;

            if (actualNumArgs != expectedNumArgs)
            {
                throw new EvaluationException(
                    string.Format("SASLClosure.Call : Expected {0} argument(s), instead of the actual {1} argument(s); body = {2}",
                        expectedNumArgs, actualNumArgs, Body),
                    Line, Column);
            }

            var evaluatedArguments = arguments.Value.Select(expr => (ISExpression)((expr is Thunk) ? expr : new Thunk(expr, localEnvironment))).ToList();
            var newEnvironment = new EnvironmentFrame<ISExpression>(ClosureEnvironment);

            newEnvironment.Compose(ArgList.Value, evaluatedArguments);
            ISExpression result = Body.Evaluate(newEnvironment, globalInfo);

            return SASLGlobalInfo.DeThunk(ref result, globalInfo);
        }
#endif
    }

    #endregion

    #region SASLEvaluableExpression

    public class SASLEvaluableExpression : EvaluableExpression, IConvertibleToGraph
    {
        public SASLEvaluableExpression(IExpression<ISExpression> firstExpression, ExpressionList<ISExpression> expressionList)
            : base(firstExpression, expressionList)
        {
        }

        protected override ISExpression DeThunkSExpression(ISExpression sexpression, IGlobalInfo<ISExpression> globalInfo)
        {
            return SASLGlobalInfo.DeThunk(ref sexpression, globalInfo);
        }

        public IExpression<ISExpression> ConvertToGraph()
        {

            if (ExpressionList.Value.Count != 1)
            {
                throw new Exception(string.Format("SASLEvaluableExpression.ConvertToGraph() : Length of ExpressionList is {0}; expected 1.",
                    ExpressionList.Value.Count));
            }

            var e1 = FirstExpression as IConvertibleToGraph;
            var e2 = ExpressionList.Value[0] as IConvertibleToGraph;

            if (e1 == null)
            {
                throw new Exception(string.Format("SASLEvaluableExpression.ConvertToGraph() : FirstExpression is not an IConvertibleToGraph."));
            }
            else if (e2 == null)
            {
                throw new Exception(string.Format("SASLEvaluableExpression.ConvertToGraph() : ExpressionList.Value[0] is not an IConvertibleToGraph."));
            }

            return new GraphReductionNode(e1.ConvertToGraph(), e2.ConvertToGraph());
        }
    }

    #endregion

#if DEAD_CODE
    #region SASLLetRecUsage

    // This class might not be necessary.  Can we write a unit test that passes with this class but fails with the Scheme LetRecUsage?

    public class SASLLetRecUsage : IExpression<ISExpression>
    {
        public readonly List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>> Bindings;
        public readonly IExpression<ISExpression> Expression;

        public SASLLetRecUsage(List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>> bindings, IExpression<ISExpression> expression)
        {
            Bindings = bindings;
            Expression = expression;
        }

        // See page 202, exercise 7.

        public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var newEnvFrame = new EnvironmentFrame<ISExpression>(localEnvironment);

            foreach (var binding in Bindings)   // Add all variables that are bound in Bindings to newEnvFrame before creating the circular references.
            {
                newEnvFrame.Add(binding.Key, new Thunk(binding.Value, localEnvironment));
            }

            foreach (var value in newEnvFrame.Dict.Values)
            {
                var thunk = (Thunk)value;

                thunk.ThunkEnvironment = newEnvFrame;
            }

            return Expression.Evaluate(newEnvFrame, globalInfo);
        }
    }

    #endregion
#endif

    #region Thunk

    // A thunk (or suspension).

    public class Thunk : SExpressionBase
    {
        public readonly IExpression<ISExpression> Body;
        public /* readonly */ EnvironmentFrame<ISExpression> ThunkEnvironment;  // Not readonly, due to letrec.

        public Thunk(IExpression<ISExpression> body, EnvironmentFrame<ISExpression> thunkEnvironment)
        {
            Body = body;
            ThunkEnvironment = thunkEnvironment;
        }

        // Do we need to override Equals() and GetHashCode() ?

        public override string ToString()
        {
            return "<thunk>";
        }

        public override bool Equals(object obj)
        {
            return object.ReferenceEquals(this, obj);
        }

        public override int GetHashCode()   // We must override this, since we overrode Equals().
        {
            return 0;
        }

        public override ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            var result = Body.Evaluate(ThunkEnvironment, globalInfo);

            return SASLGlobalInfo.DeThunk(ref result, globalInfo);
        }
    }

    #endregion

    #region SASLGlobalInfo

    public class SASLGlobalInfo : SchemeGlobalInfo
    {
        public SASLGlobalInfo(ITokenizer tokenizer, IParser parser)
            : base(tokenizer, parser)
        {
        }

        public override void LoadPresets()
        {
            LoadSASLSafePresets();

            Evaluate("(set force (lambda (x) (if (list? x) (if (force (car x)) (force (cdr x)) '()) 'T)))");    // Un-thunk x.
            Evaluate("(set ints-from (lambda (i) (cons i (ints-from (+1 i)))))");
            Evaluate("(set ints (ints-from 0))");
            Evaluate(@"(set first-n (lambda (n l)
                            (if (or (null? l) (= n 0)) '()
                                (cons (car l) (first-n (- n 1) (cdr l))))))");
            // Define commonly-used lambda expressions here.
            /* TODO:
            Evaluate("");
             */
        }

        // TODO: Make this non-static, if possible.

        public static ISExpression DeThunk(ref ISExpression sexpr, IGlobalInfo<ISExpression> globalInfo)
        {

            while (sexpr is Thunk)
            {
                var thunk = sexpr as Thunk;

                sexpr = thunk.Evaluate(null, globalInfo);
            }

            return sexpr;
        }
    }

    #endregion
}
