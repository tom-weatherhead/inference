using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Parser;
using Inference.Utilities;      // For extension methods such as m.To(n)

namespace Inference.Interpreter.APL
{
    #region Interfaces

    public interface IAPLValue
    {
        bool IsNull { get; }
        bool IsScalar { get; }
        bool IsVector { get; }
        bool IsMatrix { get; }
        bool IsIntScalar { get; }
        bool IsIntVector { get; }
        bool IsFirstScalarEqualToZero { get; }
        int NumberOfDimensions { get; }
        bool AreShapesEqual(IAPLValue otherValue);
        APLValue<int> GetShape();
        APLValue<int> ConvertToIntEquivalent();
    }

    #endregion

    #region APLValue<T>

    public class APLValue<T> : IAPLValue, IExpression<IAPLValue>
    {
        public readonly List<T> Scalars = new List<T>();
        public readonly List<int> Shape = new List<int>();
        public readonly int NumberOfContainedScalars;
        public readonly List<int> Steps = new List<int>();

        private APLValue(List<int> srcShape)
        {

            if (srcShape.Any(s => s < 0))
            {
                throw new Exception("APLValue constructor: Shape vector contains one or more negative elements");
            }

            Shape.AddRange(srcShape);
            NumberOfContainedScalars = 1;

            foreach (var s in Shape)
            {
                NumberOfContainedScalars *= s;
            }

            if (Shape.Count > 0)
            {
                Steps.Add(1);
            }

            for (var i = Shape.Count - 1; i > 0; --i)
            {
                Steps.Insert(0, Steps[0] * Shape[i]);
            }

            if (Shape.Count != Steps.Count)
            {
                throw new Exception("APLValue constructor: The Shape vector and the Steps vector have different lengths");
            }
        }

        public APLValue(List<int> srcShape, List<T> srcList)
            : this(srcShape)
        {

            if (NumberOfContainedScalars == 0)
            {
                return;
            }

            if (srcList.Count == 0)
            {
                throw new Exception("APLValue constructor: srcList is empty");
            }

            var srcIndex = 0;

            while (Scalars.Count < NumberOfContainedScalars)
            {
                Scalars.Add(srcList[srcIndex]);

                if (++srcIndex >= srcList.Count)
                {
                    srcIndex = 0;
                }
            }
        }

        public static APLValue<T> CreateScalar(T n)
        {
            return new APLValue<T>(new List<int>(), new List<T>() { n });
        }

        public static APLValue<T> CreateVector(int length, List<T> srcList)
        {
            return new APLValue<T>(new List<int>() { length }, srcList);
        }

        public static APLValue<T> CreateVector(List<T> srcList)
        {
            return APLValue<T>.CreateVector(srcList.Count, srcList);
        }

        public int NumberOfDimensions
        {
            get
            {
                return Shape.Count;
            }
        }

        public bool IsScalar
        {
            get
            {
                return NumberOfDimensions == 0;
            }
        }

        public bool IsVector
        {
            get
            {
                return NumberOfDimensions == 1;
            }
        }

        public bool IsMatrix
        {
            get
            {
                return NumberOfDimensions >= 2;
            }
        }

        public bool IsIntScalar
        {
            get
            {
                return IsScalar && this is APLValue<int>;
            }
        }

        public bool IsIntVector
        {
            get
            {
                return IsVector && this is APLValue<int>;
            }
        }

        public bool AreShapesEqual(IAPLValue otherValue)
        {
            var otherShape = otherValue.GetShape().Scalars;

            if (Shape.Count != otherShape.Count)
            {
                return false;
            }

            for (var i = 0; i < Shape.Count; ++i)
            {

                if (Shape[i] != otherShape[i])
                {
                    return false;
                }
            }

            return true;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherValue = obj as APLValue<T>;

            if (otherValue == null || !AreShapesEqual(otherValue))
            {
                return false;
            }

            for (var i = 0; i < Scalars.Count; ++i)
            {

                if (!Scalars[i].Equals(otherValue.Scalars[i]))
                {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode()
        {
            var result = 0;

            foreach (var s in Scalars)
            {
                result = 101 * result + s.GetHashCode();
            }

            return result;
        }

        private string ValueToString(T value)
        {

            if (this is APLValue<double>)
            {
                return APLGlobalInfo.APLDoubleToString(Convert.ToDouble(value));
            }

            return value.ToString();
        }

        private void ToStringHelper(StringBuilder sb, List<int> offsetVector, int offset, bool firstSlice)
        {
            var dimNum = offsetVector.Count;

            switch (NumberOfDimensions - dimNum)
            {
                case 0:
                    sb.Append(ValueToString(Scalars[offset]));
                    break;

                case 1:
                    sb.Append(string.Join(" ", Scalars.Skip(offset).Take(Shape[dimNum]).Select(n => ValueToString(n))));
                    break;

                case 2:

                    if (dimNum > 0)
                    {

                        if (!firstSlice)
                        {
                            sb.AppendLine();    // This one provides a CR/LF after the previous slice's last row.
                            sb.AppendLine();    // This one provides a blank line between the slices.
                        }

                        sb.AppendLine(string.Format("Slice ({0}) :", string.Join(", ", offsetVector)));
                    }

                    for (var row = 0; row < Shape[dimNum]; ++row)
                    {

                        if (row > 0)
                        {
                            sb.AppendLine();
                        }

                        sb.Append(string.Join(" ", Scalars.Skip(offset).Take(Shape[dimNum + 1]).Select(n => ValueToString(n))));
                        offset += Steps[dimNum];
                    }

                    break;

                default:
                    var newOffsetVector = new List<int>(offsetVector);

                    newOffsetVector.Add(0);

                    for (var slice = 0; slice < Shape[dimNum]; ++slice)
                    {
                        newOffsetVector[newOffsetVector.Count - 1] = slice;
                        ToStringHelper(sb, newOffsetVector, offset, firstSlice);
                        offset += Steps[dimNum];
                        firstSlice = false;
                    }

                    break;
            }
        }

        public override string ToString()
        {
            var sb = new StringBuilder();

            ToStringHelper(sb, new List<int>(), 0, true);
            return sb.ToString();
        }

        public bool IsNull
        {
            get
            {
                return NumberOfContainedScalars == 0;
            }
        }

        public APLValue<int> GetShape()
        {
            return APLValue<int>.CreateVector(Shape);
        }

        public T GetFirstScalar()
        {

            if (IsNull)
            {
                throw new Exception("APLValue<T>.GetFirstScalar: Value is APL null");
            }

            return Scalars[0];
        }

        public bool IsFirstScalarEqualToZero
        {
            get
            {
                return GetFirstScalar().Equals(default(T));
            }
        }

        public APLValue<int> ConvertToIntEquivalent()
        {

            if (this is APLValue<int>)
            {
                return this as APLValue<int>;
            }

            return new APLValue<int>(Shape, Scalars.Select(n => Convert.ToInt32(n)).ToList());
        }

        public APLValue<T> ToScalarIfPossible()
        {

            if (NumberOfContainedScalars != 1)
            {
                return this;
            }

            return APLValue<T>.CreateScalar(GetFirstScalar());
        }

        public APLValue<T> ToVector()        // I.e. ravel
        {
            return APLValue<T>.CreateVector(NumberOfContainedScalars, Scalars);
        }

        public APLValue<T> CreateSlice(int n)
        {

            if (NumberOfDimensions == 0)
            {
                throw new Exception("APLValue<T>.CreateSlice() : Cannot create a slice of a scalar");
            }
            else if (n <= 0 || n > Shape[0])
            {
                throw new Exception(string.Format("APLValue<T>.CreateSlice() : Index out of bounds: {0} is not in the range from 1 to {1}",
                    n, Shape[0]));
            }

            var step = Steps[0];

            return new APLValue<T>(Shape.Skip(1).ToList(), Scalars.Skip((n - 1) * step).Take(step).ToList());
        }

        public IAPLValue Evaluate(EnvironmentFrame<IAPLValue> localEnvironment, IGlobalInfo<IAPLValue> globalInfo)
        {
            return this;
        }
    }

    #endregion

    #region APLIfUsage

    public class APLIfUsage : IExpression<IAPLValue>
    {
        public readonly IExpression<IAPLValue> Condition;
        public readonly IExpression<IAPLValue> IfBody;
        public readonly IExpression<IAPLValue> ElseBody;

        public APLIfUsage(IExpression<IAPLValue> condition, IExpression<IAPLValue> ifBody, IExpression<IAPLValue> elseBody)
        {
            Condition = condition;
            IfBody = ifBody;
            ElseBody = elseBody;
        }

        public override string ToString()
        {
            return string.Format("(if {0} {1} {2})", Condition, IfBody, ElseBody);
        }

        public IAPLValue Evaluate(EnvironmentFrame<IAPLValue> localEnvironment, IGlobalInfo<IAPLValue> globalInfo)
        {
            bool conditionValue = !Condition.Evaluate(localEnvironment, globalInfo).IsFirstScalarEqualToZero;

            if (conditionValue)
            {
                return IfBody.Evaluate(localEnvironment, globalInfo);
            }
            else
            {
                return ElseBody.Evaluate(localEnvironment, globalInfo);
            }
        }
    }

    #endregion

    #region APLWhileUsage

    public class APLWhileUsage : IExpression<IAPLValue>
    {
        public readonly IExpression<IAPLValue> Condition;
        public readonly IExpression<IAPLValue> Body;

        public APLWhileUsage(IExpression<IAPLValue> condition, IExpression<IAPLValue> body)
        {
            Condition = condition;
            Body = body;
        }

        public override string ToString()
        {
            return string.Format("(while {0} {1})", Condition, Body);
        }

        public IAPLValue Evaluate(EnvironmentFrame<IAPLValue> localEnvironment, IGlobalInfo<IAPLValue> globalInfo)
        {

            while (!Condition.Evaluate(localEnvironment, globalInfo).IsFirstScalarEqualToZero)
            {
                Body.Evaluate(localEnvironment, globalInfo);
            }

            return globalInfo.FalseValue;
        }
    }

    #endregion

    #region APLCondUsage

    public class APLCondUsage : IExpression<IAPLValue>
    {
        public readonly List<KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>> ExprPairList;

        public APLCondUsage(List<KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>> exprPairList)
        {
            ExprPairList = exprPairList;
        }

        public IAPLValue Evaluate(EnvironmentFrame<IAPLValue> localEnvironment, IGlobalInfo<IAPLValue> globalInfo)
        {

            foreach (var exprPair in ExprPairList)
            {

                if (!exprPair.Key.Evaluate(localEnvironment, globalInfo).IsFirstScalarEqualToZero)
                {
                    return exprPair.Value.Evaluate(localEnvironment, globalInfo);
                }
            }

            return globalInfo.FalseValue;
        }
    }

    #endregion

    #region APLOperatorUsage

    public class APLOperatorUsage : OperatorUsage<IAPLValue>
    {
        public APLOperatorUsage(Name operatorName, ExpressionList<IAPLValue> expressionList)
            : base(operatorName, expressionList)
        {
        }

        protected override bool TryGetExpectedNumArgs(IGlobalInfo<IAPLValue> globalInfo, out int result)
        {

            switch (OperatorName.Value)
            {
                case "+/":
                case "-/":
                case "*/":
                case "//":
                case "max/":
                case "or/":
                case "and/":
                case "shape":
                case "ravel":
                case "indx":
                case "trans":
                case "exp":
                case "ln":
                case "sin":
                case "cos":
                case "tan":
                    result = 1;
                    return true;

                case "max":
                case "or":
                case "and":
                case "compress":
                case "restruct":
                case "cat":
                case "[]":
                case "random":
                case "pow":
                    result = 2;
                    return true;

                case "[;]":
                    result = 3;
                    return true;

                default:
                    return base.TryGetExpectedNumArgs(globalInfo, out result);
            }
        }

        protected override string CheckArgTypes(List<IAPLValue> evaluatedArguments)
        {

            switch (OperatorName.Value)
            {
                case "+/":
                case "-/":
                case "*/":
                case "//":
                case "max/":
                case "or/":
                case "and/":
                case "indx":

                    if (evaluatedArguments[0].IsNull)
                    {
                        return "Argument is null";
                    }

                    break;

                case "compress":

                    /* if (evaluatedArguments[0].IsNull)
                    {
                        return "The first argument is null";
                    }
                    else */ if (!evaluatedArguments[0].IsVector)
                    {
                        return "The first argument is not a vector";
                    }
                    /* else if (evaluatedArguments[1].IsNull)
                    {
                        return "The second argument is null";
                    } */
                    else if (evaluatedArguments[1].IsScalar)
                    {
                        return "The second argument is a scalar";
                    }
                    else if (evaluatedArguments[0].GetShape().Scalars[0] != evaluatedArguments[1].GetShape().Scalars[0])
                    {
                        return "The length of the first argument is not equal to the number of slices in the second argument";
                    }

                    break;

                case "[]":

                    if (evaluatedArguments[0].IsNull)
                    {
                        return "The first argument is null";
                    }
                    else if (evaluatedArguments[0].IsScalar)
                    {
                        return "The first argument is a scalar";
                    }
                    // ThAW 2012/12/10 : It is legal for the second argument to be null; the result will be a null vector or matrix.
                    // See SparseVectorTest2().
                    else if (evaluatedArguments[1].IsMatrix)
                    {
                        return "The second argument is a matrix";
                    }

                    break;

                case "random":

                    if (!evaluatedArguments[0].IsIntScalar)
                    {
                        return "The first argument is not an int scalar";
                    }
                    else if (!evaluatedArguments[1].IsIntScalar)
                    {
                        return "The second argument is not an int scalar";
                    }

                    break;

                default:
                    return base.CheckArgTypes(evaluatedArguments);
            }

            return null;
        }

        private Func<double, double> GetExpLnEtcOperator(string operatorName)
        {
            if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(operatorName))
            {
                return DoubleOperatorKeeper.OneArgumentOperators[operatorName];
            }

            throw new Exception(string.Format("GetExpLnOperator() : Unknown operator '{0}'.", operatorName));
        }

        private APLValue<double> EvaluateExpLnEtcExpressionHelper<T>(APLValue<T> arg,
            Func<T, double> conversionLambda, Func<double, double> operatorLambda)
        {
            var argAsValue = arg.ToScalarIfPossible() as APLValue<T>;

            return new APLValue<double>(argAsValue.Shape, argAsValue.Scalars.Select(n => operatorLambda(conversionLambda(n))).ToList());
        }

        private APLValue<double> EvaluateExpLnEtcExpression(IAPLValue arg, string operatorName)
        {
            var argAsIntType = arg as APLValue<int>;
            var argAsDoubleType = arg as APLValue<double>;
            Func<int, double> lambdaConvertIntToDouble = x => Convert.ToDouble(x);
            Func<double, double> lambdaConvertDoubleToDouble = x => x;
            Func<double, double> lambdaOperator = GetExpLnEtcOperator(operatorName);
            APLValue<double> result;

            if (argAsIntType != null)
            {
                result = EvaluateExpLnEtcExpressionHelper(argAsIntType, lambdaConvertIntToDouble, lambdaOperator);
            }
            else
            {
                result = EvaluateExpLnEtcExpressionHelper(argAsDoubleType, lambdaConvertDoubleToDouble, lambdaOperator);
            }

            return result;
        }

        private Func<int, int, int> GetDyadicIntIntOperator(string operatorName)
        {

            switch (operatorName)
            {
                case "+": return (x, y) => x + y;
                case "-": return (x, y) => x - y;
                case "*": return (x, y) => x * y;
                case "/": return (x, y) => x / y;
                case "max": return (x, y) => Math.Max(x, y);
                case "or": return (x, y) => x != 0 || y != 0 ? 1 : 0;
                case "and": return (x, y) => x != 0 && y != 0 ? 1 : 0;
                case "=": return (x, y) => x == y ? 1 : 0;
                case "<": return (x, y) => x < y ? 1 : 0;
                //case ">": return (x, y) => x > y ? 1 : 0;
                case "pow": return (x, y) => (int)Math.Pow(x, y);
                default: throw new Exception(string.Format("GetDyadicIntIntOperator() : Unknown operator '{0}'.", operatorName));
            }
        }

        private Func<double, double, double> GetDyadicDoubleDoubleOperator(string operatorName)
        {

            switch (operatorName)
            {
                case "+": return (x, y) => x + y;
                case "-": return (x, y) => x - y;
                case "*": return (x, y) => x * y;
                case "/": return (x, y) => x / y;
                case "max": return (x, y) => Math.Max(x, y);
                case "or": return (x, y) => x != 0.0 || y != 0.0 ? 1.0 : 0.0;
                case "and": return (x, y) => x != 0.0 && y != 0.0 ? 1.0 : 0.0;
                case "=": return (x, y) => x == y ? 1.0 : 0.0;
                case "<": return (x, y) => x < y ? 1.0 : 0.0;
                //case ">": return (x, y) => x > y ? 1.0 : 0.0;
                case "pow": return (x, y) => Math.Pow(x, y);
                default: throw new Exception(string.Format("GetDyadicDoubleDoubleOperator() : Unknown operator '{0}'.", operatorName));
            }
        }

        private bool DyadicOperatorMustReturnInt(string operatorName)
        {
            switch (operatorName)
            {
                case "or":
                case "and":
                case "=":
                case "<":
                //case ">":
                    return true;

                default:
                    return false;
            }
        }

        private string GetDyadicOperatorNameFromReductionName(string reductionName)
        {

            switch (reductionName)
            {
                case "+/":
                case "-/":
                case "*/":
                case "//":
                case "max/":
                case "or/":
                case "and/":
                    return reductionName.Substring(0, reductionName.Length - 1);

                default:
                    throw new Exception(string.Format("GetDyadicOperatorNameFromReductionName() : Unknown reduction operator '{0}'.", reductionName));
            }
        }

        private IAPLValue EvaluateDyadicExpressionHelper<T1, T2, TOut>(APLValue<T1> arg1, APLValue<T2> arg2,
            Func<T1, TOut> conversionLambda1, Func<T2, TOut> conversionLambda2, Func<TOut, TOut, TOut> operatorLambda)
        {
            var arg1AsValue = arg1.ToScalarIfPossible() as APLValue<T1>;
            var arg2AsValue = arg2.ToScalarIfPossible() as APLValue<T2>;

            if (arg1AsValue.IsScalar)
            {
                var convertedValue1 = conversionLambda1(arg1AsValue.GetFirstScalar());

                return new APLValue<TOut>(arg2AsValue.GetShape().Scalars,
                    arg2AsValue.Scalars.Select(n2 => operatorLambda(convertedValue1, conversionLambda2(n2))).ToList());
            }
            else if (arg2AsValue.IsScalar)
            {
                var convertedValue2 = conversionLambda2(arg2AsValue.GetFirstScalar());

                return new APLValue<TOut>(arg1AsValue.GetShape().Scalars,
                    arg1AsValue.Scalars.Select(n1 => operatorLambda(conversionLambda1(n1), convertedValue2)).ToList());
            }
            else if (arg1AsValue.AreShapesEqual(arg2AsValue))
            {
                var newScalars = new List<TOut>();

                for (var i = 0; i < arg1AsValue.Scalars.Count; ++i)
                {
                    newScalars.Add(operatorLambda(conversionLambda1(arg1AsValue.Scalars[i]), conversionLambda2(arg2AsValue.Scalars[i])));
                }

                return new APLValue<TOut>(arg1AsValue.GetShape().Scalars, newScalars);
            }
            else
            {
                throw new Exception("Cannot perform a dyadic operation; neither value is a scalar, and the shapes are unequal");
            }
        }

        private IAPLValue EvaluateDyadicExpression(List<IAPLValue> evaluatedArguments, string operatorName)
        {
            var arg1 = evaluatedArguments[0];
            var arg2 = evaluatedArguments[1];
            var arg1AsIntType = arg1 as APLValue<int>;
            var arg2AsIntType = arg2 as APLValue<int>;
            var arg1AsDoubleType = arg1 as APLValue<double>;
            var arg2AsDoubleType = arg2 as APLValue<double>;
            Func<int, double> lambdaConvertIntToDouble = x => Convert.ToDouble(x);
            Func<int, int> lambdaConvertIntToInt = x => x;
            Func<double, double> lambdaConvertDoubleToDouble = x => x;
            Func<double, double, double> lambdaDoubleOperator = GetDyadicDoubleDoubleOperator(operatorName);
            IAPLValue result;

            if (arg1AsIntType != null)
            {

                if (arg2AsIntType != null)
                {
                    result = EvaluateDyadicExpressionHelper(arg1AsIntType, arg2AsIntType,
                        lambdaConvertIntToInt, lambdaConvertIntToInt, GetDyadicIntIntOperator(operatorName));
                }
                else
                {
                    result = EvaluateDyadicExpressionHelper(arg1AsIntType, arg2AsDoubleType,
                        lambdaConvertIntToDouble, lambdaConvertDoubleToDouble, lambdaDoubleOperator);
                }
            }
            else
            {

                if (arg2AsIntType != null)
                {
                    result = EvaluateDyadicExpressionHelper(arg1AsDoubleType, arg2AsIntType,
                        lambdaConvertDoubleToDouble, lambdaConvertIntToDouble, lambdaDoubleOperator);
                }
                else
                {
                    result = EvaluateDyadicExpressionHelper(arg1AsDoubleType, arg2AsDoubleType,
                        lambdaConvertDoubleToDouble, lambdaConvertDoubleToDouble, lambdaDoubleOperator);
                }
            }

            if (!(result is APLValue<int>) && DyadicOperatorMustReturnInt(operatorName))
            {
                result = result.ConvertToIntEquivalent();
            }

            return result;
        }

        private void EvaluateReductionExpressionHelper2<T>(APLValue<T> arg, Func<T, T, T> operatorLambda, List<T> newScalars)
        {
            var shapeVector = arg.GetShape().Scalars;

            if (shapeVector.Count == 1)
            {
                var vector = arg.Scalars;
                var result = vector[vector.Count - 1];

                for (var i = vector.Count - 2; i >= 0; --i)
                {
                    result = operatorLambda(vector[i], result);
                }

                newScalars.Add(result);
            }
            else
            {

                for (var i = 1; i <= shapeVector[0]; ++i)
                {
                    EvaluateReductionExpressionHelper2<T>(arg.CreateSlice(i), operatorLambda, newScalars);
                }
            }
        }

        private APLValue<T> EvaluateReductionExpressionHelper<T>(APLValue<T> arg, Func<T, T, T> operatorLambda)
        {

            if (arg.IsScalar)
            {
                return arg;
            }

            var newScalars = new List<T>();

            EvaluateReductionExpressionHelper2<T>(arg, operatorLambda, newScalars);

            return new APLValue<T>(arg.GetShape().Scalars.Take(arg.NumberOfDimensions - 1).ToList(), newScalars);
        }

        private IAPLValue EvaluateReductionExpression(IAPLValue arg, string operatorName)
        {
            var dyadicOperatorName = GetDyadicOperatorNameFromReductionName(operatorName);
            IAPLValue result;

            if (arg is APLValue<int>)
            {
                result = EvaluateReductionExpressionHelper((APLValue<int>)arg, GetDyadicIntIntOperator(dyadicOperatorName));
            }
            else
            {
                result = EvaluateReductionExpressionHelper((APLValue<double>)arg, GetDyadicDoubleDoubleOperator(dyadicOperatorName));
            }

            if (!(result is APLValue<int>) && DyadicOperatorMustReturnInt(dyadicOperatorName))
            {
                result = result.ConvertToIntEquivalent();
            }

            return result;
        }

        private IAPLValue EvaluateCompressHelper<T2>(APLValue<int> vector1, APLValue<T2> arg2)
        {

            if (!vector1.Scalars.All(int1 => int1 == 0 || int1 == 1))
            {
                throw new Exception(string.Format("EvaluateCompress() : The vector {0} is not a logical vector.", vector1));
            }

            var numSlices = 0;
            var newScalars = new List<T2>();

            for (int i = 0; i < vector1.Scalars.Count; ++i)
            {

                if (vector1.Scalars[i] != 0)
                {
                    newScalars.AddRange(arg2.CreateSlice(i + 1).Scalars);
                    ++numSlices;
                }
            }

            var newShape = new List<int>(arg2.GetShape().Scalars);

            newShape[0] = numSlices;
            return new APLValue<T2>(newShape, newScalars);
        }

        private IAPLValue EvaluateCompress(IAPLValue arg1, IAPLValue arg2)
        {
            var vector1 = arg1 as APLValue<int>;

            if (vector1 == null)
            {
                vector1 = (APLValue<int>)arg1.ConvertToIntEquivalent();
            }

            if (arg2 is APLValue<int>)
            {
                return EvaluateCompressHelper(vector1, (APLValue<int>)arg2);
            }
            else
            {
                return EvaluateCompressHelper(vector1, (APLValue<double>)arg2);
            }
        }

        private APLValue<T> EvaluateRavelHelper<T>(APLValue<T> arg)
        {
            return arg.ToVector();
        }

        private IAPLValue EvaluateRavel(IAPLValue arg)
        {

            if (arg is APLValue<int>)
            {
                return EvaluateRavelHelper((APLValue<int>)arg);
            }
            else
            {
                return EvaluateRavelHelper((APLValue<double>)arg);
            }
        }

        private IAPLValue EvaluateRestructHelper<T2>(APLValue<int> arg1, APLValue<T2> arg2)
        {

            if (arg1.IsMatrix)
            {
                throw new Exception("EvaluateRestruct() : First argument must not be a matrix.");
            }

            if (arg2.IsNull)
            {
                throw new Exception("EvaluateRestruct() : Second argument must not be null.");
            }

            var shapeVector = (APLValue<int>)arg1.ToVector();
            var arg2Ravel = EvaluateRavelHelper(arg2);

            return new APLValue<T2>(shapeVector.Scalars, arg2Ravel.Scalars);
        }

        private IAPLValue EvaluateRestruct(IAPLValue arg1, IAPLValue arg2)
        {
            var arg1AsIntType = arg1 as APLValue<int>;

            if (arg1AsIntType == null)
            {
                throw new Exception("EvaluateRestruct() : arg1 is not an APLValue<int>");
            }

            if (arg2 is APLValue<int>)
            {
                return EvaluateRestructHelper(arg1AsIntType, (APLValue<int>)arg2);
            }
            else
            {
                return EvaluateRestructHelper(arg1AsIntType, (APLValue<double>)arg2);
            }
        }

        private APLValue<T> EvaluateCatHelper<T>(APLValue<T> arg1, APLValue<T> arg2)
        {
            var arg1Ravel = EvaluateRavelHelper(arg1);
            var arg2Ravel = EvaluateRavelHelper(arg2);
            var newScalars = new List<T>(arg1Ravel.Scalars);

            newScalars.AddRange(arg2Ravel.Scalars);

            return APLValue<T>.CreateVector(newScalars);
        }

        private IAPLValue EvaluateCat(IAPLValue arg1, IAPLValue arg2)
        {

            if (arg1 is APLValue<int> && arg2 is APLValue<int>)
            {
                return EvaluateCatHelper((APLValue<int>)arg1, (APLValue<int>)arg2);
            }
            else if (arg1 is APLValue<double> && arg2 is APLValue<double>)
            {
                return EvaluateCatHelper((APLValue<double>)arg1, (APLValue<double>)arg2);
            }
            else
            {
                throw new Exception("EvaluateCat() : arg1 and arg2 have different element types (int vs. double).");
            }
        }

        private IAPLValue EvaluateIndxHelper(APLValue<int> arg)
        {
            return APLValue<int>.CreateVector(1.To(arg.GetFirstScalar()));
        }

        private IAPLValue EvaluateIndx(IAPLValue arg)
        {

            if (arg is APLValue<int>)
            {
                return EvaluateIndxHelper((APLValue<int>)arg);
            }
            else
            {
                throw new Exception("EvaluateIndx() : arg's element type is not int.");
            }
        }

        private void EvaluateTransHelper2<T>(int dimNum, List<int> shape, List<int> steps, int offset, List<T> oldScalars, List<T> newScalars)
        {

            if (dimNum >= shape.Count)
            {
                newScalars.Add(oldScalars[offset]);
            }
            else
            {
                var length = shape[dimNum];
                var step = steps[dimNum];

                for (var i = 0; i < length; ++i)
                {
                    EvaluateTransHelper2<T>(dimNum + 1, shape, steps, offset, oldScalars, newScalars);
                    offset += step;
                }
            }
        }

        private IAPLValue EvaluateTransHelper<T>(APLValue<T> arg)
        {
#if DEAD_CODE
            if (arg.NumberOfDimensions != 2) // TODO: Consider supporting more than 2 dimensions in the future, perhaps using CreateSlice().
            {
                return arg;
            }

            var numRows = arg.Shape[0];
            var numColumns = arg.Shape[1];
            var newScalars = new List<T>();

            for (var col = 0; col < numColumns; ++col)
            {

                for (var row = 0; row < numRows; ++row)
                {
                    newScalars.Add(arg.Scalars[row * numColumns + col]);
                }
            }

            return new APLValue<T>(new List<int>() { numColumns, numRows }, newScalars);
#else
            // 2014/01/20

            if (arg.NumberOfDimensions < 2)
            {
                return arg;
            }

            // Rotate the Shape and Steps vectors.
            var modifiedShape = new List<int>(arg.Shape);
            var modifiedSteps = new List<int>(arg.Steps);
            var lastShape = modifiedShape[modifiedShape.Count - 1];
            var lastStep = modifiedSteps[modifiedSteps.Count - 1];
            var newScalars = new List<T>();

            modifiedShape.RemoveAt(modifiedShape.Count - 1);
            modifiedSteps.RemoveAt(modifiedSteps.Count - 1);
            modifiedShape.Insert(0, lastShape);
            modifiedSteps.Insert(0, lastStep);

            EvaluateTransHelper2<T>(0, modifiedShape, modifiedSteps, 0, arg.Scalars, newScalars);

            return new APLValue<T>(modifiedShape, newScalars);
#endif
        }

        private IAPLValue EvaluateTrans(IAPLValue arg)
        {

            if (arg is APLValue<int>)
            {
                return EvaluateTransHelper((APLValue<int>)arg);
            }
            else
            {
                return EvaluateTransHelper((APLValue<double>)arg);
            }
        }

        private IAPLValue EvaluateSubscriptingHelper<T1>(APLValue<T1> arg1, APLValue<int> arg2)
        {
            var vector2 = (APLValue<int>)arg2;

            if (arg2.IsIntScalar)
            {
                vector2 = arg2.ToVector();
            }

            var arg1AsValue = (APLValue<T1>)arg1;
            var newScalars = new List<T1>();

            foreach (var n2 in vector2.Scalars)
            {
                newScalars.AddRange(arg1AsValue.CreateSlice(n2).Scalars);
            }

            var newShape = new List<int>(arg1AsValue.GetShape().Scalars);

            newShape[0] = vector2.Scalars.Count;

            return new APLValue<T1>(newShape, newScalars);
        }

        private IAPLValue EvaluateSubscripting(IAPLValue arg1, IAPLValue arg2)
        {
            var arg2AsIntType = arg2 as APLValue<int>;

            if (arg2AsIntType == null)
            {
                throw new Exception("EvaluateSubscripting() : arg2's element type is not int.");
            }

            if (arg1 is APLValue<int>)
            {
                return EvaluateSubscriptingHelper((APLValue<int>)arg1, arg2AsIntType);
            }
            else
            {
                return EvaluateSubscriptingHelper((APLValue<double>)arg1, arg2AsIntType);
            }
        }

        private IAPLValue EvaluateDoubleSubscripting(IAPLValue A, IAPLValue B, IAPLValue C)
        {
            // See Table 3.3 on page 86: ([;] A B C) = (trans ([] (trans ([] A B)) C))

            if (!A.IsMatrix)
            {
                throw new Exception("[;] : A is not a matrix");
            }

            var matrix1 = EvaluateSubscripting(A, B);
            var matrix2 = EvaluateTrans(matrix1);
            var matrix3 = EvaluateSubscripting(matrix2, C);

            return EvaluateTrans(matrix3);
        }

        private APLValue<int> EvaluateRandom(APLValue<int> n, APLValue<int> limit)
        {
            var intList = new List<int>();
            var r = new Random();
            var nValue = n.GetFirstScalar();
            var limitValue = limit.GetFirstScalar();

            for (int i = 0; i < nValue; ++i)
            {
                intList.Add(r.Next(limitValue));
            }

            return APLValue<int>.CreateVector(intList);
        }

        protected override IAPLValue EvaluateAux(List<IAPLValue> evaluatedArguments, EnvironmentFrame<IAPLValue> localEnvironment, IGlobalInfo<IAPLValue> globalInfo)
        {

            switch (OperatorName.Value)
            {
                case "+":
                case "-":
                case "*":
                case "/":
                case "max":
                case "or":
                case "and":
                case "=":   // Note: E.g. (= '(2 3 5 7) '(2 3 5 7)) yields '(1 1 1 1), not 1
                case "<":
                //case ">":
                case "pow":
                    return EvaluateDyadicExpression(evaluatedArguments, OperatorName.Value);

                case "exp":
                case "ln":
                case "sin":
                case "cos":
                case "tan":
                    return EvaluateExpLnEtcExpression(evaluatedArguments[0], OperatorName.Value);

                case "+/":
                case "-/":
                case "*/":
                case "//":
                case "max/":
                case "or/":
                case "and/":
                    return EvaluateReductionExpression(evaluatedArguments[0], OperatorName.Value);

                case "compress":
                    return EvaluateCompress(evaluatedArguments[0], evaluatedArguments[1]);

                case "shape":
                    return evaluatedArguments[0].GetShape();

                case "ravel":
                    return EvaluateRavel(evaluatedArguments[0]);

                case "restruct":
                    return EvaluateRestruct(evaluatedArguments[0], evaluatedArguments[1]);

                case "cat":
                    return EvaluateCat(evaluatedArguments[0], evaluatedArguments[1]);

                case "indx":
                    return EvaluateIndx(evaluatedArguments[0]);

                case "trans":
                    return EvaluateTrans(evaluatedArguments[0]);

                case "[]":
                    return EvaluateSubscripting(evaluatedArguments[0], evaluatedArguments[1]);

                case "[;]":
                    return EvaluateDoubleSubscripting(evaluatedArguments[0], evaluatedArguments[1], evaluatedArguments[2]);

                case "random":
                    return EvaluateRandom((APLValue<int>)evaluatedArguments[0], (APLValue<int>)evaluatedArguments[1]);

                default:
                    return base.EvaluateAux(evaluatedArguments, localEnvironment, globalInfo);
            }
        }
    }

    #endregion

    #region VectorAssignmentUsage

    public class VectorAssignmentUsage : IExpression<IAPLValue>
    {
        public readonly Variable<IAPLValue> VariableName;
        public readonly IExpression<IAPLValue> IndicesExpression;
        public readonly IExpression<IAPLValue> ValuesExpression;

        public VectorAssignmentUsage(Variable<IAPLValue> variableName, IExpression<IAPLValue> indicesExpression, IExpression<IAPLValue> valuesExpression)
        {
            VariableName = variableName;
            IndicesExpression = indicesExpression;
            ValuesExpression = valuesExpression;
        }

        public override string ToString()
        {
            return string.Format("(:= {0} {1} {2})", VariableName, IndicesExpression, ValuesExpression);
        }

        private void AssignVectorElement<T>(List<T> v, int i, T x)
        {

            if (i < 1 || i > v.Count)
            {
                throw new Exception(string.Format(":= : Index {0} is not in the range from 1 to {1}.", i, v.Count));
            }

            v[i - 1] = x;
        }

        public IAPLValue EvaluateHelper<T>(APLValue<T> vectorValue, IAPLValue indicesValue, IAPLValue valuesValue)
        {
            var newIntList = new List<T>(vectorValue.Scalars);

            if (indicesValue.IsIntScalar && valuesValue.IsScalar)
            {
                var indexScalar = (APLValue<int>)indicesValue;
                var valueScalar = (APLValue<T>)valuesValue;

                AssignVectorElement(newIntList, indexScalar.GetFirstScalar(), valueScalar.GetFirstScalar());
            }
            else if (indicesValue.IsIntVector && valuesValue.IsVector)
            {
                var indicesVector = ((APLValue<int>)indicesValue).Scalars;
                var valuesVector = ((APLValue<T>)valuesValue).Scalars;

                if (indicesVector.Count != valuesVector.Count)
                {
                    throw new Exception(string.Format(":= : Indices vector has length {0}; values vector has length {1}.",
                        indicesVector.Count, valuesVector.Count));
                }

                for (var i = 0; i < indicesVector.Count; ++i)
                {
                    AssignVectorElement(newIntList, indicesVector[i], valuesVector[i]);
                }
            }
            else
            {
                throw new Exception(string.Format(":= : Vector has type {0}; indices have type {1}; values have type {2}.",
                    vectorValue.GetType().FullName, indicesValue.GetType().FullName, valuesValue.GetType().FullName));
            }

            return APLValue<T>.CreateVector(newIntList);
        }

        public IAPLValue Evaluate(EnvironmentFrame<IAPLValue> localEnvironment, IGlobalInfo<IAPLValue> globalInfo)
        {
            var vectorValue = VariableName.Evaluate(localEnvironment, globalInfo);
            var indicesValue = IndicesExpression.Evaluate(localEnvironment, globalInfo);
            var valuesValue = ValuesExpression.Evaluate(localEnvironment, globalInfo);

            if (vectorValue is APLValue<int>)
            {
                vectorValue = EvaluateHelper((APLValue<int>)vectorValue, indicesValue, valuesValue);
            }
            else if (vectorValue is APLValue<double>)
            {
                vectorValue = EvaluateHelper((APLValue<double>)vectorValue, indicesValue, valuesValue);
            }
            else
            {
                throw new Exception(":= : v is not a vector");
            }

            // If the variable is not already defined in the local env, we may have to assign it to the global env (assuming that there are only two envs).
            localEnvironment.AddBubbleDown(VariableName, vectorValue);
            return vectorValue;
        }
    }

    #endregion

    #region APLGlobalInfo

    public class APLGlobalInfo : GlobalInfoBase<IAPLValue>
    {
        public readonly IAPLValue TrueVal = APLValue<int>.CreateScalar(1);
        public readonly IAPLValue FalseVal = APLValue<int>.CreateScalar(0);

        public APLGlobalInfo(ITokenizer tokenizer, IParser parser)
            : base(tokenizer, parser)
        {
        }

        public override string LoadPreset(string presetName)
        {

            switch (presetName.ToLower())
            {
                case "min":
                    // min functions (from page 70)
                    Evaluate("(define neg (v) (- 0 v))");
                    Evaluate("(define min (v1 v2) (neg (max (neg v1) (neg v2))))");
                    Evaluate("(define min/ (v) (neg (max/ (neg v))))");
                    break;

                case "reverse":
                    // From page 71
                    Evaluate(@"
(define reverse (a)
    (let ((size ([] (shape a) 1)))
        ([] a (+1 (- size (indx size))))))");
                    break;

                case "find":
                    // From page 72
                    LoadPreset("min");
                    //Evaluate("(define signum (x) (+ (* (< x 0) -1) (> x 0)))");
                    Evaluate("(define signum (x) (- (> x 0) (< x 0)))"); // ThAW 2014/01/24
                    Evaluate("(define abs (x) (* x (signum x)))");
                    Evaluate("(define find (x v) ([] (compress (= x v) (indx (shape v))) 1))");
                    Evaluate(@"
(define find-closest (x v)
    (let ((absdiffs (abs (- v x))))
        (find (min/ absdiffs) absdiffs)))");
                    break;

                case @"+\":
                    // +\ ("+-scan") functions (from page 74)
                    Evaluate("(define dropend (v) ([] v (indx (- (shape v) 1))))");
                    Evaluate(@"
(define +\ (v)
    (if (= (shape v) 0) v
        (cat (+\ (dropend v)) (+/ v))))");
                    break;

                case "<=":
                    Evaluate("(define <= (x y) (or (< x y) (= x y)))");
                    break;

                default:
                    return base.LoadPreset(presetName);
            }

            return string.Format("The preset '{0}' has been successfully loaded.", presetName);
        }

        public override void LoadPresets()
        {
            // Define values for unit testing here.
            Evaluate("(set testvector1 '(1 1 2 3))");
            Evaluate("(set testvector2 '(5 8 13 21))");
            Evaluate("(set logicalvector3 '(1 0 1))");
            Evaluate("(set logicalvector4 '(0 1 1 0))");
            Evaluate("(set floatvector5 '(1.0 1.25 1.5 1.75))");
            Evaluate("(set floatvector6 '(2.0 3.5 5.0 7.5))");
            Evaluate("(set testmatrix1 (restruct '(3 4) '(1 2 3 4 5 6 7 8 9 10 11 12)))");
            Evaluate("(set testmatrix2 (restruct '(3 4) '(2 3 5 7 11 13 17 19 23 29 31 37)))");
            Evaluate("(set logicalmatrix3 (restruct '(3 4) '(1 1 1 1 0 0 0 0 1 1 1 1)))");
            Evaluate("(set logicalmatrix4 (restruct '(3 4) '(0 1 0 1 0 1 0 1 0 1 0 1)))");
            Evaluate("(set floatmatrix5 (restruct '(3 4) '(1.0 2.5 3.0 4.5 5.0 6.5 7.0 8.5 9.0 10.5 11.0 12.5)))");
            Evaluate("(set floatmatrix6 (restruct '(3 4) '(2.0 3.0 5.5 7.5 11.0 13.0 17.5 19.5 23.0 29.0 31.5 37.5)))");

            GlobalEnvironment.Add(new Variable<IAPLValue>("e", 0, 0), APLValue<double>.CreateScalar(Math.E));
            GlobalEnvironment.Add(new Variable<IAPLValue>("pi", 0, 0), APLValue<double>.CreateScalar(Math.PI));

            // Define commonly-used functions here.
            Evaluate("(define > (x y) (< y x))");
            Evaluate("(define mod (m n) (- m (* n (/ m n))))");
            Evaluate("(define +1 (n) (+ n 1))");
            /* TODO:
            Evaluate("(define )");
             */
        }

        public override IAPLValue FalseValue
        {
            get
            {
                return FalseVal;
            }
        }

        public override IAPLValue TrueValue
        {
            get
            {
                return TrueVal;
            }
        }

        public override bool ValueIsInteger(IAPLValue value)
        {
            return value.IsIntScalar;
        }

        public override int ValueAsInteger(IAPLValue value)
        {
            var valueAsScalar = value as APLValue<int>;

            if (valueAsScalar == null)
            {
                throw new ArgumentException("ValueAsInteger() : The value is not an integer scalar.");
            }
            else if (valueAsScalar.IsNull)
            {
                throw new ArgumentException("ValueAsInteger() : The value is a null scalar.");
            }

            return valueAsScalar.GetFirstScalar();
        }

        public override IAPLValue IntegerAsValue(int value)
        {
            return APLValue<int>.CreateScalar(value);
        }

        public static string APLDoubleToString(double d)
        {
            // E.g. If d == 13.0, we want the string to be "13.0" rather than just "13", so that we can distinguish it from an integer.
            // Note: d == (tan (/ pi 4)) is a good test case; it should yield "1.0", not "1".
            var result = d.ToString();

            if (result.IndexOf('.') < 0 && result.IndexOf('E') < 0) // I.e. If result does not contain either '.' or 'E'.
            {
                result = result + ".0";
            }

            return result;
        }
    }

    #endregion
}
