using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

using IConvertibleToGraph = Inference.Interpreter.SASL.IConvertibleToGraph;
using Thunk = Inference.Interpreter.SASL.Thunk;

namespace Inference.Interpreter.LISP
{
    #region Interfaces

    public interface ISExpression
    {
        bool IsNumber();
        bool IsSymbol();
        bool IsList();
        bool IsNull();
        bool IsPrimOp();
        bool IsClosure();
        bool IsString();
    }

    public interface INumber
    {
        int ToInteger();
        double ToDouble();
    }

    #endregion

    #region LISPException

    public class LISPException : EvaluationException
    {
        public LISPException(string message, int line, int column)
            : base(message, line, column)
        {
        }
    }

    #endregion

    #region SExpressionBareBase

    public class SExpressionBareBase : ISExpression
    {
        public virtual bool IsNumber()
        {
            return false;
        }

        public virtual bool IsSymbol()
        {
            return false;
        }

        public virtual bool IsList()
        {
            return false;
        }

        public virtual bool IsNull()
        {
            return false;
        }

        public virtual bool IsPrimOp()
        {
            return false;
        }

        public virtual bool IsClosure()
        {
            return false;
        }

        public virtual bool IsString()
        {
            return false;
        }
    }

    #endregion

    #region SExpressionBase

    public class SExpressionBase : SExpressionBareBase, IExpression<ISExpression>
    {
        public virtual ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            return this;
        }
    }

    #endregion

    #region LISPOperatorUsage

    public class LISPOperatorUsage : OperatorUsage<ISExpression>
    {
        private static readonly HashSet<string> OperatorsThatTakeEitherIntOrFloatArgs = new HashSet<string>() { "<", /* ">", */ "+", "-", "*", "/" };

        public LISPOperatorUsage(Name operatorName, ExpressionList<ISExpression> expressionList)
            : base(operatorName, expressionList)
        {
        }

        protected override bool TryGetExpectedNumArgs(IGlobalInfo<ISExpression> globalInfo, out int result)
        {

            if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
            {
                result = 1;
                return true;
            }
            else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value) ||
                DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
            {
                result = 2;
                return true;
            }

            switch (OperatorName.Value)
            {
                case "list":
                    result = -1;  // Any number of arguments is permitted.
                    return true;

                case "car":
                case "cdr":
                case "number?":
                case "symbol?":
                case "list?":
                case "null?":
                case "string?":
                case "random":
                case "tostring":
                case "listtostring":
                case "stringtolist":
                case "stringtosymbol":
                case "floor":
                case "throw":
                    result = 1;
                    return true;

                case "cons":
                case "rplaca":
                case "rplacd":
                case "string<":
                    result = 2;
                    return true;

                default:
                    return base.TryGetExpectedNumArgs(globalInfo, out result);
            }
        }

#if DEAD_CODE
        private bool IsListOfStrings(ISExpression arg)
        {

            if (arg is NullSExpression)
            {
                return true;
            }

            var argAsList = arg as SExpressionList;

            if (argAsList == null || !(argAsList.Head is LISPString))
            {
                return false;
            }

            return IsListOfStrings(argAsList.Tail);
        }
#endif

        protected override string CheckArgTypes(List<ISExpression> evaluatedArguments)
        {

            switch (OperatorName.Value)
            {
                case "+":
                case "-":
                case "*":
                case "/":
                case "<":
                //case ">":
                case "pow":
                case "atan2":

                    if (!evaluatedArguments[0].IsNumber())
                    {
                        return "First argument is not a number";
                    }

                    if (!evaluatedArguments[1].IsNumber())
                    {
                        return "Second argument is not a number";
                    }

                    break;

                case "car":
                case "cdr":
                case "rplaca":
                case "rplacd":

                    if (!evaluatedArguments[0].IsList())
                    {
                        //throw new ArgumentException(string.Format("Operator {0} : Argument '{1}' of type {2} is not a list.",
                        //    OperatorName.Value, evaluatedArguments[0], evaluatedArguments[0].GetType().FullName));
                        return string.Format("Argument '{0}' of type {1} is not a list",
                            evaluatedArguments[0], evaluatedArguments[0].GetType().FullName);
                    }

                    break;

                case "random":
                case "exp":
                case "ln":
                case "sin":
                case "cos":
                case "tan":
                case "floor":

                    if (!evaluatedArguments[0].IsNumber())
                    {
                        //throw new ArgumentException(string.Format("Operator {0} : Argument is not a number.", OperatorName.Value));
                        return "Argument is not a number";
                    }

                    break;

                case "listtostring":

                    //if (!IsListOfStrings(evaluatedArguments[0]))
                    if (!evaluatedArguments[0].IsList() && !evaluatedArguments[0].IsNull())
                    {
                        //throw new ArgumentException(string.Format("Operator {0} : Argument is not a list or a null.", OperatorName.Value));
                        return "Argument is not a list or a null";
                    }

                    break;

                case "stringtolist":
                case "stringtosymbol":
                case "throw":

                    if (!evaluatedArguments[0].IsString())
                    {
                        //throw new ArgumentException(string.Format("Operator {0} : Argument is not a string.", OperatorName.Value));
                        return "Argument is not a string";
                    }

                    break;

                case "string<":

                    if (!evaluatedArguments[0].IsString())
                    {
                        return "First argument is not a string";
                    }

                    if (!evaluatedArguments[1].IsString())
                    {
                        return "Second argument is not a string";
                    }

                    break;

                default:
                    return base.CheckArgTypes(evaluatedArguments);
            }

            return null;
        }

        protected override bool TryInvokeMacro(
            List<IExpression<ISExpression>> unevaluatedArguments,
            EnvironmentFrame<ISExpression> localEnvironment,
            IGlobalInfo<ISExpression> globalInfo,
            out ISExpression macroResult)
        {

            if (!globalInfo.MacroDefinitions.ContainsKey(OperatorName))
            {
                macroResult = null;
                return false;
            }

            macroResult = globalInfo.MacroDefinitions[OperatorName].InvokeMacro(unevaluatedArguments, localEnvironment, globalInfo);
            return true;
        }

        private static void ListToStringHelper(ISExpression arg, StringBuilder sb)
        {

            if (arg is NullSExpression)
            {
                return;
            }

            var argAsList = arg as SExpressionList;

            if (argAsList == null)
            {
                sb.Append(arg.ToString());  // This should never get called.
            }
            else
            {
                sb.Append(argAsList.Head.ToString());
                ListToStringHelper(argAsList.Tail, sb);
            }
        }

        private static ISExpression StringToListHelper(string str, int i)
        {

            if (i >= str.Length)
            {
                return new NullSExpression();
            }

            return new SExpressionList(
                new LISPString(str.Substring(i, 1)),
                StringToListHelper(str, i + 1));
        }

        protected override void UpdateStackTrace(EnvironmentFrame<ISExpression> oldEnvFrame, EnvironmentFrame<ISExpression> newEnvFrame,
            int line, int column)
        {
            LISPGlobalInfo.CreateStackTraceInNewEnvironmentFrame(oldEnvFrame, newEnvFrame, line, column);
        }

        private ISExpression EvaluateAuxFloat(List<ISExpression> evaluatedArguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            // Currently, EvaluateAuxFloat() is only called for two-argument functions.
            var firstArgAsDouble = ((INumber)evaluatedArguments[0]).ToDouble();
            var secondArgAsDouble = ((INumber)evaluatedArguments[1]).ToDouble();

            if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
            {
                return new FloatLiteral(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsDouble, secondArgAsDouble));
            }
            else if (DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
            {
                return DoubleOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsDouble, secondArgAsDouble) ? globalInfo.TrueValue : globalInfo.FalseValue;
            }

            throw new Exception(string.Format("LISPOperatorUsage.EvaluateAuxFloat() : Invalid operator {0}", OperatorName.Value));
        }

        protected override ISExpression EvaluateAux(List<ISExpression> evaluatedArguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            SExpressionList sExprList;

            switch (OperatorName.Value)
            {
                case "cons":
                    return new SExpressionList(evaluatedArguments[0], evaluatedArguments[1]);

                case "car":
                    sExprList = (SExpressionList)evaluatedArguments[0];
                    return sExprList.Head;

                case "cdr":
                    sExprList = (SExpressionList)evaluatedArguments[0];
                    return sExprList.Tail;

                case "rplaca":
                    sExprList = (SExpressionList)evaluatedArguments[0];
                    sExprList.Head = evaluatedArguments[1];
                    return sExprList.Head;

                case "rplacd":
                    sExprList = (SExpressionList)evaluatedArguments[0];
                    sExprList.Tail = evaluatedArguments[1];
                    return sExprList.Tail;

                case "number?":
                    return evaluatedArguments[0].IsNumber() ? globalInfo.TrueValue : globalInfo.FalseValue;

                case "symbol?":
                    return evaluatedArguments[0].IsSymbol() ? globalInfo.TrueValue : globalInfo.FalseValue;

                case "list?":
                    return evaluatedArguments[0].IsList() ? globalInfo.TrueValue : globalInfo.FalseValue;

                case "null?":
                    return evaluatedArguments[0].IsNull() ? globalInfo.TrueValue : globalInfo.FalseValue;

                case "string?":
                    return evaluatedArguments[0].IsString() ? globalInfo.TrueValue : globalInfo.FalseValue;

                case "list":
                    return SExpressionList.MakeFromList(evaluatedArguments);

                case "random":
                    return new IntegerLiteral(globalInfo.RandomNumberGenerator.Next(globalInfo.ValueAsInteger(evaluatedArguments[0])));

                case "tostring":
                    return new LISPString(evaluatedArguments[0].ToString());

                case "listtostring":
                    var sbListToString = new StringBuilder();

                    ListToStringHelper(evaluatedArguments[0], sbListToString);
                    return new LISPString(sbListToString.ToString());

                case "stringtolist":
                    var str2list = (LISPString)evaluatedArguments[0];

                    return StringToListHelper(str2list.Value, 0);

                case "stringtosymbol":
                    var str2sym = (LISPString)evaluatedArguments[0];

                    return new LISPSymbol(str2sym.Value);

                case "floor":
                    return new IntegerLiteral(((INumber)evaluatedArguments[0]).ToInteger());

                case "throw":
                    throw new LISPException(((LISPString)evaluatedArguments[0]).Value, OperatorName.Line, OperatorName.Column);

                case "string<": // See page 54.
                    return ((LISPString)evaluatedArguments[0]).Value.CompareTo(((LISPString)evaluatedArguments[1]).Value) < 0
                        ? globalInfo.TrueValue : globalInfo.FalseValue;

                default:

                    if (OperatorsThatTakeEitherIntOrFloatArgs.Contains(OperatorName.Value))
                    {

                        if (evaluatedArguments.Any(arg => arg is FloatLiteral))
                        {
                            return EvaluateAuxFloat(evaluatedArguments, localEnvironment, globalInfo); ;
                        }
                        else
                        {
                            // This line is a copy of a line below, but we must handle this here
                            // so that we don't fall through to the DoubleOperatorKeeper.TwoArgumentOperators case.
                            return base.EvaluateAux(evaluatedArguments, localEnvironment, globalInfo);
                        }
                    }
                    // The next two cases involve operators that must take arguments as doubles, not ints.
                    else if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
                    {
                        return new FloatLiteral(DoubleOperatorKeeper.OneArgumentOperators[OperatorName.Value](((INumber)evaluatedArguments[0]).ToDouble()));
                    }
                    else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
                    {
                        return new FloatLiteral(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](
                            ((INumber)evaluatedArguments[0]).ToDouble(),
                            ((INumber)evaluatedArguments[1]).ToDouble()));
                    }

                    return base.EvaluateAux(evaluatedArguments, localEnvironment, globalInfo); // This handles = for all types
            }
        }
    }

    #endregion

    #region QuotedConstantWithApostrophe

    public class QuotedConstantWithApostrophe : IExpression<ISExpression>
    {
        public readonly ISExpression sexpression;

        public QuotedConstantWithApostrophe(ISExpression sexpression)
        {
            this.sexpression = sexpression;
        }

        public override string ToString()
        {
            return "'" + this.sexpression.ToString();
        }

        public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            return this.sexpression;
        }
    }

    #endregion

    #region QuotedConstantWithQuoteKeyword

    public class QuotedConstantWithQuoteKeyword : SExpressionBase
    {
        public readonly ISExpression sexpression;

        public QuotedConstantWithQuoteKeyword(ISExpression sexpression)
        {
            this.sexpression = sexpression;
        }

        public override string ToString()
        {
            return string.Format("(quote {0})", this.sexpression);
        }

        public override ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            return this.sexpression;
        }
    }

    #endregion

    #region IntegerLiteral

    public class IntegerLiteral : SExpressionBase, INumber, IConvertibleToGraph
    {
        public readonly int Value;

        public IntegerLiteral(object value)
        {

            if (!(value is int))
            {
                throw new ArgumentException("IntegerLiteral constructor: value is not an int.", "value");
            }

            Value = (int)value;
        }

        public override string ToString()
        {
            return Value.ToString();
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            IntegerLiteral otherIntLit = obj as IntegerLiteral;

            return otherIntLit != null && Value == otherIntLit.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public int ToInteger()
        {
            return Value;
        }

        public double ToDouble()
        {
            return Convert.ToDouble(Value);
        }

        public override bool IsNumber()
        {
            return true;
        }

        public IExpression<ISExpression> ConvertToGraph()
        {
            return this;
        }
    }

    #endregion

    #region FloatLiteral

    public class FloatLiteral : SExpressionBase, INumber //, IConvertibleToGraph
    {
        public readonly double Value;

        public FloatLiteral(object value)
        {

            if (!(value is double))
            {
                throw new ArgumentException("FloatLiteral constructor: value is not a double.", "value");
            }

            Value = (double)value;
        }

        public override string ToString()
        {
            // E.g. If d == 13.0, we want the string to be "13.0" rather than just "13", so that we can distinguish it from an integer.
            // Note: d == (tan (/ pi 4)) is a good test case; it should yield "1.0", not "1".
            var result = Value.ToString();

            if (result.IndexOf('.') < 0 && result.IndexOf('E') < 0) // I.e. If result does not contain either '.' or 'E'.
            {
                result = result + ".0";
            }

            return result;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            FloatLiteral otherFltLit = obj as FloatLiteral;

            return otherFltLit != null && Value == otherFltLit.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public int ToInteger()
        {
            return Convert.ToInt32(Math.Floor(Value));
        }

        public double ToDouble()
        {
            return Value;
        }

        public override bool IsNumber()
        {
            return true;
        }

        /*
        public IExpression<ISExpression> ConvertToGraph()
        {
            return this;
        }
         */
    }

    #endregion

    #region LISPSymbol

    public class LISPSymbol : SExpressionBase
    {
        public readonly string Value;

        public LISPSymbol(string value)
        {

            if (string.IsNullOrEmpty(value))
            {
                throw new ArgumentException("LISPSymbol constructor: value is null or empty.", "value");
            }

            Value = value;
        }

        public override string ToString()
        {
            return Value;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            LISPSymbol otherSymbol = obj as LISPSymbol;

            return otherSymbol != null && Value == otherSymbol.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public override bool IsSymbol()
        {
            return true;
        }
    }

    #endregion

    #region LISPString

    public class LISPString : SExpressionBase
    {
        public readonly string Value;

        public LISPString(string value)
        {

            if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
            {
                throw new ArgumentException("LISPString constructor: value is null.", "value");
            }

            Value = value;
        }

        public override string ToString()
        {
            return Value;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            LISPString otherString = obj as LISPString;

            return otherString != null && Value == otherString.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public override bool IsString()
        {
            return true;
        }
    }

    #endregion

    #region NullSExpression

    public class NullSExpression : SExpressionBase
    {
        public NullSExpression()
        {
        }

        public override string ToString()
        {
            return "()";
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            return obj is NullSExpression;
        }

        public override int GetHashCode()
        {
            return 0;
        }

        public override bool IsNull()
        {
            return true;
        }
    }

    #endregion

    #region SExpressionList

    public class SExpressionList : SExpressionBareBase
    {
        // These two data members cannot be readonly, because of thunk evaluation.
        public ISExpression Head;
        public ISExpression Tail;

        public SExpressionList(ISExpression head, ISExpression tail)
        {
            Head = head;
            Tail = tail;
        }

        private static ISExpression MakeFromListHelper(List<ISExpression> l, int i)
        {

            if (i >= l.Count)
            {
                return new NullSExpression();
            }

            return new SExpressionList(l[i], MakeFromListHelper(l, i + 1));
        }

        public static ISExpression MakeFromList(List<ISExpression> l)
        {
            return MakeFromListHelper(l, 0);
        }

        private string ToStringWithoutBrackets()
        {
            var headAsString = Head.ToString();

            if (Tail is NullSExpression)
            {
                return headAsString;
            }
            else if (Tail is Thunk)
            {
                return string.Format("{0} {1}", headAsString, Tail);
            }
            else if (Tail is SExpressionList)
            {
                var tail = Tail as SExpressionList;

                return string.Format("{0} {1}", headAsString, tail.ToStringWithoutBrackets());
            }
            else // Tail is a symbol, an integer literal, a string, a closure, etc.
            {
                return string.Format("{0} . {1}", headAsString, Tail);
            }
        }

        public override string ToString()
        {
            return "(" + ToStringWithoutBrackets() + ")";
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherSExprList = obj as SExpressionList;

            return otherSExprList != null && Head.Equals(otherSExprList.Head) && Tail.Equals(otherSExprList.Tail);
        }

        public override int GetHashCode()
        {
            // The + 1 is in case this is a list of one element; we don't want (list of head) to have the same hash code as head itself,
            // since they are both ISExpressions, and could be together in the same set or dictionary.
            return Head.GetHashCode() + 101 * Tail.GetHashCode() + 1;
        }

        public override bool IsList()
        {
            return true;
        }

        public int Length
        {
            get
            {
                var tail = Tail as SExpressionList;

                return (tail == null) ? 1 : tail.Length + 1;
            }
        }
    }

    #endregion

    #region MacroDefinition

    public class MacroDefinition : IExpression<ISExpression>, IMacroDefinition<ISExpression>
    {
        public readonly Name MacroName;
        public readonly VariableList<ISExpression> ArgList;
        public readonly IExpression<ISExpression> Body;

        public MacroDefinition(Name macroName, VariableList<ISExpression> argList, IExpression<ISExpression> body)
        {
            MacroName = macroName;
            ArgList = argList;
            Body = body;
        }

        public override string ToString()
        {
            return string.Format("(define-macro {0} {1} {2})", MacroName, ArgList, Body);
        }

        public int ArgumentCount
        {
            get
            {
                return ArgList.Value.Count;
            }
        }

        public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
        {
            globalInfo.MacroDefinitions[MacroName] = this;
            return globalInfo.TrueValue;
        }

        private static string SExpressionListToString_ApostrophesToQuoteKeywords(SExpressionList l)
        {
            var headAsString = ObjectToString_ApostrophesToQuoteKeywords(l.Head);

            if (l.Tail is NullSExpression)
            {
                return headAsString;
            }
            else if (l.Tail is Thunk)
            {
                return string.Format("{0} {1}", headAsString, l.Tail);
            }
            else if (l.Tail is SExpressionList)
            {
                var tail = l.Tail as SExpressionList;

                return string.Format("{0} {1}", headAsString, SExpressionListToString_ApostrophesToQuoteKeywords(tail));
            }
            else // Tail is a symbol, an integer literal, a string, a closure, etc.
            {
                return string.Format("{0} . {1}", headAsString, ObjectToString_ApostrophesToQuoteKeywords(l.Tail));
            }
        }

        public static string ObjectToString_ApostrophesToQuoteKeywords(object expr)
        {

            if (expr is FunctionDefinition<ISExpression>)
            {
                var fd = (FunctionDefinition<ISExpression>)expr;

                return string.Format("(define {0} {1} {2})", fd.FunctionName, fd.ArgList, ObjectToString_ApostrophesToQuoteKeywords(fd.Body));
            }
            else if (expr is IfUsage<ISExpression>)
            {
                var iu = (IfUsage<ISExpression>)expr;

                return string.Format("(if {0} {1} {2})",
                    ObjectToString_ApostrophesToQuoteKeywords(iu.Condition),
                    ObjectToString_ApostrophesToQuoteKeywords(iu.IfBody),
                    ObjectToString_ApostrophesToQuoteKeywords(iu.ElseBody));
            }
            else if (expr is WhileUsage<ISExpression>)
            {
                var wu = (WhileUsage<ISExpression>)expr;

                return string.Format("(while {0} {1})",
                    ObjectToString_ApostrophesToQuoteKeywords(wu.Condition),
                    ObjectToString_ApostrophesToQuoteKeywords(wu.Body));
            }
            else if (expr is SetUsage<ISExpression>)
            {
                var su = (SetUsage<ISExpression>)expr;

                return string.Format("(set {0} {1})", su.VariableName,
                    ObjectToString_ApostrophesToQuoteKeywords(su.Expression));
            }
            else if (expr is BeginUsage<ISExpression>)
            {
                var bu = (BeginUsage<ISExpression>)expr;

                return string.Format("(begin {0} {1})",
                    ObjectToString_ApostrophesToQuoteKeywords(bu.FirstExpression),
                    string.Join(" ", bu.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
            }
            else if (expr is CondUsage<ISExpression>)
            {
                var cu = (CondUsage<ISExpression>)expr;

                return string.Format("(cond {0})", string.Join(" ", cu.ExprPairList.Select(ep => string.Format("({0} {1})",
                    ObjectToString_ApostrophesToQuoteKeywords(ep.Key),
                    ObjectToString_ApostrophesToQuoteKeywords(ep.Value)))));
            }
            else if (expr is LetUsage<ISExpression>)
            {
                var lu = (LetUsage<ISExpression>)expr;

                return string.Format("(let ({0}) {1})",
                    string.Join(" ", lu.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
                    ObjectToString_ApostrophesToQuoteKeywords(lu.Expression));
            }
            else if (expr is LetStarUsage<ISExpression>)
            {
                var lsu = (LetStarUsage<ISExpression>)expr;

                return string.Format("(let* ({0}) {1})",
                    string.Join(" ", lsu.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
                    ObjectToString_ApostrophesToQuoteKeywords(lsu.Expression));
            }
            else if (expr is OperatorUsage<ISExpression> && !(expr is Scheme.PrimOp))
            {
                var ou = (OperatorUsage<ISExpression>)expr;

                if (ou.ExpressionList.Value.Count == 0)
                {
                    return string.Format("({0})", ou.OperatorName);
                }

                return string.Format("({0} {1})", ou.OperatorName,
                    string.Join(" ", ou.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
            }
            else if (expr is QuotedConstantWithApostrophe)
            {
                var qc = (QuotedConstantWithApostrophe)expr;

                return string.Format("(quote {0})", qc.sexpression);
            }
                /*
            else if (expr is SExpressionList) // Not an IExpression<ISExpression>
            {
                return string.Format("({0})", SExpressionListToString_ApostrophesToQuoteKeywords((SExpressionList)expr));
            }
                 */
            else if (expr is MacroDefinition)
            {
                var md = (MacroDefinition)expr;

                return string.Format("(define-macro {0} {1} {2})", md.MacroName, md.ArgList, ObjectToString_ApostrophesToQuoteKeywords(md.Body));
            }
            else if (expr is Scheme.LambdaExpression)
            {
                var le = (Scheme.LambdaExpression)expr;

                return string.Format("(lambda {0} {1})", le.ArgList, ObjectToString_ApostrophesToQuoteKeywords(le.Body));
            }
            else if (expr is Scheme.EvaluableExpression)
            {
                var ee = (Scheme.EvaluableExpression)expr;
                var feAsString = ObjectToString_ApostrophesToQuoteKeywords(ee.FirstExpression);

                if (ee.ExpressionList.Value.Count == 0)
                {
                    return string.Format("({0})", feAsString);
                }

                return string.Format("({0} {1})", feAsString,
                    string.Join(" ", ee.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
            }
            else if (expr is Scheme.LetRecUsage)
            {
                var lru = (Scheme.LetRecUsage)expr;

                return string.Format("(letrec ({0}) {1})",
                    string.Join(" ", lru.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
                    ObjectToString_ApostrophesToQuoteKeywords(lru.Expression));
            }
            else if (expr is Scheme.CallCCUsage)
            {
                var cccu = (Scheme.CallCCUsage)expr;

                return string.Format("(call/cc {0})", ObjectToString_ApostrophesToQuoteKeywords(cccu.Body));
            }
            else
            {
                return expr.ToString();
            }
        }

        private static ISExpression ExpressionToSExpression(IExpression<ISExpression> expr, IGlobalInfo<ISExpression> globalInfo)
        {
            string quotedConstStr;

            if (expr is QuotedConstantWithApostrophe)
            {
                quotedConstStr = expr.ToString();
            }
            else
            {
                quotedConstStr = "'" + ObjectToString_ApostrophesToQuoteKeywords(expr);
            }

            object parserResult;

            try
            {
                parserResult = globalInfo.Parser.Parse(globalInfo.Tokenizer.Tokenize(quotedConstStr));
            }
            catch (Exception ex)
            {
                throw new Exception(string.Format("Error while parsing {0} : {1}", quotedConstStr, ex.Message));
            }

            if (!(parserResult is QuotedConstantWithApostrophe))
            {
                throw new Exception(string.Format(
                    "MacroDefinition.ExpressionToSExpression() : The following did not parse to a quoted constant with apostrophe: {0}",
                    quotedConstStr));
            }

            var quotedConst = (QuotedConstantWithApostrophe)parserResult;

            return quotedConst.Evaluate(null, globalInfo);
        }

        private static string SExpressionListToStringWithoutBracketsForReparse(SExpressionList l)
        {
            var headAsString = SExpressionToStringForReparse(l.Head);

            if (l.Tail is NullSExpression)
            {
                return headAsString;
            }
            else if (l.Tail is Thunk)
            {
                return string.Format("{0} {1}", headAsString, SExpressionToStringForReparse(l.Tail));
            }
            else if (l.Tail is SExpressionList)
            {
                var tail = (SExpressionList)l.Tail;

                return string.Format("{0} {1}", headAsString, SExpressionListToStringWithoutBracketsForReparse(tail));
            }
            else // Tail is a symbol, an integer literal, a string, a closure, etc.
            {
                return string.Format("{0} . {1}", headAsString, SExpressionToStringForReparse(l.Tail));
            }
        }

        public static string SExpressionToStringForReparse(ISExpression sexpression)
        {
            // Convert the first level of quote keywords to apostrophes; e.g.:
            // (quote foo) -> "'foo"
            // (quote (quote foo)) -> "'(quote foo)"
            // ((quote foo) (quote bar)) -> "('foo 'bar)"

            var qc = sexpression as QuotedConstantWithQuoteKeyword;

            if (qc != null)
            {
                return "'" + qc.sexpression.ToString();
            }

            var l = sexpression as SExpressionList;

            if (l == null)
            {
                return sexpression.ToString();
            }
                /*
            else if (l.Head.ToString() == "quote")
            {
                var l2 = l.Tail as SExpressionList;

                if (l2 != null && l2.Length == 1)
                {
                    return "'" + l2.Head.ToString();
                }
                else
                {
                    return "'" + l.Tail.ToString();
                }
            }
                 */
            else
            {
                return string.Format("({0})", SExpressionListToStringWithoutBracketsForReparse(l));
            }
        }

        public ISExpression InvokeMacro(
            List<IExpression<ISExpression>> unevaluatedArguments,
            EnvironmentFrame<ISExpression> localEnvironment,
            IGlobalInfo<ISExpression> globalInfo)
        {
            var rhoPrime = new EnvironmentFrame<ISExpression>(localEnvironment);

            rhoPrime.Compose(ArgList.Value, unevaluatedArguments.Select(expr => ExpressionToSExpression(expr, globalInfo)).ToList());

            var substitutedBody = Body.Evaluate(rhoPrime, globalInfo);
            var substitutedBodyAsString = SExpressionToStringForReparse(substitutedBody);
            object parserResult;

            try
            {
                parserResult = globalInfo.Parser.Parse(globalInfo.Tokenizer.Tokenize(substitutedBodyAsString));
            }
            catch (Exception ex)
            {
                throw new Exception(string.Format("Error while parsing {0} : {1}", substitutedBodyAsString, ex.Message));
            }

            if (!(parserResult is IExpression<ISExpression>))
            {
                throw new Exception(string.Format(
                    "MacroDefinition.InvokeMacro() : The following did not parse to an IExpression<ISExpression>: {0}",
                    substitutedBodyAsString));
            }

            var exprParsed = (IExpression<ISExpression>)parserResult;

            return exprParsed.Evaluate(localEnvironment, globalInfo);
        }
    }

    #endregion

    #region LISPGlobalInfo

    public class LISPGlobalInfo : GlobalInfoBase<ISExpression>
    {
        private readonly ISExpression TrueVal = new LISPSymbol("T");       // Symbols are immutable
        private readonly ISExpression FalseVal = new NullSExpression();    // This is immutable too
        private readonly Dictionary<Name, IMacroDefinition<ISExpression>> MacroDefs = new Dictionary<Name, IMacroDefinition<ISExpression>>();
        public static readonly Variable<ISExpression> varStackTrace = new Variable<ISExpression>("__STACK_TRACE__", 0, 0);

        public LISPGlobalInfo(ITokenizer tokenizer, IParser parser)
            : base(tokenizer, parser)
        {
        }

        public override string LoadPreset(string presetName)
        {

            switch (presetName.ToLower())
            {
                case "assoc":
                    // Association list functions (from page 32)
                    Evaluate("(define caar (x) (car (car x)))");
                    Evaluate("(define cadar (x) (car (cdr (car x))))");
                    Evaluate(@"
(define assoc (x alist)
    (if (null? alist) '()
        (if (= x (caar alist)) (cadar alist)
            (assoc x (cdr alist)))))");
                    Evaluate(@"
(define mkassoc (x y alist)
    (if (null? alist)
        (list (list x y))
        (if (= x (caar alist)) (cons (list x y) (cdr alist))
            (cons (car alist) (mkassoc x y (cdr alist))))))");
                    // Additional function
                    Evaluate(@"
(define assoc-contains-key (x alist)
    (if (null? alist) '()
        (if (= x (caar alist)) 'T
            (assoc-contains-key x (cdr alist)))))");
                    // From page 55
                    Evaluate(@"
(define rplac-assoc (x y alist)
    (if (null? alist) '()
        (if (= x (caar alist))
            (rplacd (car alist) (list y))
            (if (null? (cdr alist))
                (rplacd alist (list (list x y)))
                (rplac-assoc x y (cdr alist))))))");
                    break;

                case "set":
                    // Set functions (from page 34)
                    Evaluate("(set nullset '())");
                    Evaluate(@"
(define member? (x s)
    (if (null? s) '()
        (if (equal x (car s)) 'T (member? x (cdr s)))))");
                    Evaluate("(define addelt (x s) (if (member? x s) s (cons x s)))");
                    Evaluate("(define size (s) (length s))");
                    Evaluate(@"
(define union (s1 s2)
    (if (null? s1) s2
        (if (member? (car s1) s2)
            (union (cdr s1) s2)
            (cons (car s1) (union (cdr s1) s2)))))");
                    // Additional set functions (from page 43)
                    Evaluate(@"
(define inter (s1 s2)
    (if (null? s1) s1
        (if (member? (car s1) s2)
            (cons (car s1) (inter (cdr s1) s2))
            (inter (cdr s1) s2))))");
                    Evaluate(@"
(define diff (s1 s2)
    (if (null? s1) s1
        (if (null? s2) s1
            (if (member? (car s1) s2)
                (diff (cdr s1) s2)
                (cons (car s1) (diff (cdr s1) s2))))))");
                    break;

                case "queue":
                    // Queue functions (from page 37)
                    Evaluate("(set empty-queue '())");
                    Evaluate("(define front (q) (car q))");
                    Evaluate("(define rm-front (q) (cdr q))");
                    Evaluate("(define enqueue (t q) (if (null? q) (list t) (cons (car q) (enqueue t (cdr q)))))");
                    Evaluate("(define empty? (q) (null? q))");
                    break;

                default:
                    //throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
                    return base.LoadPreset(presetName);
            }

            return string.Format("The preset '{0}' has been successfully loaded.", presetName);
        }

        public override void LoadPresets()
        {
            GlobalEnvironment.Add(varStackTrace, new NullSExpression());
            GlobalEnvironment.Add(new Variable<ISExpression>("e", 0, 0), new FloatLiteral(Math.E));
            GlobalEnvironment.Add(new Variable<ISExpression>("pi", 0, 0), new FloatLiteral(Math.PI));

            Evaluate("(define > (x y) (< y x))");
            Evaluate("(define not (x) (if x '() 'T))"); // Page 30
            Evaluate("(define and (x y) (if x y x))");  // Page 30
            Evaluate("(define or (x y) (if x x y))");   // Page 30
            Evaluate("(define atom? (x) (or (null? x) (or (number? x) (symbol? x))))"); // Page 31
            Evaluate(@"
(define equal (l1 l2)
    (if (atom? l1) (= l1 l2)
        (if (atom? l2) '()
            (if (equal (car l1) (car l2))
                (equal (cdr l1) (cdr l2))
                '()))))"); // Page 31
            Evaluate("(define +1 (n) (+ n 1))");
            Evaluate("(define append (list1 list2) (if (null? list1) list2 (cons (car list1) (append (cdr list1) list2))))");   // Similar to Page 45
            Evaluate("(define length (l) (if (null? l) 0 (+1 (length (cdr l)))))");     // Page 29
            Evaluate("(define nth (n l) (if (= n 0) (car l) (nth (- n 1) (cdr l))))");  // Page 43
            Evaluate("(define mod (m n) (- m (* n (/ m n))))");                 // Page 8
            Evaluate("(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))");    // Page 8
        }

        public override ISExpression FalseValue
        {
            get
            {
                return FalseVal;
            }
        }

        public override ISExpression TrueValue
        {
            get
            {
                return TrueVal;
            }
        }

        public override bool ValueIsFalse(ISExpression value)
        {
            return value.IsNull();
        }

        public override Dictionary<Name, IMacroDefinition<ISExpression>> MacroDefinitions
        {
            get
            {
                return MacroDefs;
            }
        }

        public override bool ValueIsInteger(ISExpression value)
        {
            return value is IntegerLiteral;
        }

        public override int ValueAsInteger(ISExpression value)
        {
            var valueAsNumber = value as INumber;

            if (valueAsNumber == null)
            {
                throw new ArgumentException("ValueAsInteger() : The value is not an INumber.");
            }

            return valueAsNumber.ToInteger();
        }

        public override ISExpression IntegerAsValue(int value)
        {
            return new IntegerLiteral(value);
        }

        public static void CreateStackTraceInNewEnvironmentFrame(EnvironmentFrame<ISExpression> oldEnvFrame, EnvironmentFrame<ISExpression> newEnvFrame,
            int line, int column)
        {
            var oldStackTrace = oldEnvFrame.Lookup(varStackTrace);
            var list1 = new SExpressionList(new IntegerLiteral(column), new NullSExpression());
            var list2 = new SExpressionList(new IntegerLiteral(line), list1);
            var newStackTrace = new SExpressionList(list2, oldStackTrace);

            newEnvFrame.Add(varStackTrace, newStackTrace); // Add(), not AddBubbleDown().
            //Console.WriteLine("CreateStackTraceInNewEnvironmentFrame(): Added (line, column) = ({0}, {1}).", line, column);
            //Console.WriteLine("newStackTrace = {0}", newStackTrace);
        }

        public override bool SetDebug(bool debug)
        {
            Debug = debug;
            return true;
        }
    }

    #endregion
}
