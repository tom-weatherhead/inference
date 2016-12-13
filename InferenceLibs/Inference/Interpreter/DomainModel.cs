using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

using ISExpression = Inference.Interpreter.LISP.ISExpression;
using IConvertibleToGraph = Inference.Interpreter.SASL.IConvertibleToGraph;

namespace Inference.Interpreter
{
    #region Interfaces

    public interface IGlobalInfoOps
    {
        void Clear();
        string LoadPreset(string presetName);
        void LoadPresets();
        bool SetScoping(bool dynamicScoping);
        bool SetDebug(bool debug);
    }

    public interface IGlobalInfo<T>
    {
        ITokenizer Tokenizer { get; }
        IParser Parser { get; }
        T FalseValue { get; }
        T TrueValue { get; }
        bool ValueIsFalse(T value);
        EnvironmentFrame<T> GlobalEnvironment { get; }
        Dictionary<Name, FunctionDefinition<T>> FunctionDefinitions { get; }
        Dictionary<Name, IMacroDefinition<T>> MacroDefinitions { get; }
        bool ValueIsInteger(T value);
        int ValueAsInteger(T value);
        T IntegerAsValue(int value);
        Random RandomNumberGenerator { get; }
        bool DynamicScoping { get; set; }
        bool Debug { get; set; }
    }

    public interface IExpression<T>
    {
        T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo);
    }

    public interface IMacroDefinition<T>
    {
        int ArgumentCount { get; }
        T InvokeMacro(List<IExpression<T>> unevaluatedArguments, EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo);
    }

    #endregion

    #region EvaluationException

    public class EvaluationException : ExceptionWithLineAndColumnNumbers
    {
        public EvaluationException(string message, int line, int column)
            //: base("Evaluation error: " + message, line, column)
            : base(message, line, column)
        {
        }
    }

    #endregion

    #region Name

    public class Name
    {
        public readonly string Value;
        public readonly int Line;
        public readonly int Column;

        public Name(string value, int line, int column)
        {

            if (string.IsNullOrEmpty(value))
            {
                throw new ArgumentNullException("value", "A Name cannot have a null or empty value");
            }

            Value = value;
            Line = line;
            Column = column;
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

            Name otherName = obj as Name;

            return otherName != null && Value == otherName.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    #endregion

    #region EnvironmentFrame<T>

    public class EnvironmentFrame<T>
    {
        public readonly Dictionary<Variable<T>, T> Dict;
        public readonly EnvironmentFrame<T> Next;

        public EnvironmentFrame(EnvironmentFrame<T> next)
        {
            Dict = new Dictionary<Variable<T>, T>();
            Next = next;
        }

        public bool IsDefined(Variable<T> key)
        {

            if (Dict.ContainsKey(key))
            {
                return true;
            }

            if (Next != null)
            {
                return Next.IsDefined(key);
            }

            return false;
        }

        public T Lookup(Variable<T> key)
        {

            if (Dict.ContainsKey(key))
            {
#if DEAD_CODE
                Console.WriteLine("Lookup: The value of {0} in {1} environment frame is {2}",
                    key, (Next != null) ? "a local" : "the global", Dict[key]);
#endif
                return Dict[key];
            }

            if (Next != null)
            {
                return Next.Lookup(key);
            }

            throw new KeyNotFoundException(string.Format("Environment.Lookup() : No value found for variable '{0}'.", key.Name));
        }

        public void Add(Variable<T> key, T value)
        {
            Dict[key] = value;
        }

        public void AddBubbleDown(Variable<T> key, T value)
        {

            if (!Dict.ContainsKey(key) && Next != null)
            {
                Next.AddBubbleDown(key, value);     // Bubble down towards the global environment.
            }
            else
            {
                // Bug fix: Before 2013/12/04, the "else" above was absent, and the code below was executed unconditionally.
#if DEAD_CODE
                Console.WriteLine("AddBubbleDown: The new value of {0} in {1} environment frame is {2}",
                    key, (Next != null) ? "a local" : "the global", value);
#endif
                Add(key, value);
            }
        }

        public void Compose(List<Variable<T>> keys, List<T> values)
        {

            if (keys.Count != values.Count)
            {
                throw new ArgumentException("Environment.Compose() : The keys list and the values list have different lengths.");
            }

            for (var i = 0; i < keys.Count; ++i)
            {
                Add(keys[i], values[i]);
            }
        }
    }

    #endregion

    #region Variable<T>

    public class Variable<T> : IExpression<T>, IConvertibleToGraph
    {
        public readonly string Name;
        public readonly int Line;
        public readonly int Column;

        public Variable(string name, int line, int column)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A Variable cannot have a null or empty name");
            }

            /*
            //Console.WriteLine("Creating a Variable named '{0}'.", name);

            if (name.StartsWith("Inference"))
            {
                throw new Exception(string.Format("Error: Creating variable named '{0}'.", name));
            }
             */

            Name = name;
            Line = line;
            Column = column;
        }

        public override string ToString()
        {
            return Name;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            Variable<T> otherVariable = obj as Variable<T>;

            return otherVariable != null && Name == otherVariable.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {

            try
            {
                return localEnvironment.Lookup(this);
            }
            catch (KeyNotFoundException)
            {
                throw new EvaluationException(
                    string.Format("Variable<T>.Evaluate() : No value found for variable '{0}'", Name),
                    Line, Column);
            }
        }

        public IExpression<ISExpression> ConvertToGraph()
        {

            if (this is IExpression<ISExpression>)
            {
                return (IExpression<ISExpression>)this;
            }

            throw new NotImplementedException("Variable<T>.ConvertToGraph() : T is not ISExpression.");
        }
    }

    #endregion

    #region VariableList<T>

    public class VariableList<T>
    {
        public readonly List<Variable<T>> Value;

        public VariableList()
        {
            Value = new List<Variable<T>>();
        }

        public override string ToString()
        {
            return "(" + string.Join(" ", Value) + ")";
        }
    }

    #endregion

    #region FunctionDefinition<T>

    public class FunctionDefinition<T> : IExpression<T>
    {
        public readonly Name FunctionName;
        public readonly VariableList<T> ArgList;
        public readonly IExpression<T> Body;

        public FunctionDefinition(Name functionName, VariableList<T> argList, IExpression<T> body)
        {
            FunctionName = functionName;
            ArgList = argList;
            Body = body;
        }

        public override string ToString()
        {
            return string.Format("(define {0} {1} {2})", FunctionName, ArgList, Body);
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
            globalInfo.FunctionDefinitions[FunctionName] = this;
            return globalInfo.TrueValue;
        }
    }

    #endregion

    #region IfUsage<T>

    public class IfUsage<T> : IExpression<T>
    {
        public readonly IExpression<T> Condition;
        public readonly IExpression<T> IfBody;
        public readonly IExpression<T> ElseBody;

        public IfUsage(IExpression<T> condition, IExpression<T> ifBody, IExpression<T> elseBody)
        {
            Condition = condition;
            IfBody = ifBody;
            ElseBody = elseBody;
        }

        public override string ToString()
        {
            return string.Format("(if {0} {1} {2})", Condition, IfBody, ElseBody);
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
            T conditionValue = Condition.Evaluate(localEnvironment, globalInfo);

            //if (!conditionValue.Equals(globalInfo.FalseValue))
            if (!globalInfo.ValueIsFalse(conditionValue))
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

    #region WhileUsage<T>

    public class WhileUsage<T> : IExpression<T>
    {
        public readonly IExpression<T> Condition;
        public readonly IExpression<T> Body;

        public WhileUsage(IExpression<T> condition, IExpression<T> body)
        {
            Condition = condition;
            Body = body;
        }

        public override string ToString()
        {
            return string.Format("(while {0} {1})", Condition, Body);
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
#if DEAD_CODE
            T conditionValue;
            T falseValue = globalInfo.FalseValue;

            for (; ; )
            {
                conditionValue = Condition.Evaluate(localEnvironment, globalInfo);
    
                if (conditionValue.Equals(falseValue))
                {
                    break;
                }

                Body.Evaluate(localEnvironment, globalInfo);
            }

            return conditionValue;
#else

            while (!globalInfo.ValueIsFalse(Condition.Evaluate(localEnvironment, globalInfo)))
            {
                Body.Evaluate(localEnvironment, globalInfo);
            }

            return globalInfo.FalseValue;
#endif
        }
    }

    #endregion

    #region SetUsage<T>

    public class SetUsage<T> : IExpression<T>
    {
        public readonly Variable<T> VariableName;
        public readonly IExpression<T> Expression;

        public SetUsage(Variable<T> variableName, IExpression<T> expression)
        {
            VariableName = variableName;
            Expression = expression;
        }

        public override string ToString()
        {
            return string.Format("(set {0} {1})", VariableName, Expression);
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
            T expressionValue = Expression.Evaluate(localEnvironment, globalInfo);

            // If the variable is not already defined in a local env, we may have to assign it to the global env.
            localEnvironment.AddBubbleDown(VariableName, expressionValue);
            return expressionValue;
        }
    }

    #endregion

    #region ExpressionList<T>

    public class ExpressionList<T>
    {
        public readonly List<IExpression<T>> Value;

        public ExpressionList()
        {
            Value = new List<IExpression<T>>();
        }

        public override string ToString()
        {
            return string.Join(" ", Value);
        }
    }

    #endregion

    #region BeginUsage<T>

    public class BeginUsage<T> : IExpression<T>
    {
        public readonly IExpression<T> FirstExpression;
        public readonly ExpressionList<T> ExpressionList;

        public BeginUsage(IExpression<T> firstExpression, ExpressionList<T> expressionList)
        {
            FirstExpression = firstExpression;
            ExpressionList = expressionList;
        }

        public override string ToString()
        {
            return string.Format("(begin {0} {1})", FirstExpression, ExpressionList);
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
            T result = FirstExpression.Evaluate(localEnvironment, globalInfo);

            foreach (var expression in ExpressionList.Value)
            {
                result = expression.Evaluate(localEnvironment, globalInfo);
            }

            return result;
        }
    }

    #endregion

    #region CondUsage<T>

    public class CondUsage<T> : IExpression<T>
    {
        public readonly List<KeyValuePair<IExpression<T>, IExpression<T>>> ExprPairList;

        public CondUsage(List<KeyValuePair<IExpression<T>, IExpression<T>>> exprPairList)
        {
            ExprPairList = exprPairList;
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
            //var falseValue = globalInfo.FalseValue;

            foreach (var exprPair in ExprPairList)
            {

                //if (!exprPair.Key.Evaluate(localEnvironment, globalInfo).Equals(falseValue))
                if (!globalInfo.ValueIsFalse(exprPair.Key.Evaluate(localEnvironment, globalInfo)))
                {
                    return exprPair.Value.Evaluate(localEnvironment, globalInfo);
                }
            }

            //return falseValue;
            return globalInfo.FalseValue;
        }
    }

    #endregion

    #region LetUsage<T>

    public class LetUsage<T> : IExpression<T>
    {
        public readonly List<KeyValuePair<Variable<T>, IExpression<T>>> Bindings;
        public readonly IExpression<T> Expression;

        public LetUsage(List<KeyValuePair<Variable<T>, IExpression<T>>> bindings, IExpression<T> expression)
        {
            Bindings = bindings;
            Expression = expression;
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
            var newEnvFrame = new EnvironmentFrame<T>(localEnvironment);

            foreach (var binding in Bindings)
            {
                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(localEnvironment, globalInfo));
            }

            return Expression.Evaluate(newEnvFrame, globalInfo);
        }
    }

    #endregion

    #region LetStarUsage<T>

    public class LetStarUsage<T> : IExpression<T>
    {
        public readonly List<KeyValuePair<Variable<T>, IExpression<T>>> Bindings;
        public readonly IExpression<T> Expression;

        public LetStarUsage(List<KeyValuePair<Variable<T>, IExpression<T>>> bindings, IExpression<T> expression)
        {
            Bindings = bindings;
            Expression = expression;
        }

        public T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
#if DEAD_CODE
            var newEnvFrame = new EnvironmentFrame<T>(localEnvironment);

            foreach (var binding in Bindings)
            {
                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, globalInfo));
            }

            return Expression.Evaluate(newEnvFrame, globalInfo);
#else
            // 2014/02/17 : This implementation does not support recursive definitions.
            var lastEnv = localEnvironment;

            foreach (var binding in Bindings)
            {
                var newEnvFrame = new EnvironmentFrame<T>(lastEnv);

                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(lastEnv, globalInfo));
                lastEnv = newEnvFrame;
            }

            return Expression.Evaluate(lastEnv, globalInfo);
#endif
        }
    }

    #endregion

    #region OperatorUsage<T>

    public class OperatorUsage<T> : IExpression<T>
    {
        public readonly Name OperatorName;
        public readonly ExpressionList<T> ExpressionList;

        public OperatorUsage(Name operatorName, ExpressionList<T> expressionList)
        {
            OperatorName = operatorName;
            ExpressionList = expressionList;
        }

        public override string ToString()
        {

            if (ExpressionList.Value.Count == 0)
            {
                return string.Format("({0})", OperatorName);
            }

            return string.Format("({0} {1})", OperatorName, ExpressionList);
        }

        protected virtual bool TryGetExpectedNumArgs(IGlobalInfo<T> globalInfo, out int result)
        {

            if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value) ||
                IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
            {
                result = 2;
                return true;
            }

            switch (OperatorName.Value)
            {
                case "print":
                    result = 1;
                    return true;

                case "=":
                    result = 2;
                    return true;

                default:

                    if (globalInfo.FunctionDefinitions.ContainsKey(OperatorName))
                    {
                        result = globalInfo.FunctionDefinitions[OperatorName].ArgList.Value.Count;
                        return true;
                    }
                    else if (globalInfo.MacroDefinitions != null && globalInfo.MacroDefinitions.ContainsKey(OperatorName))
                    {
                        result = globalInfo.MacroDefinitions[OperatorName].ArgumentCount;
                        return true;
                    }
                    else
                    {
                        result = 0;
                        return false;
                    }
            }
        }

        protected virtual string CheckArgTypes(List<T> evaluatedArguments)
        {
            return null;
        }

        protected virtual bool TryInvokeMacro(
            List<IExpression<T>> unevaluatedArguments,
            EnvironmentFrame<T> localEnvironment,
            IGlobalInfo<T> globalInfo,
            out T macroResult)
        {
            macroResult = default(T);
            return false;
        }

        protected virtual void UpdateStackTrace(EnvironmentFrame<T> oldEnvFrame, EnvironmentFrame<T> newEnvFrame,
            int line, int column)
        {
        }

        protected virtual T EvaluateAux(List<T> evaluatedArguments, EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
            int firstArgAsInt = (evaluatedArguments.Count > 0 && globalInfo.ValueIsInteger(evaluatedArguments[0]))
                ? globalInfo.ValueAsInteger(evaluatedArguments[0]) : 0;
            int secondArgAsInt = (evaluatedArguments.Count > 1 && globalInfo.ValueIsInteger(evaluatedArguments[1]))
                ? globalInfo.ValueAsInteger(evaluatedArguments[1]) : 0;

            if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
            {
                return globalInfo.IntegerAsValue(IntegerOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsInt, secondArgAsInt));
            }
            else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
            {
                return IntegerOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
            }

            switch (OperatorName.Value)
            {
                case "=":
                    return evaluatedArguments[0].Equals(evaluatedArguments[1]) ? globalInfo.TrueValue : globalInfo.FalseValue;

                case "print":
                    Console.WriteLine(evaluatedArguments[0]);
                    return evaluatedArguments[0];

                default:

                    if (globalInfo.FunctionDefinitions.ContainsKey(OperatorName))
                    {
                        // Evaluate a user-defined function.
                        var newEnvironment = new EnvironmentFrame<T>(globalInfo.DynamicScoping ? localEnvironment : globalInfo.GlobalEnvironment);

                        if (globalInfo.Debug)
                        {
                            UpdateStackTrace(localEnvironment, newEnvironment, OperatorName.Line, OperatorName.Column);
                        }

                        newEnvironment.Compose(globalInfo.FunctionDefinitions[OperatorName].ArgList.Value, evaluatedArguments);
                        return globalInfo.FunctionDefinitions[OperatorName].Body.Evaluate(newEnvironment, globalInfo);
                    }

                    throw new EvaluationException(
                        string.Format("EvaluateAux() : Unknown operator name '{0}'", OperatorName),
                        OperatorName.Line, OperatorName.Column);
            }
        }

        // This is virtual because Scheme.PrimOp overrides it.

        public virtual T Evaluate(EnvironmentFrame<T> localEnvironment, IGlobalInfo<T> globalInfo)
        {
            var actualNumArgs = ExpressionList.Value.Count;
            int expectedNumArgs;

            if (!TryGetExpectedNumArgs(globalInfo, out expectedNumArgs))
            {
                throw new EvaluationException(
                    string.Format("OperatorUsage : Unknown operator name '{0}'", OperatorName.Value),
                    OperatorName.Line, OperatorName.Column);
            }
            else if (expectedNumArgs >= 0 && actualNumArgs != expectedNumArgs)
            {
                throw new EvaluationException(
                    string.Format("OperatorUsage : Expected {0} argument(s) for operator '{1}', instead of the actual {2} argument(s)",
                        expectedNumArgs, OperatorName.Value, actualNumArgs),
                    OperatorName.Line, OperatorName.Column);
            }

            T macroResult;

            if (TryInvokeMacro(ExpressionList.Value, localEnvironment, globalInfo, out macroResult))
            {
                return macroResult;
            }

            var evaluatedArguments = ExpressionList.Value.Select(expr => expr.Evaluate(localEnvironment, globalInfo)).ToList();
            var argTypesErrorMessage = CheckArgTypes(evaluatedArguments);

            if (!string.IsNullOrEmpty(argTypesErrorMessage))
            {
                throw new EvaluationException(
                    string.Format("Operator '{0}': {1}", OperatorName.Value, argTypesErrorMessage),
                    OperatorName.Line, OperatorName.Column);
            }

            return EvaluateAux(evaluatedArguments, localEnvironment, globalInfo);
        }
    }

    #endregion

    #region IntegerOperatorKeeper

    public static class IntegerOperatorKeeper
    {
        public static readonly Dictionary<string, Func<int, int, bool>> TwoArgumentPredicates = new Dictionary<string, Func<int, int, bool>>();
        public static readonly Dictionary<string, Func<int, int, int>> TwoArgumentOperators = new Dictionary<string, Func<int, int, int>>();

        static IntegerOperatorKeeper()
        {
            TwoArgumentPredicates["<"] = (x, y) => x < y;
            //TwoArgumentPredicates[">"] = (x, y) => x > y;

            TwoArgumentOperators["+"] = (x, y) => x + y;
            TwoArgumentOperators["-"] = (x, y) => x - y;
            TwoArgumentOperators["*"] = (x, y) => x * y;
            TwoArgumentOperators["/"] = (x, y) => x / y;
        }
    }

    #endregion

    #region DoubleOperatorKeeper

    public static class DoubleOperatorKeeper
    {
        public static readonly Dictionary<string, Func<double, double>> OneArgumentOperators = new Dictionary<string, Func<double, double>>();
        public static readonly Dictionary<string, Func<double, double, bool>> TwoArgumentPredicates = new Dictionary<string, Func<double, double, bool>>();
        public static readonly Dictionary<string, Func<double, double, double>> TwoArgumentOperators = new Dictionary<string, Func<double, double, double>>();

        static DoubleOperatorKeeper()
        {
            OneArgumentOperators["exp"] = x => Math.Exp(x);
            OneArgumentOperators["ln"] = x => Math.Log(x);
            OneArgumentOperators["sin"] = x => Math.Sin(x);
            OneArgumentOperators["cos"] = x => Math.Cos(x);
            OneArgumentOperators["tan"] = x => Math.Tan(x);

            TwoArgumentPredicates["<"] = (x, y) => x < y;
            //TwoArgumentPredicates[">"] = (x, y) => x > y;

            TwoArgumentOperators["+"] = (x, y) => x + y;
            TwoArgumentOperators["-"] = (x, y) => x - y;
            TwoArgumentOperators["*"] = (x, y) => x * y;
            TwoArgumentOperators["/"] = (x, y) => x / y;
            TwoArgumentOperators["pow"] = (x, y) => Math.Pow(x, y);
            TwoArgumentOperators["atan2"] = (y, x) => Math.Atan2(y, x); // Note the reversed order of the parameters.
        }
    }

    #endregion

    #region GlobalInfoBase<T>

    public abstract class GlobalInfoBase<T> : IGlobalInfo<T>, IGlobalInfoOps
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly EnvironmentFrame<T> GlobalEnv = new EnvironmentFrame<T>(null);
        private readonly Dictionary<Name, FunctionDefinition<T>> FunctionDefs = new Dictionary<Name, FunctionDefinition<T>>();
        private readonly Random RNG = new Random();
        public bool DynamicScoping { get; set; }
        public bool Debug { get; set; }

        protected GlobalInfoBase(ITokenizer t, IParser p)
        {
            tokenizer = t;
            parser = p;
            DynamicScoping = false;
            Debug = false;
        }

        public ITokenizer Tokenizer
        {
            get
            {
                return tokenizer;
            }
        }

        public IParser Parser
        {
            get
            {
                return parser;
            }
        }

        public void Clear()
        {
            GlobalEnv.Dict.Clear();
            FunctionDefs.Clear();

            if (MacroDefinitions != null)
            {
                MacroDefinitions.Clear();
            }

            SetScoping(false);  // Set the scope rules to "static" rather than "dynamic".
            SetDebug(false);    // Turn debug mode off.
        }

        protected void Evaluate(string input)
        {
            var expr = Parser.Parse(Tokenizer.Tokenize(input)) as IExpression<T>;

            if (expr == null)
            {
                throw new Exception(string.Format("GlobalInfoBase.Evaluate() : Parse failed; input is: {0}", input));
            }

            expr.Evaluate(GlobalEnvironment, this);
        }

        public virtual string LoadPreset(string presetName)
        {
            throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
        }

        public virtual void LoadPresets()
        {
        }

        public abstract T FalseValue { get; }
        public abstract T TrueValue { get; }

        public virtual bool ValueIsFalse(T value)
        {
            return value.Equals(FalseValue);
        }

        public EnvironmentFrame<T> GlobalEnvironment
        {
            get
            {
                return GlobalEnv;
            }
        }

        public virtual Dictionary<Name, FunctionDefinition<T>> FunctionDefinitions
        {
            get
            {
                return FunctionDefs;
            }
        }

        public virtual Dictionary<Name, IMacroDefinition<T>> MacroDefinitions
        {
            get
            {
                return null;
            }
        }

        public abstract bool ValueIsInteger(T value);
        public abstract int ValueAsInteger(T value);
        public abstract T IntegerAsValue(int value);

        public Random RandomNumberGenerator
        {
            get
            {
                return RNG;
            }
        }

        public virtual bool SetScoping(bool dynamicScoping)
        {
            DynamicScoping = dynamicScoping;
            return true;
        }

        public virtual bool SetDebug(bool debug)
        {
            return false;
        }
    }

    #endregion
}
