using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.CLU
{
    #region Interfaces

    public interface ICLUFunctionName
    {
    }

    public interface ICLUValue
    {
    }

    public interface ICLUExpression
    {
        ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo);
    }

    #endregion

    #region FunctionNotExportedException

    public class FunctionNotExportedException : Exception
    {
        public FunctionNotExportedException(string clusterName, string functionName)
            : base(string.Format("The cluster '{0}' does not export a function named '{1}'.", clusterName, functionName))
        {
        }
    }

    #endregion

    #region OnePartFunctionName

    public class OnePartFunctionName : ICLUFunctionName
    {
        public readonly string FunctionPart;

        public OnePartFunctionName(string f)
        {
            FunctionPart = f;
        }

        public override string ToString()
        {
            return FunctionPart;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            if (obj == null || !GetType().Equals(obj.GetType()))
            {
                return false;
            }

            var otherOnePartFunName = (OnePartFunctionName)obj;

            return FunctionPart == otherOnePartFunName.FunctionPart;
        }

        public override int GetHashCode()
        {
            return FunctionPart.GetHashCode();
        }
    }

    #endregion

    #region TwoPartFunctionName

    public class TwoPartFunctionName : OnePartFunctionName
    {
        public readonly string ClusterPart;

        public TwoPartFunctionName(string c, string f)
            : base(f)
        {
            ClusterPart = c;
        }

        public override string ToString()
        {
            return string.Format("{0}${1}", ClusterPart, FunctionPart);
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            if (obj == null || !GetType().Equals(obj.GetType()))
            {
                return false;
            }

            var otherTwoPartFunName = (TwoPartFunctionName)obj;

            return ClusterPart == otherTwoPartFunName.ClusterPart && FunctionPart == otherTwoPartFunName.FunctionPart;
        }

        public override int GetHashCode()
        {
            return ClusterPart.GetHashCode() * 101 + FunctionPart.GetHashCode();
        }
    }

    #endregion

    #region CLUPrimitiveValue

    // CLUPrimitiveValue objects are immutable.

    public class CLUPrimitiveValue : ICLUValue, ICLUExpression
    {
        public readonly int Value;

        public CLUPrimitiveValue(int value)
        {
            Value = value;
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

            var otherPrim = obj as CLUPrimitiveValue;

            return otherPrim != null && Value == otherPrim.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            return this;
        }
    }

    #endregion

    #region CLUUserValue

    public class CLUUserValue : ICLUValue, ICLUExpression
    {
        public readonly Cluster Owner;
        public readonly CLUEnvironmentFrame Value;

        public CLUUserValue(Cluster owner, CLUEnvironmentFrame environmentFrame)
        {
            Owner = owner;
            Value = environmentFrame;
        }

        public override string ToString()
        {
            // TODO: Output the values of the cluster instance variables, in the order in which they are declared in the cluster.
            //return "CLUUserValue: " + Value.ToString();
            return string.Join("\r\n", Owner.ClRep.Select(v => Value.Lookup(v)));
        }

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            return this;
        }
    }

    #endregion

    #region CLUEnvironmentFrame

    public class CLUEnvironmentFrame
    {
        public readonly Dictionary<CLUVariable, ICLUValue> Dict = new Dictionary<CLUVariable,ICLUValue>();
        public readonly CLUEnvironmentFrame Next;

        public CLUEnvironmentFrame(CLUEnvironmentFrame next)
        {
            Next = next;
        }

        public override string ToString()
        {
            return string.Format("({0})", string.Join("; ", Dict.Keys.Select(key => string.Format("{0} = {1}", key, Dict[key]))));
        }

        public ICLUValue Lookup(CLUVariable key)
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

            throw new KeyNotFoundException(string.Format("CLUEnvironmentFrame.Lookup() : No value found for variable '{0}'.", key.Name));
        }

        public void Add(CLUVariable key, ICLUValue value)
        {
            Dict[key] = value;
        }

        public void AddBubbleDown(CLUVariable key, ICLUValue value)
        {

            if (!Dict.ContainsKey(key) && Next != null)
            {
                Next.AddBubbleDown(key, value);     // Bubble down towards the global environment.
            }
            else
            {
                // Bug fix: Before 2013/12/05, the "else" above was absent, and the code below was executed unconditionally.
#if DEAD_CODE
                Console.WriteLine("AddBubbleDown: The new value of {0} in {1} environment frame is {2}",
                    key, (Next != null) ? "a local" : "the global", value);
#endif
                Add(key, value);
            }
        }

        public void Compose(List<CLUVariable> keys, List<ICLUValue> values)
        {

            if (keys.Count != values.Count)
            {
                throw new ArgumentException("CLUEnvironmentFrame.Compose() : The keys list and the values list have different lengths.");
            }

            for (var i = 0; i < keys.Count; ++i)
            {
                Add(keys[i], values[i]);
            }
        }
    }

    #endregion

    #region CLUVariable

    public class CLUVariable : ICLUExpression
    {
        public readonly string Name;

        public CLUVariable(string name)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A CLUVariable cannot have a null or empty name");
            }

            Name = name;
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

            CLUVariable otherVariable = obj as CLUVariable;

            return otherVariable != null && Name == otherVariable.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            return localEnvironment.Lookup(this);
        }
    }

    #endregion

    #region CLUIfUsage

    public class CLUIfUsage : ICLUExpression
    {
        public readonly ICLUExpression Condition;
        public readonly ICLUExpression IfBody;
        public readonly ICLUExpression ElseBody;

        public CLUIfUsage(ICLUExpression condition, ICLUExpression ifBody, ICLUExpression elseBody)
        {
            Condition = condition;
            IfBody = ifBody;
            ElseBody = elseBody;
        }

        /*
        public override string ToString()
        {
            return string.Format("(if {0} {1} {2})", Condition, IfBody, ElseBody);
        }
         */

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            ICLUValue conditionValue = Condition.Evaluate(localEnvironment, cluster, globalInfo);

            //if (!conditionValue.Equals(globalInfo.FalseValue))
            if (!globalInfo.ValueIsFalse(conditionValue))
            {
                return IfBody.Evaluate(localEnvironment, cluster, globalInfo);
            }
            else
            {
                return ElseBody.Evaluate(localEnvironment, cluster, globalInfo);
            }
        }
    }

    #endregion

    #region CLUWhileUsage

    public class CLUWhileUsage : ICLUExpression
    {
        public readonly ICLUExpression Condition;
        public readonly ICLUExpression Body;

        public CLUWhileUsage(ICLUExpression condition, ICLUExpression body)
        {
            Condition = condition;
            Body = body;
        }

        /*
        public override string ToString()
        {
            return string.Format("(while {0} {1})", Condition, Body);
        }
         */

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
#if DEAD_CODE
            ICLUValue conditionValue;
            //ICLUValue falseValue = globalInfo.FalseValue;

            for (; ; )
            {
                conditionValue = Condition.Evaluate(localEnvironment, cluster, globalInfo);

                //if (conditionValue.Equals(falseValue))
                if (globalInfo.ValueIsFalse(conditionValue))
                {
                    break;
                }

                Body.Evaluate(localEnvironment, cluster, globalInfo);
            }

            return conditionValue;
#else

            while (!globalInfo.ValueIsFalse(Condition.Evaluate(localEnvironment, cluster, globalInfo)))
            {
                Body.Evaluate(localEnvironment, cluster, globalInfo);
            }

            return globalInfo.FalseValue;
#endif
        }
    }

    #endregion

    #region CLUSetUsage

    public class CLUSetUsage : ICLUExpression
    {
        public readonly CLUVariable VariableName;
        public readonly ICLUExpression Expression;

        public CLUSetUsage(CLUVariable variableName, ICLUExpression expression)
        {
            VariableName = variableName;
            Expression = expression;
        }

        /*
        public override string ToString()
        {
            return string.Format("(set {0} {1})", VariableName, Expression);
        }
         */

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            ICLUValue expressionValue = Expression.Evaluate(localEnvironment, cluster, globalInfo);

            // If the variable is not already defined in the local env, we may have to assign it to the global env (assuming that there are only two envs).
            localEnvironment.AddBubbleDown(VariableName, expressionValue);    // TODO: Warning: This may be too simple and very wrong.
            return expressionValue;
        }
    }

    #endregion

    #region CLUBeginUsage

    public class CLUBeginUsage : ICLUExpression
    {
        public readonly ICLUExpression FirstExpression;
        public readonly List<ICLUExpression> ExpressionList;

        public CLUBeginUsage(ICLUExpression firstExpression, List<ICLUExpression> expressionList)
        {
            FirstExpression = firstExpression;
            ExpressionList = expressionList;
        }

        /*
        public override string ToString()
        {
            return string.Format("(begin {0} {1})", FirstExpression, ExpressionList);
        }
         */

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            ICLUValue result = FirstExpression.Evaluate(localEnvironment, cluster, globalInfo);

            foreach (var expression in ExpressionList)
            {
                result = expression.Evaluate(localEnvironment, cluster, globalInfo);
            }

            return result;
        }
    }

    #endregion

    #region CLUCondUsage

    public class CLUCondUsage : ICLUExpression
    {
        public readonly List<KeyValuePair<ICLUExpression, ICLUExpression>> ExprPairList;

        public CLUCondUsage(List<KeyValuePair<ICLUExpression, ICLUExpression>> exprPairList)
        {
            ExprPairList = exprPairList;
        }

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            //var falseValue = globalInfo.FalseValue;

            foreach (var exprPair in ExprPairList)
            {

                //if (!exprPair.Key.Evaluate(localEnvironment, cluster, globalInfo).Equals(falseValue))
                if (!globalInfo.ValueIsFalse(exprPair.Key.Evaluate(localEnvironment, cluster, globalInfo)))
                {
                    return exprPair.Value.Evaluate(localEnvironment, cluster, globalInfo);
                }
            }

            //return falseValue;
            return globalInfo.FalseValue;
        }
    }

    #endregion

    #region CLULetUsage

    public class CLULetUsage : ICLUExpression
    {
        public readonly List<KeyValuePair<CLUVariable, ICLUExpression>> Bindings;
        public readonly ICLUExpression Expression;

        public CLULetUsage(List<KeyValuePair<CLUVariable, ICLUExpression>> bindings, ICLUExpression expression)
        {
            Bindings = bindings;
            Expression = expression;
        }

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            var newEnvFrame = new CLUEnvironmentFrame(localEnvironment);

            foreach (var binding in Bindings)
            {
                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(localEnvironment, cluster, globalInfo));
            }

            return Expression.Evaluate(newEnvFrame, cluster, globalInfo);
        }
    }

    #endregion

    #region CLULetStarUsage

    public class CLULetStarUsage : ICLUExpression
    {
        public readonly List<KeyValuePair<CLUVariable, ICLUExpression>> Bindings;
        public readonly ICLUExpression Expression;

        public CLULetStarUsage(List<KeyValuePair<CLUVariable, ICLUExpression>> bindings, ICLUExpression expression)
        {
            Bindings = bindings;
            Expression = expression;
        }

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
#if DEAD_CODE
            var newEnvFrame = new CLUEnvironmentFrame(localEnvironment);

            foreach (var binding in Bindings)
            {
                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, cluster, globalInfo));
            }

            return Expression.Evaluate(newEnvFrame, cluster, globalInfo);
#else
            // 2014/02/17 : This implementation does not support recursive definitions.
            var lastEnv = localEnvironment;

            foreach (var binding in Bindings)
            {
                var newEnvFrame = new CLUEnvironmentFrame(lastEnv);

                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(lastEnv, cluster, globalInfo));
                lastEnv = newEnvFrame;
            }

            return Expression.Evaluate(lastEnv, cluster, globalInfo);
#endif
        }
    }

    #endregion

    #region CLUOperatorUsage

    public class CLUOperatorUsage : ICLUExpression
    {
        public readonly List<ICLUExpression> ExpressionList;
        private readonly string ClusterName;
        private readonly string FunctionName;
        private readonly HashSet<string> BuiltInOperatorNames;

        public CLUOperatorUsage(ICLUFunctionName operatorName, List<ICLUExpression> expressionList)
        {
            ExpressionList = expressionList;

            if (operatorName is TwoPartFunctionName)
            {
                var o = (TwoPartFunctionName)operatorName;

                ClusterName = o.ClusterPart;
                FunctionName = o.FunctionPart;
            }
            else
            {
                var o = (OnePartFunctionName)operatorName;

                ClusterName = string.Empty;
                FunctionName = o.FunctionPart;
            }

            BuiltInOperatorNames = new HashSet<string>() { "+", "-", "*", "/", "=", "<", /* ">", */ "print" };
        }

        /*
        public override string ToString()
        {
            return string.Format("({0} {1})", OperatorName, ExpressionList);
        }
         */

        protected bool TryGetExpectedNumArgs(CLUFunctionDefinitionBase funDef, Cluster cluster, CLUGlobalInfo globalInfo, out int result)
        {

            if (funDef == null)
            {

                switch (FunctionName)
                {
                    case "print":
                        result = 1;
                        return true;

                    case "+":
                    case "-":
                    case "*":
                    case "/":
                    case "=":
                    case "<":
                    //case ">":
                        result = 2;
                        return true;

                    default:
                        throw new Exception(string.Format("TryGetExpectedNumArgs() : Unknown built-in operator '{0}'.", FunctionName));
                }
            }

            if (funDef is CLUNormalFunctionDefinition)
            {
                var normalFunDef = (CLUNormalFunctionDefinition)funDef;

                result = normalFunDef.ArgList.Count;
                return true;
            }
            else if (funDef is CLUConstructorDefinition)
            {
                result = cluster.ClRep.Count;
                return true;
            }
            else if (funDef is CLUSelectorDefinition)
            {
                result = 1;
                return true;
            }
            else if (funDef is CLUSettorDefinition)
            {
                result = 2;
                return true;
            }
            else
            {
                throw new Exception(string.Format("TryGetExpectedNumArgs() : Unknown operator type '{0}'.", funDef.GetType().FullName));
            }
        }

        protected void CheckArgTypes(CLUFunctionDefinitionBase funDef, Cluster cluster, List<ICLUValue> evaluatedArguments)
        {

            if (funDef != null)
            {
                CLUVariable associatedVariable = null;

                if (funDef is CLUSelectorDefinition)
                {
                    var selector = (CLUSelectorDefinition)funDef;

                    associatedVariable = selector.AssociatedVariable;
                }
                else if (funDef is CLUSettorDefinition)
                {
                    var settor = (CLUSettorDefinition)funDef;

                    associatedVariable = settor.AssociatedVariable;
                }

                if (associatedVariable != null)
                {
                    var userValue = evaluatedArguments[0] as CLUUserValue;

                    if (userValue == null)
                    {
                        throw new Exception("Selector/settor arg type check: First arg is not a CLUUserValue.");
                    }
                    else if (!userValue.Value.Dict.ContainsKey(associatedVariable))
                    {
                        throw new Exception(string.Format("Selector/settor arg type check: First arg does not contain the member var {0}.", associatedVariable));
                    }
                }

                return;
            }

            if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(FunctionName) ||
                IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(FunctionName))
            {

                if (!(evaluatedArguments[0] is CLUPrimitiveValue))
                {
                    throw new ArgumentException(string.Format("Operator {0} : First argument is not a number.", FunctionName));
                }

                if (!(evaluatedArguments[1] is CLUPrimitiveValue))
                {
                    throw new ArgumentException(string.Format("Operator {0} : Second argument is not a number.", FunctionName));
                }
            }
        }

        protected ICLUValue EvaluateNormal(CLUNormalFunctionDefinition funDef, List<ICLUValue> evaluatedArguments, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            int firstArgAsInt = (evaluatedArguments.Count > 0 && globalInfo.ValueIsInteger(evaluatedArguments[0]))
                ? globalInfo.ValueAsInteger(evaluatedArguments[0]) : 0;
            int secondArgAsInt = (evaluatedArguments.Count > 1 && globalInfo.ValueIsInteger(evaluatedArguments[1]))
                ? globalInfo.ValueAsInteger(evaluatedArguments[1]) : 0;

            if (cluster == null)
            {

                if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(FunctionName))
                {
                    return globalInfo.IntegerAsValue(IntegerOperatorKeeper.TwoArgumentOperators[FunctionName](firstArgAsInt, secondArgAsInt));
                }
                else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(FunctionName))
                {
                    return IntegerOperatorKeeper.TwoArgumentPredicates[FunctionName](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
                }

                switch (FunctionName)
                {
                    case "=":
                        return evaluatedArguments[0].Equals(evaluatedArguments[1]) ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "print":
                        Console.WriteLine(evaluatedArguments[0]);
                        return evaluatedArguments[0];

                    default:
                        break;
                }
            }

            // Evaluate a user-defined function.
            var newEnvironment = new CLUEnvironmentFrame(globalInfo.GlobalEnvironment);

            newEnvironment.Compose(funDef.ArgList, evaluatedArguments);
            return funDef.Body.Evaluate(newEnvironment, cluster, globalInfo);
        }

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            Cluster originalCluster = cluster;
            CLUFunctionDefinitionBase funDef = null;

            if (!string.IsNullOrEmpty(ClusterName))
            {

                if (!globalInfo.ClusterDict.ContainsKey(ClusterName))
                {
                    throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Unknown cluster '{0}'.", ClusterName));
                }

                cluster = globalInfo.ClusterDict[ClusterName];

                if (!cluster.ExportedDict.ContainsKey(FunctionName))
                {
                    //throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Cluster '{0}' does not contain an exported function named '{1}'.", ClusterName, FunctionName));
                    throw new FunctionNotExportedException(ClusterName, FunctionName);
                }

                funDef = cluster.ExportedDict[FunctionName];
            }
            else if (cluster == null)
            {

                if (BuiltInOperatorNames.Contains(FunctionName))
                {
                    funDef = null;
                }
                else
                {

                    if (!globalInfo.FunctionDefinitions.ContainsKey(FunctionName))
                    {
                        throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Unknown global function '{0}'.", FunctionName));
                    }

                    funDef = globalInfo.FunctionDefinitions[FunctionName];
                }
            }
            else if (cluster.ExportedDict.ContainsKey(FunctionName))
            {
                funDef = cluster.ExportedDict[FunctionName];
            }
            else if (cluster.NonExportedDict.ContainsKey(FunctionName))
            {
                funDef = cluster.NonExportedDict[FunctionName];
            }
            else
            {
                cluster = null;

                if (BuiltInOperatorNames.Contains(FunctionName))
                {
                    funDef = null;
                }
                else
                {

                    if (!globalInfo.FunctionDefinitions.ContainsKey(FunctionName))
                    {
                        throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Unknown global function '{0}'.", FunctionName));
                    }

                    funDef = globalInfo.FunctionDefinitions[FunctionName];
                }
            }

            // At this point, funDef == null means that it's a built-in operator.

            var actualNumArgs = ExpressionList.Count;
            int expectedNumArgs;

            if (!TryGetExpectedNumArgs(funDef, cluster, globalInfo, out expectedNumArgs))
            {
                throw new Exception(string.Format("CLUOperatorUsage : Unknown operator name '{0}'.", FunctionName));
            }
            else if (actualNumArgs != expectedNumArgs)
            {
                throw new Exception(string.Format("CLUOperatorUsage : Expected {0} arguments for operator '{1}', instead of the actual {2} arguments.",
                    expectedNumArgs, FunctionName, actualNumArgs));
            }

            // originalCluster
            //List<ICLUValue> evaluatedArguments = ExpressionList.Select(expr => expr.Evaluate(localEnvironment, cluster, globalInfo)).ToList();
            List<ICLUValue> evaluatedArguments = ExpressionList.Select(expr => expr.Evaluate(localEnvironment, originalCluster, globalInfo)).ToList();

            CheckArgTypes(funDef, cluster, evaluatedArguments);

            if (funDef == null || funDef is CLUNormalFunctionDefinition)
            {
                return EvaluateNormal((CLUNormalFunctionDefinition)funDef, evaluatedArguments, cluster, globalInfo);
            }
            else if (funDef is CLUConstructorDefinition)
            {
                var newEnvironment = new CLUEnvironmentFrame(globalInfo.GlobalEnvironment);

                newEnvironment.Compose(cluster.ClRep, evaluatedArguments);
                return new CLUUserValue(cluster, newEnvironment);
            }
            else if (funDef is CLUSelectorDefinition)
            {
                var instance = (CLUUserValue)evaluatedArguments[0];

                return funDef.Evaluate(instance.Value, cluster, globalInfo);
            }
            else if (funDef is CLUSettorDefinition)
            {
                var settor = (CLUSettorDefinition)funDef;
                var instance = (CLUUserValue)evaluatedArguments[0];

                settor.SetValue = evaluatedArguments[1];
                return funDef.Evaluate(instance.Value, cluster, globalInfo);
            }
            else
            {
                throw new Exception("CLUOperatorUsage : Failed to evaluate; unrecognized type of funDef.");
            }
        }
    }

    #endregion

    #region CLUFunctionDefinitionBase

    public abstract class CLUFunctionDefinitionBase : ICLUExpression
    {
        public readonly string FunctionName;

        protected CLUFunctionDefinitionBase(string funcName)
        {
            FunctionName = funcName;
        }

        public abstract ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo);
    }

    #endregion

    #region CLUNormalFunctionDefinition

    public class CLUNormalFunctionDefinition : CLUFunctionDefinitionBase
    {
        public readonly List<CLUVariable> ArgList;
        public readonly ICLUExpression Body;

        public CLUNormalFunctionDefinition(string functionName, List<CLUVariable> argList, ICLUExpression body)
            : base(functionName)
        {
            ArgList = argList;
            Body = body;
        }

        /*
        public override string ToString()
        {
            return string.Format("(define {0} {1} {2})", FunctionName, ArgList, Body);
        }
         */

        public override ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            globalInfo.FunctionDefinitions[FunctionName] = this;
            return globalInfo.TrueValue;
        }
    }

    #endregion

    #region CLUConstructorDefinition

    public class CLUConstructorDefinition : CLUFunctionDefinitionBase
    {
        //public readonly string ClusterName;

        public CLUConstructorDefinition( /* string funcName, */ string clusterName)
            : base(clusterName)
        {
            //ClusterName = clusterName;
        }

        public override ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            /*
            // ClusterName is the name of the cluster of which newInstance is an instance.

            if (!globalInfo.ClusterDict.ContainsKey(ClusterName))
            {
                throw new Exception(string.Format("CLUConstructorDefinition.Evaluate() : Unknown cluster '{0}'.", FunctionName));
            }

            var newInstance = new CLUUserValue(localEnvironment);
            var zero = new CLUPrimitiveValue(0);    // This can be shared because it is immutable.

            foreach (var variable in globalInfo.ClusterDict[ClusterName].ClRep)
            {
                newInstance.Value.Dict[variable] = zero;
            }

            return newInstance;
             */
            return new CLUUserValue(cluster, localEnvironment);
        }
    }

    #endregion

    #region CLUSelectorDefinition

    public class CLUSelectorDefinition : CLUFunctionDefinitionBase
    {
        public readonly CLUVariable AssociatedVariable;

        public CLUSelectorDefinition(string funcName, CLUVariable associatedVariable)
            : base(funcName)
        {
            AssociatedVariable = associatedVariable;
        }

        public override ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            return localEnvironment.Dict[AssociatedVariable];
        }
    }

    #endregion

    #region CLUSettorDefinition

    public class CLUSettorDefinition : CLUFunctionDefinitionBase
    {
        //public readonly string SelName;
        public readonly CLUVariable AssociatedVariable;
        public ICLUValue SetValue;  // This must be set before Evaluate() is called.

        public CLUSettorDefinition(string funcName, /* string selName, */ CLUVariable associatedVariable)
            : base(funcName)
        {
            //SelName = selName;
            AssociatedVariable = associatedVariable;
        }

        public override ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            localEnvironment.Dict[AssociatedVariable] = SetValue;
            return SetValue;
        }
    }

    #endregion

    #region Cluster

    public class Cluster : ICLUExpression
    {
        public readonly string ClusterName;
        public readonly HashSet<string> ExportSet;
        public readonly List<CLUVariable> ClRep;
        private List<CLUFunctionDefinitionBase> FunDefList;
        public readonly Dictionary<string, CLUFunctionDefinitionBase> ExportedDict = new Dictionary<string, CLUFunctionDefinitionBase>();
        public readonly Dictionary<string, CLUFunctionDefinitionBase> NonExportedDict = new Dictionary<string, CLUFunctionDefinitionBase>();

        public Cluster(string name, HashSet<string> exportSet, List<CLUVariable> clRep, List<CLUFunctionDefinitionBase> funDefList)
        {
            ClusterName = name;
            ExportSet = exportSet;
            ClRep = clRep;
            FunDefList = funDefList;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherCluster = obj as Cluster;

            return otherCluster != null && ClusterName == otherCluster.ClusterName;
        }

        public override int GetHashCode()
        {
            return ClusterName.GetHashCode();
        }

        public ICLUValue Evaluate(CLUEnvironmentFrame localEnvironment, Cluster cluster, CLUGlobalInfo globalInfo)
        {
            globalInfo.ClusterDict[ClusterName] = this;

            // Make the constructor:
            NonExportedDict[ClusterName] = new CLUConstructorDefinition(ClusterName);

            // Make the selectors and settors:

            foreach (var memberVariable in ClRep)
            {
                NonExportedDict[memberVariable.Name] = new CLUSelectorDefinition(memberVariable.Name, memberVariable);
                NonExportedDict["set-" + memberVariable.Name] =
                    new CLUSettorDefinition("set-" + memberVariable.Name, memberVariable);
            }

            foreach (var funDef in FunDefList)
            {

                if (ExportSet.Contains(funDef.FunctionName))
                {
                    ExportedDict[funDef.FunctionName] = funDef;
                }
                else
                {
                    NonExportedDict[funDef.FunctionName] = funDef;
                }
            }

            return globalInfo.TrueValue;
        }
    }

    #endregion

    #region CLUGlobalInfo

    public class CLUGlobalInfo : IGlobalInfoOps
    {
        private readonly ITokenizer Tokenizer;
        private readonly IParser Parser;
        private readonly ICLUValue FalseVal = new CLUPrimitiveValue(0);
        private readonly ICLUValue TrueVal = new CLUPrimitiveValue(1);
        public readonly CLUEnvironmentFrame GlobalEnvironment = new CLUEnvironmentFrame(null);
        public readonly Dictionary<string, CLUNormalFunctionDefinition> FunctionDefinitions = new Dictionary<string, CLUNormalFunctionDefinition>();
        public readonly Dictionary<string, Cluster> ClusterDict = new Dictionary<string, Cluster>();

        public CLUGlobalInfo(ITokenizer t, IParser p)
        {
            Tokenizer = t;
            Parser = p;
        }

        public void Clear()
        {
            GlobalEnvironment.Dict.Clear();
            FunctionDefinitions.Clear();
            ClusterDict.Clear();
        }

        public string LoadPreset(string presetName)
        {
            throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
        }

        private void Evaluate(string input)
        {
            var expr = Parser.Parse(Tokenizer.Tokenize(input)) as ICLUExpression;

            if (expr == null)
            {
                throw new Exception(string.Format("CLUGlobalInfo.Evaluate() : Parse failed; input is: {0}", input));
            }

            expr.Evaluate(GlobalEnvironment, null, this);
        }

        public void LoadPresets()
        {
            Evaluate("(define > (x y) (< y x))");
        }

        public ICLUValue FalseValue
        {
            get
            {
                return FalseVal;
            }
        }

        public ICLUValue TrueValue
        {
            get
            {
                return TrueVal;
            }
        }

        public bool ValueIsFalse(ICLUValue value)
        {
            return value.Equals(FalseValue);
        }

        public bool ValueIsInteger(ICLUValue value)
        {
            return value is CLUPrimitiveValue;
        }

        public int ValueAsInteger(ICLUValue value)
        {

            if (!ValueIsInteger(value))
            {
                throw new ArgumentException("ValueAsInteger() : value is not an integer");
            }

            var prim = (CLUPrimitiveValue)value;

            return prim.Value;
        }

        public ICLUValue IntegerAsValue(int value)
        {
            return new CLUPrimitiveValue(value);
        }

        public bool SetScoping(bool dynamicScoping)
        {
            return false;
        }

        public bool SetDebug(bool debug)
        {
            return false;
        }
    }

    #endregion
}
