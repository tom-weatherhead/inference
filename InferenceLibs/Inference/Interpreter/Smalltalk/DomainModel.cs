#define USE_BLOCKS

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.Smalltalk
{
    #region Interfaces

    public interface ISmalltalkValue
    {
        SmalltalkClass Owner { get; }
        string GetTypename();
        bool IsNumber();
        bool IsSymbol();
        bool IsCharacter();
        bool IsString();
        bool IsObject();
        bool IsArray();
    }

    public interface ISmalltalkNumber
    {
        int ToInteger();
        double ToDouble();
    }

    public interface ISmalltalkExpression
    {
        //"SmalltalkClass c" was added here as the third parameter in order to support "super"; see Exercise 11 on pages 347-348.
        ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo);
    }

    #endregion

    #region SmalltalkException

    public class SmalltalkException : EvaluationException
    {
        public SmalltalkException(string message, int line, int column)
            : base(message, line, column)
        {
        }
    }

    #endregion

    #region SmalltalkValueBase

    public abstract class SmalltalkValueBase : ISmalltalkValue, ISmalltalkExpression
    {
        public SmalltalkClass Owner { get; private set; }

        protected SmalltalkValueBase(SmalltalkClass owner)
        {
            Owner = owner;
        }

        public abstract string GetTypename();

        public virtual bool IsNumber()
        {
            return false;
        }

        public virtual bool IsSymbol()
        {
            return false;
        }

        public virtual bool IsCharacter()
        {
            return false;
        }

        public virtual bool IsString()
        {
            return false;
        }

        public virtual bool IsObject()
        {
            return false;
        }

        public virtual bool IsArray()
        {
            return false;
        }

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            return this;
        }
    }

    #endregion

    #region SmalltalkNumberBase

    public abstract class SmalltalkNumberBase : SmalltalkValueBase, ISmalltalkNumber
    {
        protected SmalltalkNumberBase(SmalltalkClass owner)
            : base(owner)
        {
        }

        public override bool IsNumber()
        {
            return true;
        }

        public abstract int ToInteger();
        public abstract double ToDouble();
    }

    #endregion

    #region SmalltalkIntegerValue

    // SmalltalkIntegerValue objects are immutable.

    public class SmalltalkIntegerValue : SmalltalkNumberBase
    {
        public readonly int Value;

        public SmalltalkIntegerValue(int value)
            : base(SmalltalkObjectClassKeeper.ObjectClass)
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

            var otherIntVal = obj as SmalltalkIntegerValue;

            return otherIntVal != null && Value == otherIntVal.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public override string GetTypename()
        {
            return "int";
        }

        public override int ToInteger()
        {
            return Value;
        }

        public override double ToDouble()
        {
            return Convert.ToDouble(Value);
        }

        /*
        public override ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            return this;
        }
         */
    }

    #endregion

    #region SmalltalkFloatValue

    // SmalltalkFloatValue objects are immutable.

    public class SmalltalkFloatValue : SmalltalkNumberBase
    {
        public readonly double Value;

        public SmalltalkFloatValue(double value)
            : base(SmalltalkObjectClassKeeper.ObjectClass)
        {
            /*
            if (!(value is double))
            {
                throw new ArgumentException("FloatLiteral constructor: value is not a double.", "value");
            }

            Value = (double)value;
             */
            Value = value;
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

            SmalltalkFloatValue otherFltVal = obj as SmalltalkFloatValue;

            return otherFltVal != null && Value == otherFltVal.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public override string GetTypename()
        {
            return "float";
        }

        public override int ToInteger()
        {
            return Convert.ToInt32(Math.Floor(Value));
        }

        public override double ToDouble()
        {
            return Value;
        }

        /*
        public override ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            return this;
        }
         */
    }

    #endregion

    #region SmalltalkSymbolValue

    // SmalltalkSymbolValue objects are immutable.

    public class SmalltalkSymbolValue : SmalltalkValueBase
    {
        public readonly string Value;

        public SmalltalkSymbolValue(string value)
            : base(SmalltalkObjectClassKeeper.ObjectClass)
        {

            if (string.IsNullOrEmpty(value))
            {
                throw new ArgumentException("SmalltalkSymbolValue constructor: value is null or empty.", "value");
            }

            Value = value;
        }

        public override string ToString()
        {
            //return "#" + Value;
            return Value;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherSymbol = obj as SmalltalkSymbolValue;

            return otherSymbol != null && Value == otherSymbol.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public override string GetTypename()
        {
            return "symbol";
        }

        public override bool IsSymbol()
        {
            return true;
        }
    }

    #endregion

    #region SmalltalkCharacterValue

    // SmalltalkCharacterValue objects are immutable.

    public class SmalltalkCharacterValue : SmalltalkValueBase
    {
        public readonly char Value;

        public SmalltalkCharacterValue(char value)
            : base(SmalltalkObjectClassKeeper.ObjectClass)
        {

            if (value == '\0')
            {
                throw new ArgumentException("SmalltalkCharacterValue constructor: value is the null character.", "value");
            }

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

            SmalltalkCharacterValue otherCharVal = obj as SmalltalkCharacterValue;

            return otherCharVal != null && Value == otherCharVal.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public override string GetTypename()
        {
            return "char";
        }

        public override bool IsCharacter()
        {
            return true;
        }
    }

    #endregion

    #region SmalltalkStringValue

    // SmalltalkStringValue objects are immutable.

    public class SmalltalkStringValue : SmalltalkValueBase // TODO: Is this class identical to SmalltalkSymbolValue?
    {
        public readonly string Value;

        public SmalltalkStringValue(string value)
            : base(SmalltalkObjectClassKeeper.ObjectClass)
        {

            if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
            {
                throw new ArgumentException("SmalltalkStringValue constructor: value is null.", "value");
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

            SmalltalkStringValue otherStringVal = obj as SmalltalkStringValue;

            return otherStringVal != null && Value == otherStringVal.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public override string GetTypename()
        {
            return "string";
        }

        public override bool IsString()
        {
            return true;
        }

        public ISmalltalkValue Index(ISmalltalkValue idx)
        {
            var i = ((ISmalltalkNumber)idx).ToInteger();

            if (i <= 0 || i > Value.Length)
            {
                throw new Exception(string.Format("SmalltalkStringValue.Index(): Index {0} is not in the range from 1 to {1}",
                    i, Value.Length));
            }

            return new SmalltalkCharacterValue(Value[i - 1]);
        }
    }

    #endregion

    #region SmalltalkArrayValue

    // SmalltalkArrayValue objects are mutable.

    public class SmalltalkArrayValue : SmalltalkValueBase
    {
        public readonly ISmalltalkValue[] Value;

        public SmalltalkArrayValue(int size)
            : base(SmalltalkObjectClassKeeper.ObjectClass)
        {

            if (size < 0)
            {
                throw new ArgumentException("SmalltalkStringValue constructor: size < 0", "size");
            }

            Value = new ISmalltalkValue[size];

            var zero = new SmalltalkIntegerValue(0);

            for (var i = 0; i < size; ++i)
            {
                Value[i] = zero;
            }
        }

        public override string ToString()
        {
            return string.Join(" ", (IEnumerable<ISmalltalkValue>)Value);
        }

        public override bool Equals(object obj)
        {
            return object.ReferenceEquals(this, obj);
        }

        public override int GetHashCode()
        {
            return 0;
        }

        public override string GetTypename()
        {
            return "array";
        }

        public override bool IsArray()
        {
            return true;
        }

        public ISmalltalkValue GetElement(int i) // Indexing starts at 1
        {

            if (i <= 0 || i > Value.Length)
            {
                throw new ArgumentException(
                    string.Format("SmalltalkArrayValue.GetElement() : i is not in the range from 1 to {0}", Value.Length),
                    "i");
            }

            return Value[i - 1];
        }

        public ISmalltalkValue SetElement(int i, ISmalltalkValue elementValue) // Indexing starts at 1
        {

            if (i <= 0 || i > Value.Length)
            {
                throw new ArgumentException(
                    string.Format("SmalltalkArrayValue.GetElement() : i is not in the range from 1 to {0}", Value.Length),
                    "i");
            }

            Value[i - 1] = elementValue;
            return elementValue;
        }
    }

    #endregion

    #region SmalltalkUserValue

    public class SmalltalkUserValue : SmalltalkValueBase
    {
        public readonly SmalltalkEnvironmentFrame Value;

        public SmalltalkUserValue(SmalltalkClass owner, SmalltalkEnvironmentFrame environmentFrame)
            : base(owner)
        {
            Value = environmentFrame;
        }

        public override bool Equals(object obj)
        {
#if DEAD_CODE
            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherUserValue = obj as SmalltalkUserValue;

            return otherUserValue != null && Owner.Equals(otherUserValue.Owner) && Value.Equals(otherUserValue.Value);
#else
            // 2014/01/30: The book (at the top of page 278) recommends that this always return false.
            return object.ReferenceEquals(this, obj);
#endif
        }

        public override int GetHashCode()
        {
#if DEAD_CODE
            return Owner.GetHashCode() * 101 + Value.GetHashCode();
#else
            // 2014/01/30
            return 0;
#endif
        }

        public override string ToString()
        {
            // Avoid looking up the value of "self", as that would cause an infinite loop.
            return string.Join("\r\n", Owner.ClRep.Where(v => !v.Equals(SmalltalkObjectClassKeeper.SelfVar)).Select(v => Value.Lookup(v)));
        }

        public override string GetTypename()
        {
            return Owner.ClassName;
        }

        public override bool IsObject()
        {
            return true;
        }
    }

    #endregion

    #region SmalltalkBlock

    // A SmalltalkBlock is a lot like an SASL Thunk (a suspended computation).

    // SmalltalkBlock objects are immutable.

    public class SmalltalkBlock : SmalltalkValueBase
    {
        public readonly ISmalltalkExpression Expression;
        public readonly SmalltalkEnvironmentFrame LocalEnvironment;
        public readonly ISmalltalkValue Receiver;
        public readonly SmalltalkClass Class;
        public readonly SmalltalkGlobalInfo GlobalInfo;

        public SmalltalkBlock(ISmalltalkExpression expression, SmalltalkEnvironmentFrame localEnvironment,
            ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
            : base(SmalltalkObjectClassKeeper.ObjectClass)
        {
            Expression = expression;
            LocalEnvironment = localEnvironment;
            Receiver = receiver;
            Class = c;
            GlobalInfo = globalInfo;
        }

        public override string ToString()
        {
            return "<block>";
        }

        public override bool Equals(object obj)
        {
            return object.ReferenceEquals(this, obj);
        }

        public override int GetHashCode()
        {
            return 0;
        }

        public override string GetTypename()
        {
            return "block";
        }

        public ISmalltalkValue Unblock()
        {
            ISmalltalkValue result = this;

            for (; ;)
            {
                var block = result as SmalltalkBlock;

                if (block == null)
                {
                    break;
                }

                result = block.Expression.Evaluate(block.LocalEnvironment, block.Receiver, block.Class, block.GlobalInfo);
            }

            return result;
        }
    }

    #endregion

    #region SmalltalkEnvironmentFrame

    public class SmalltalkEnvironmentFrame
    {
        public readonly Dictionary<SmalltalkVariable, ISmalltalkValue> Dict = new Dictionary<SmalltalkVariable, ISmalltalkValue>();
        public readonly SmalltalkEnvironmentFrame Next;

        public SmalltalkEnvironmentFrame(SmalltalkEnvironmentFrame next)
        {
            Next = next;
        }

        private HashSet<SmalltalkVariable> GetAllVariables()
        {
            var result = new HashSet<SmalltalkVariable>(Dict.Keys.Where(key => !key.Equals(SmalltalkObjectClassKeeper.SelfVar)));

            if (Next != null)
            {
                result.UnionWith(Next.GetAllVariables());
            }

            return result;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherEnvFrame = obj as SmalltalkEnvironmentFrame;

            if (otherEnvFrame == null)
            {
                return false;
            }

            var thisVars = GetAllVariables();
            var otherVars = otherEnvFrame.GetAllVariables();

            if (!thisVars.IsSubsetOf(otherVars) || !otherVars.IsSubsetOf(thisVars))
            {
                return false;
            }

            return thisVars.All(v => Lookup(v).Equals(otherEnvFrame.Lookup(v)));
        }

        public override int GetHashCode()
        {
            return GetAllVariables()
                .Select(v => Lookup(v).GetHashCode())
                .Aggregate(0, (accumulator, hashCode) => accumulator + hashCode);
        }

        private string ToStringLocalFrame()
        {
            return string.Format("({0})", string.Join("; ",
                Dict.Keys.Select(key => string.Format("{0} = {1}", key, key.Equals(SmalltalkObjectClassKeeper.SelfVar) ? "<self>" : Dict[key].ToString()))));
        }

        public override string ToString()
        {
            var result = ToStringLocalFrame();

            if (Next != null)
            {
                result = result + "; " + Next.ToString();
            }

            return result;
        }

        public bool IsDefined(SmalltalkVariable key)
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

        public ISmalltalkValue Lookup(SmalltalkVariable key)
        {

            if (Dict.ContainsKey(key))
            {
                return Dict[key];
            }

            if (Next != null)
            {
                return Next.Lookup(key);
            }

            throw new KeyNotFoundException(string.Format("SmalltalkEnvironmentFrame.Lookup() : No value found for variable '{0}'.", key.Name));
        }

        public void Add(SmalltalkVariable key, ISmalltalkValue value)
        {
            Dict[key] = value;
        }

        public void AddBubbleDown(SmalltalkVariable key, ISmalltalkValue value)
        {

            if (!Dict.ContainsKey(key) && Next != null)
            {
                Next.AddBubbleDown(key, value);     // Bubble down towards the global environment.
            }
            else
            {
                Add(key, value);
            }
        }

        public void Compose(List<SmalltalkVariable> keys, List<ISmalltalkValue> values)
        {

            if (keys.Count != values.Count)
            {
                throw new ArgumentException("SmalltalkEnvironmentFrame.Compose() : The keys list and the values list have different lengths.");
            }

            for (var i = 0; i < keys.Count; ++i)
            {
                Add(keys[i], values[i]);
            }
        }
    }

    #endregion

    #region SmalltalkVariable

    public class SmalltalkVariable : ISmalltalkExpression
    {
        public readonly string Name;

        public SmalltalkVariable(string name)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A SmalltalkVariable cannot have a null or empty name");
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

            SmalltalkVariable otherVariable = obj as SmalltalkVariable;

            return otherVariable != null && Name == otherVariable.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            // See Kamin page 295.
            var userVal = receiver as SmalltalkUserValue;

            if (localEnvironment != null && localEnvironment.IsDefined(this))
            {
                // TODO: Stop this Lookup (and IsDefined above) from searching the GlobalEnvironment.
                // Probably the best way to do this is to avoid linking any environment frames with the GlobalEnvironment.
                // 2013/12/05 : I believe this is a non-issue, since the top-level Evaluate passes in a null localEnvironment.
                return localEnvironment.Lookup(this);
            }
            else if (userVal != null && userVal.Value.Dict.ContainsKey(this))
            {
                return userVal.Value.Dict[this];
            }
            else
            {
                // 2013/12/05 : Question: In what situations does c differ from userVal.Owner?
                // - At the top level Evaluate(), where c is null, and userVal.Owner is ObjectClass (because userVal is ObjectInstance)
                // ? Perhaps in class hierarchies, e.g. where class D inherits from C which inherits from B; one can imagine an instance where
                //   userVal.Owner is class D, and c is class C (so that "super" refers to class B).
                //   - I.e. if userVal is an object of type D, but we are in a function in class C.

                if (c != null)  // TODO: Should we use userVal.Owner instead of c?  2013/12/05 : I think not; the current implementation matches the code in SmalltalkSetUsage.Evaluate().
                {
                    var value = c.FindClassVariableValue(this);

                    if (value != null)
                    {
                        return value;
                    }
                }

                return globalInfo.GlobalEnvironment.Lookup(this);
            }
        }
    }

    #endregion

#if DEAD_CODE
    #region SmalltalkIfUsage

    public class SmalltalkIfUsage : ISmalltalkExpression
    {
        public readonly ISmalltalkExpression Condition;
        public readonly ISmalltalkExpression IfBody;
        public readonly ISmalltalkExpression ElseBody;

        public SmalltalkIfUsage(ISmalltalkExpression condition, ISmalltalkExpression ifBody, ISmalltalkExpression elseBody)
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

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            ISmalltalkValue conditionValue = SmalltalkGlobalInfo.UnblockValue(Condition.Evaluate(localEnvironment, receiver, c, globalInfo));

            //if (!conditionValue.Equals(globalInfo.FalseValue))
            if (!globalInfo.ValueIsFalse(conditionValue))
            {
                return IfBody.Evaluate(localEnvironment, receiver, c, globalInfo);
            }
            else
            {
                return ElseBody.Evaluate(localEnvironment, receiver, c, globalInfo);
            }
        }
    }

    #endregion
#endif

    #region SmalltalkWhileUsage

    public class SmalltalkWhileUsage : ISmalltalkExpression
    {
        public readonly ISmalltalkExpression Condition;
        public readonly ISmalltalkExpression Body;

        public SmalltalkWhileUsage(ISmalltalkExpression condition, ISmalltalkExpression body)
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

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
#if DEAD_CODE
            ISmalltalkValue conditionValue;
            ISmalltalkValue falseValue = globalInfo.FalseValue;

            for (; ; )
            {
                conditionValue = Condition.Evaluate(localEnvironment, receiver, c, globalInfo);

                if (conditionValue.Equals(falseValue))
                {
                    break;
                }

                Body.Evaluate(localEnvironment, receiver, c, globalInfo);
            }

            return conditionValue;
#else

            while (!globalInfo.ValueIsFalse(SmalltalkGlobalInfo.UnblockValue(Condition.Evaluate(localEnvironment, receiver, c, globalInfo))))
            {
                Body.Evaluate(localEnvironment, receiver, c, globalInfo);
            }

            return globalInfo.ZeroValue;
#endif
        }
    }

    #endregion

    #region SmalltalkSetUsage

    public class SmalltalkSetUsage : ISmalltalkExpression
    {
        public readonly SmalltalkVariable VariableName;
        public readonly ISmalltalkExpression Expression;

        public SmalltalkSetUsage(SmalltalkVariable variableName, ISmalltalkExpression expression)
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

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            var expressionValue = SmalltalkGlobalInfo.UnblockValue(Expression.Evaluate(localEnvironment, receiver, c, globalInfo));
            var userVal = receiver as SmalltalkUserValue;

            if (localEnvironment != null && localEnvironment.IsDefined(VariableName))
            {
                localEnvironment.AddBubbleDown(VariableName, expressionValue);
            }
            else if (userVal != null && userVal.Value.Dict.ContainsKey(VariableName))
            {
                userVal.Value.Dict[VariableName] = expressionValue;
            }
            else if (c == null || !c.TrySetClassVariableValue(VariableName, expressionValue))
            {
                globalInfo.GlobalEnvironment.Dict[VariableName] = expressionValue;
            }

            return expressionValue;
        }
    }

    #endregion

    #region SmalltalkBeginUsage

    public class SmalltalkBeginUsage : ISmalltalkExpression
    {
        public readonly ISmalltalkExpression FirstExpression;
        public readonly List<ISmalltalkExpression> ExpressionList;

        public SmalltalkBeginUsage(ISmalltalkExpression firstExpression, List<ISmalltalkExpression> expressionList)
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

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            ISmalltalkValue result = FirstExpression.Evaluate(localEnvironment, receiver, c, globalInfo);

            foreach (var expression in ExpressionList)
            {
                result = expression.Evaluate(localEnvironment, receiver, c, globalInfo);
            }

            return result;
        }
    }

    #endregion

    #region SmalltalkCondUsage

    public class SmalltalkCondUsage : ISmalltalkExpression
    {
        public readonly List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>> ExprPairList;

        public SmalltalkCondUsage(List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>> exprPairList)
        {
            ExprPairList = exprPairList;
        }

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            //var falseValue = globalInfo.FalseValue;

            foreach (var exprPair in ExprPairList)
            {

                //if (!exprPair.Key.Evaluate(localEnvironment, receiver, c, globalInfo).Equals(falseValue))
                if (!globalInfo.ValueIsFalse(SmalltalkGlobalInfo.UnblockValue(exprPair.Key.Evaluate(localEnvironment, receiver, c, globalInfo))))
                {
                    return exprPair.Value.Evaluate(localEnvironment, receiver, c, globalInfo);
                }
            }

            //return falseValue;
            return globalInfo.ZeroValue;
        }
    }

    #endregion

    #region SmalltalkLetUsage

    public class SmalltalkLetUsage : ISmalltalkExpression
    {
        public readonly List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> Bindings;
        public readonly ISmalltalkExpression Expression;

        public SmalltalkLetUsage(List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> bindings, ISmalltalkExpression expression)
        {
            Bindings = bindings;
            Expression = expression;
        }

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            var newEnvFrame = new SmalltalkEnvironmentFrame(localEnvironment);

            foreach (var binding in Bindings)
            {
                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(localEnvironment, receiver, c, globalInfo));
            }

            return Expression.Evaluate(newEnvFrame, receiver, c, globalInfo);
        }
    }

    #endregion

    #region SmalltalkLetStarUsage

    public class SmalltalkLetStarUsage : ISmalltalkExpression
    {
        public readonly List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> Bindings;
        public readonly ISmalltalkExpression Expression;

        public SmalltalkLetStarUsage(List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> bindings, ISmalltalkExpression expression)
        {
            Bindings = bindings;
            Expression = expression;
        }

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
#if DEAD_CODE
            var newEnvFrame = new SmalltalkEnvironmentFrame(localEnvironment);

            foreach (var binding in Bindings)
            {
                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, receiver, c, globalInfo));
            }

            return Expression.Evaluate(newEnvFrame, receiver, c, globalInfo);
#else
            // 2014/02/17 : This implementation does not support recursive definitions.
            var lastEnv = localEnvironment;

            foreach (var binding in Bindings)
            {
                var newEnvFrame = new SmalltalkEnvironmentFrame(lastEnv);

                newEnvFrame.Add(binding.Key, binding.Value.Evaluate(lastEnv, receiver, c, globalInfo));
                lastEnv = newEnvFrame;
            }

            return Expression.Evaluate(lastEnv, receiver, c, globalInfo);
#endif
        }
    }

    #endregion

    #region SmalltalkFunctionDefinition

    public class SmalltalkFunctionDefinition : ISmalltalkExpression
    {
        public readonly string FunctionName;
        public readonly List<SmalltalkVariable> ArgList;
        public readonly ISmalltalkExpression Body;

        public SmalltalkFunctionDefinition(string functionName, List<SmalltalkVariable> argList, ISmalltalkExpression body)
        {
            FunctionName = functionName;
            ArgList = argList;
            Body = body;
        }

        /*
        public override string ToString()
        {
            return string.Format("(define {0} {1} {2})", FunctionName, ArgList, Body);
        }
         */

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            globalInfo.FunctionDefinitions[FunctionName] = this;
            return globalInfo.ZeroValue;
        }
    }

    #endregion

    #region SmalltalkOperatorUsage

    public class SmalltalkOperatorUsage : ISmalltalkExpression
    {
        private readonly Name OperatorName;
        public readonly List<ISmalltalkExpression> ExpressionList;
        // The method reference cache, as described in Exercise 12 on page 348:
        private SmalltalkClass CachedClassReference;
        private SmalltalkFunctionDefinition CachedMethodReference;
        private static readonly HashSet<string> OperatorsThatTakeEitherIntOrFloatArgs = new HashSet<string>() { "<", /* ">", */ "+", "-", "*", "/" };
        private static readonly HashSet<string> ValueOpNames = new HashSet<string>() {
            "print", "=", "<", "exp", "ln", "sin", "cos", "tan", "+", "-", "*", "/", "pow", "atan2",
            "strcat", "number?", "symbol?", "char?", "string?", "object?", "array?", "random", "tostring", "stringtosymbol",
            "floor", "throw", "strlen", "typename", "hash", "newarray", "arraylength",
            "string<", "ref=", "arrayget", "stridx", "substr", "arrayset" };

        public SmalltalkOperatorUsage(Name operatorName, List<ISmalltalkExpression> expressionList)
        {
            OperatorName = operatorName;
            ExpressionList = expressionList;
            CachedClassReference = null;
            CachedMethodReference = null;
        }

        private ISmalltalkValue EvaluateNew(SmalltalkGlobalInfo globalInfo)
        {

            if (ExpressionList.Count == 0)
            {
                throw new EvaluationException("EvaluateNew() : There are no arguments.", OperatorName.Line, OperatorName.Column);
            }

            // ExpressionList[0] is a SmalltalkVariable; its name is the name of the class of which we will create an instance.
            var variable = ExpressionList[0] as SmalltalkVariable;

            if (variable == null)
            {
                throw new EvaluationException("EvaluateNew() : The first argument is not in the form of a variable.",
                    OperatorName.Line, OperatorName.Column);
            }

            var className = variable.Name;

            if (!globalInfo.ClassDict.ContainsKey(className))
            {
                throw new EvaluationException(string.Format("EvaluateNew() : Unknown class name '{0}'.", className),
                    OperatorName.Line, OperatorName.Column);
            }

            var smalltalkClass = globalInfo.ClassDict[className];
            var env = new SmalltalkEnvironmentFrame(null);

            foreach (var memberVariable in smalltalkClass.ClRep)
            {
                env.Dict[memberVariable] = globalInfo.ZeroValue;
            }

            var result = new SmalltalkUserValue(smalltalkClass, env);

            result.Value.Dict[SmalltalkObjectClassKeeper.SelfVar] = result;
            return result;
        }

        private ISmalltalkValue EvaluateAuxInt(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
        {
            var firstArgAsInt = ((ISmalltalkNumber)evaluatedArguments[0]).ToInteger();
            var secondArgAsInt = ((ISmalltalkNumber)evaluatedArguments[1]).ToInteger();

            if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
            {
                return new SmalltalkIntegerValue(IntegerOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsInt, secondArgAsInt));
            }
            else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
            {
                return IntegerOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
            }

            // Internal error:
            throw new Exception(string.Format("SmalltalkOperatorUsage.EvaluateAuxInt() : Invalid operator {0}", OperatorName.Value));
        }

        private ISmalltalkValue EvaluateAuxFloat(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
        {
            // Currently, EvaluateAuxFloat() is only called for two-argument functions.
            var firstArgAsDouble = ((ISmalltalkNumber)evaluatedArguments[0]).ToDouble();
            var secondArgAsDouble = ((ISmalltalkNumber)evaluatedArguments[1]).ToDouble();

            if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
            {
                return new SmalltalkFloatValue(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsDouble, secondArgAsDouble));
            }
            else if (DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
            {
                return DoubleOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsDouble, secondArgAsDouble) ? globalInfo.TrueValue : globalInfo.FalseValue;
            }

            // Internal error:
            throw new Exception(string.Format("SmalltalkOperatorUsage.EvaluateAuxFloat() : Invalid operator {0}", OperatorName.Value));
        }

        // TODO 2014/02/04: Split EvaluateGlobalFunction() into EvaluateValueOp() and a new, much smaller EvaluateGlobalFunction() (i.e. user-defined functions).

        private ISmalltalkValue EvaluateValueOp(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
        {

            for (var i = 0; i < evaluatedArguments.Count; ++i)
            {
                evaluatedArguments[i] = SmalltalkGlobalInfo.UnblockValue(evaluatedArguments[i]);
            }

            // First, verify the number of arguments.
            var expectedNumArgs = -1;
            var actualNumArgs = evaluatedArguments.Count;

            // Note: We only check DoubleOperatorKeeper here (and not IntegerOperatorKeeper) because
            // the integer operators are a subset of the double operators.

            if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
            {
                expectedNumArgs = 1;
            }
            else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value) ||
                DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
            {
                expectedNumArgs = 2;
            }
            else
            {

                switch (OperatorName.Value)
                {
                    case "strcat":
                        expectedNumArgs = -1;   // Any number of arguments will do.
                        break;

                    case "print":
                    case "number?":
                    case "symbol?":
                    case "char?":
                    case "string?":
                    case "object?":
                    case "array?":
                    case "random":
                    case "tostring":
                    case "stringtosymbol":
                    case "floor":
                    case "throw":
                    case "strlen":
                    case "typename":
                    case "hash":
                    case "newarray":
                    case "arraylength":
                        expectedNumArgs = 1;
                        break;

                    case "=":
                    case "string<":
                    case "ref=":
                    case "arrayget":
                    case "stridx":
                        expectedNumArgs = 2;
                        break;

                    case "substr":
                    case "arrayset":
                        expectedNumArgs = 3;
                        break;

                    default:
                        break; // Do not throw an exception; global user-defined functions pass through here.
                }
            }

            if (expectedNumArgs >= 0 && actualNumArgs != expectedNumArgs)
            {
                throw new EvaluationException(
                    string.Format("EvaluateValueOp: Expected {0} arguments for operator '{1}', instead of the actual {2} arguments.",
                    expectedNumArgs, OperatorName.Value, actualNumArgs), OperatorName.Line, OperatorName.Column);
            }

            // Next, check the types of the arguments.
            string exceptionMessage = null;

            switch (OperatorName.Value)
            {
                case "+":
                case "-":
                case "*":
                case "/":
                //case "<":
                //case ">":
                case "pow":
                case "atan2":

                    if (!evaluatedArguments[0].IsNumber())
                    {
                        exceptionMessage = "First argument is not a number";
                    }
                    else if (!evaluatedArguments[1].IsNumber())
                    {
                        exceptionMessage = "Second argument is not a number";
                    }

                    break;

                case "<":

                    if (!(evaluatedArguments[0].IsNumber() && evaluatedArguments[1].IsNumber()) &&
                        !(evaluatedArguments[0].IsSymbol() && evaluatedArguments[1].IsSymbol()) &&
                        !(evaluatedArguments[0].IsCharacter() && evaluatedArguments[1].IsCharacter()) &&
                        !(evaluatedArguments[0].IsString() && evaluatedArguments[1].IsString()))
                    {
                        exceptionMessage = "Arguments must be both numbers or both symbols or both characters or both strings";
                    }

                    break;

                case "random": // 2014/02/01: TODO: Should we insist that random's argument be an integer?
                case "exp":
                case "ln":
                case "sin":
                case "cos":
                case "tan":
                case "floor":

                    if (!evaluatedArguments[0].IsNumber())
                    {
                        exceptionMessage = "Argument is not a number";
                    }

                    break;

                case "stringtosymbol":

                    if (!evaluatedArguments[0].IsString())
                    {
                        exceptionMessage = "Argument is not a string";
                    }
                    else if (string.IsNullOrEmpty(((SmalltalkStringValue)evaluatedArguments[0]).Value))
                    {
                        exceptionMessage = "Argument is the empty string"; // We know that it's empty and not the null .NET reference.
                    }

                    break;

                case "throw":
                case "strlen":

                    if (!evaluatedArguments[0].IsString())
                    {
                        exceptionMessage = "Argument is not a string";
                    }

                    break;

                case "string<": // Deprecated; see <

                    if (!evaluatedArguments[0].IsString())
                    {
                        exceptionMessage = "First argument is not a string";
                    }
                    else if (!evaluatedArguments[1].IsString())
                    {
                        exceptionMessage = "Second argument is not a string";
                    }

                    break;

                case "substr":

                    if (!evaluatedArguments[0].IsString())
                    {
                        exceptionMessage = "First argument is not a string";
                    }
                    else if (!evaluatedArguments[1].IsNumber())
                    {
                        exceptionMessage = "Second argument is not a number";
                    }
                    else if (!evaluatedArguments[2].IsNumber())
                    {
                        exceptionMessage = "Third argument is not a number";
                    }

                    break;

                case "strcat":

                    for (var i = 0; i < evaluatedArguments.Count; ++i)
                    {

                        if (evaluatedArguments[i].IsObject())
                        {
                            exceptionMessage = string.Format("Argument {0} is an object of type '{1}'",
                                i + 1, evaluatedArguments[i].Owner.ClassName);
                            break;
                        }
                    }

                    break;

                case "newarray":

                    if (!(evaluatedArguments[0] is SmalltalkIntegerValue))
                    {
                        exceptionMessage = "Argument is not an integer";
                    }

                    break;

                case "arraylength":

                    if (!evaluatedArguments[0].IsArray())
                    {
                        exceptionMessage = "Argument is not an array";
                    }

                    break;

                case "arrayget":
                case "arrayset":

                    if (!evaluatedArguments[0].IsArray())
                    {
                        exceptionMessage = "First argument is not an array";
                    }
                    else if (!(evaluatedArguments[1] is SmalltalkIntegerValue))
                    {
                        exceptionMessage = "Second argument is not an integer";
                    }

                    break;

                case "stridx":

                    if (!evaluatedArguments[0].IsString())
                    {
                        exceptionMessage = "First argument is not a string";
                    }
                    else if (!evaluatedArguments[1].IsNumber())
                    {
                        exceptionMessage = "Second argument is not a number";
                    }

                    break;

                default:
                    break;
            }

            if (!string.IsNullOrEmpty(exceptionMessage))
            {
                throw new EvaluationException(
                    string.Format("EvaluateValueOp: Operator '{0}': {1}", OperatorName.Value, exceptionMessage),
                    OperatorName.Line, OperatorName.Column);
            }

            // Now evaluate.

            try
            {

                switch (OperatorName.Value)
                {
                    case "=":
                        return evaluatedArguments[0].Equals(evaluatedArguments[1]) ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "print":
                        Console.WriteLine(evaluatedArguments[0]);
                        return evaluatedArguments[0];

                    case "number?":
                        return evaluatedArguments[0].IsNumber() ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "symbol?":
                        return evaluatedArguments[0].IsSymbol() ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "char?":
                        return evaluatedArguments[0].IsCharacter() ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "string?":
                        return evaluatedArguments[0].IsString() ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "object?":
                        return evaluatedArguments[0].IsObject() ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "array?":
                        return evaluatedArguments[0].IsArray() ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "random":
                        return new SmalltalkIntegerValue(globalInfo.RandomNumberGenerator.Next(globalInfo.ValueAsInteger(evaluatedArguments[0])));

                    case "tostring":
                        return new SmalltalkStringValue(evaluatedArguments[0].ToString());

                    case "stringtosymbol":
                        var str2sym = (SmalltalkStringValue)evaluatedArguments[0];

                        return new SmalltalkSymbolValue(str2sym.Value);

                    case "floor":
                        return new SmalltalkIntegerValue(((ISmalltalkNumber)evaluatedArguments[0]).ToInteger());

                    case "throw":
                        throw new SmalltalkException(((SmalltalkStringValue)evaluatedArguments[0]).Value, OperatorName.Line, OperatorName.Column);

                    case "string<": // See page 54.  Deprecated; see <
                        return ((SmalltalkStringValue)evaluatedArguments[0]).Value.CompareTo(((SmalltalkStringValue)evaluatedArguments[1]).Value) < 0
                            ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "strlen":
                        var strForLen = (SmalltalkStringValue)evaluatedArguments[0];

                        return new SmalltalkIntegerValue(strForLen.Value.Length);

                    case "substr":
                        var strForSubstr = (SmalltalkStringValue)evaluatedArguments[0];
                        var startForSubstr = globalInfo.ValueAsInteger(evaluatedArguments[1]);
                        var lengthForSubstr = globalInfo.ValueAsInteger(evaluatedArguments[2]);

                        return new SmalltalkStringValue(strForSubstr.Value.Substring(startForSubstr, lengthForSubstr));

                    case "typename":
                        return new SmalltalkStringValue(evaluatedArguments[0].GetTypename());

                    case "hash":
                        // ThAW 2014/01/28 : For now, we will avoid calling GetHashCode() on Smalltalk objects.
                        // Of course, Smalltalk classes are free to implement their own hash functions.
                        // (Would a hash function stub in the Object class prevent this global "hash" implementation from being called?)
                        var hashResult = 0;

                        if (!evaluatedArguments[0].IsObject())
                        {
                            hashResult = evaluatedArguments[0].GetHashCode();
                        }

                        return new SmalltalkIntegerValue(hashResult);

                    case "ref=":
                        return object.ReferenceEquals(evaluatedArguments[0], evaluatedArguments[1]) ? globalInfo.TrueValue : globalInfo.FalseValue;

                    case "strcat": // TODO 2014/12/09 : Use string.Join() instead of a StringBuilder?
                        var sb = new StringBuilder();

                        foreach (var ea in evaluatedArguments)
                        {
                            sb.Append(ea.ToString());
                        }

                        return new SmalltalkStringValue(sb.ToString());

                    case "newarray":
                        return new SmalltalkArrayValue(((SmalltalkIntegerValue)evaluatedArguments[0]).Value);

                    case "arraylength":
                        return new SmalltalkIntegerValue(((SmalltalkArrayValue)evaluatedArguments[0]).Value.Length);

                    case "arrayget":
                        return ((SmalltalkArrayValue)evaluatedArguments[0]).GetElement(((SmalltalkIntegerValue)evaluatedArguments[1]).Value);

                    case "arrayset":
                        return ((SmalltalkArrayValue)evaluatedArguments[0]).SetElement(
                            ((SmalltalkIntegerValue)evaluatedArguments[1]).Value, evaluatedArguments[2]);

                    case "stridx":
                        return ((SmalltalkStringValue)evaluatedArguments[0]).Index(evaluatedArguments[1]);

                    default:

                        if (OperatorName.Value == "<")
                        {

                            if (evaluatedArguments[0].IsSymbol())
                            {
                                return ((SmalltalkSymbolValue)evaluatedArguments[0]).Value.CompareTo(((SmalltalkSymbolValue)evaluatedArguments[1]).Value) < 0
                                    ? globalInfo.TrueValue : globalInfo.FalseValue;
                            }
                            else if (evaluatedArguments[0].IsCharacter())
                            {
                                return ((SmalltalkCharacterValue)evaluatedArguments[0]).Value < ((SmalltalkCharacterValue)evaluatedArguments[1]).Value
                                    ? globalInfo.TrueValue : globalInfo.FalseValue;
                            }
                            else if (evaluatedArguments[0].IsString())
                            {
                                return ((SmalltalkStringValue)evaluatedArguments[0]).Value.CompareTo(((SmalltalkStringValue)evaluatedArguments[1]).Value) < 0
                                    ? globalInfo.TrueValue : globalInfo.FalseValue;
                            }
                        }

                        if (OperatorsThatTakeEitherIntOrFloatArgs.Contains(OperatorName.Value))
                        {

                            if (evaluatedArguments.Any(arg => arg is SmalltalkFloatValue))
                            {
                                return EvaluateAuxFloat(evaluatedArguments, globalInfo);
                            }
                            else
                            {
                                return EvaluateAuxInt(evaluatedArguments, globalInfo);
                            }
                        }
                        // The next two cases involve operators that must take arguments as doubles, not ints.
                        else if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
                        {
                            return new SmalltalkFloatValue(DoubleOperatorKeeper.OneArgumentOperators[OperatorName.Value](((ISmalltalkNumber)evaluatedArguments[0]).ToDouble()));
                        }
                        else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
                        {
                            return new SmalltalkFloatValue(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](
                                ((ISmalltalkNumber)evaluatedArguments[0]).ToDouble(),
                                ((ISmalltalkNumber)evaluatedArguments[1]).ToDouble()));
                        }

#if DEAD_CODE
                        // Evaluate a user-defined function.

                        if (!globalInfo.FunctionDefinitions.ContainsKey(OperatorName.Value))
                        {
                            throw new EvaluationException(
                                string.Format("EvaluateGlobalFunction: Unknown function name '{0}'", OperatorName.Value),
                                OperatorName.Line, OperatorName.Column);
                        }

                        var funDef = globalInfo.FunctionDefinitions[OperatorName.Value];
                        var newEnvironment = new SmalltalkEnvironmentFrame(null);

                        newEnvironment.Compose(funDef.ArgList, evaluatedArguments);
                        return funDef.Body.Evaluate(newEnvironment, globalInfo.ObjectInstance, null, globalInfo);
#else
                        throw new EvaluationException(
                            string.Format("EvaluateValueOp: Unknown value op '{0}'", OperatorName.Value),
                            OperatorName.Line, OperatorName.Column);
#endif
                }
            }
            catch (EvaluationException)
            {
                throw;
            }
            catch (Exception ex)
            {
                throw new EvaluationException(
                    string.Format("EvaluateValueOp: Operator '{0}': {1}", OperatorName.Value, ex.Message),
                    OperatorName.Line, OperatorName.Column);
            }
        }

        private ISmalltalkValue EvaluateGlobalFunction(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
        {

            if (ValueOpNames.Contains(OperatorName.Value))
            {
                return EvaluateValueOp(evaluatedArguments, globalInfo);
            }

            try
            {
                // Evaluate a user-defined function.

                if (!globalInfo.FunctionDefinitions.ContainsKey(OperatorName.Value))
                {
                    throw new EvaluationException(
                        string.Format("EvaluateGlobalFunction: Unknown function name '{0}'", OperatorName.Value),
                        OperatorName.Line, OperatorName.Column);
                }

                var funDef = globalInfo.FunctionDefinitions[OperatorName.Value];
                var newEnvironment = new SmalltalkEnvironmentFrame(null);

                newEnvironment.Compose(funDef.ArgList, evaluatedArguments);
                return funDef.Body.Evaluate(newEnvironment, globalInfo.ObjectInstance, null, globalInfo);
            }
            catch (EvaluationException)
            {
                throw;
            }
            catch (Exception ex)
            {
                throw new EvaluationException(
                    string.Format("EvaluateGlobalFunction: Operator '{0}': {1}", OperatorName.Value, ex.Message),
                    OperatorName.Line, OperatorName.Column);
            }
        }

        private ISmalltalkValue EvaluateMethod(SmalltalkFunctionDefinition method, List<ISmalltalkValue> evaluatedArguments, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {
            var newEnvironment = new SmalltalkEnvironmentFrame(null /* globalInfo.GlobalEnvironment */);

            newEnvironment.Compose(method.ArgList, evaluatedArguments);
            return method.Body.Evaluate(newEnvironment, receiver, c, globalInfo);
        }

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {

            if (OperatorName.Value == "new")
            {
                return EvaluateNew(globalInfo);
            }

            List<ISmalltalkValue> evaluatedArguments;
            SmalltalkFunctionDefinition method;
            SmalltalkClass classInWhichMethodWasFound;

            if (ExpressionList.Count > 0)
            {
                var variable = ExpressionList[0] as SmalltalkVariable;

                if (variable != null && variable.Name == "super")
                {

                    if (c == null)
                    {
                        throw new EvaluationException(
                            string.Format("{0}: super usage: c is null", OperatorName.Value),
                            OperatorName.Line, OperatorName.Column);
                    }

                    if (c.SuperClass == null)
                    {
                        throw new EvaluationException(
                            string.Format("{0}: super usage: c.SuperClass is null", OperatorName.Value),
                            OperatorName.Line, OperatorName.Column);
                    }

                    method = c.SuperClass.FindMethod(OperatorName.Value, out classInWhichMethodWasFound);

                    if (method == null)
                    {
                        throw new EvaluationException(
                            string.Format("super usage: Method '{0}' not found", OperatorName.Value),
                            OperatorName.Line, OperatorName.Column);
                    }

                    var selfValue = SmalltalkObjectClassKeeper.SelfVar.Evaluate(localEnvironment, receiver, c, globalInfo);

                    evaluatedArguments = ExpressionList.Skip(1).Select(expr => expr.Evaluate(localEnvironment, receiver, c, globalInfo)).ToList();
                    return EvaluateMethod(method, evaluatedArguments, selfValue, classInWhichMethodWasFound, globalInfo);
                }
            }

#if USE_BLOCKS
            // Create blocks (suspended computations) from the expressions.
            evaluatedArguments = ExpressionList.Select(expr => (ISmalltalkValue)new SmalltalkBlock(expr, localEnvironment, receiver, c, globalInfo)).ToList();
#else
            evaluatedArguments = ExpressionList.Select(expr => expr.Evaluate(localEnvironment, receiver, c, globalInfo)).ToList();
#endif

            ISmalltalkValue result;

            if (evaluatedArguments.Count == 0)
            {
                result = EvaluateGlobalFunction(evaluatedArguments, globalInfo);
            }
            else
            {
                evaluatedArguments[0] = SmalltalkGlobalInfo.UnblockValue(evaluatedArguments[0]);

                var newReceiverClass = evaluatedArguments[0].Owner;

                if (newReceiverClass == CachedClassReference)
                {
                    method = CachedMethodReference;
                }
                else
                {
                    method = newReceiverClass.FindMethod(OperatorName.Value, out classInWhichMethodWasFound);
                    newReceiverClass = classInWhichMethodWasFound;

                    CachedClassReference = newReceiverClass;
                    CachedMethodReference = method;
                }

                if (method != null)
                {
                    result = EvaluateMethod(method, evaluatedArguments.Skip(1).ToList(), evaluatedArguments[0], newReceiverClass, globalInfo);
                }
                else
                {
                    result = EvaluateGlobalFunction(evaluatedArguments, globalInfo);
                }
            }

            return SmalltalkGlobalInfo.UnblockValue(result);
        }
    }

    #endregion

    #region SmalltalkClass

    public class SmalltalkClass : ISmalltalkExpression
    {
        public readonly string ClassName;
        public readonly string SuperClassName;
        public SmalltalkClass SuperClass = null;
        public readonly List<SmalltalkVariable> ClassVariableList;
        public readonly List<SmalltalkVariable> ClRep;  // In other words, the list of instance variables.
        private List<SmalltalkFunctionDefinition> ExportedList;
        public readonly Dictionary<string, SmalltalkFunctionDefinition> ExportedDict = new Dictionary<string, SmalltalkFunctionDefinition>();
        public readonly SmalltalkEnvironmentFrame ClassVariableEnvFrame = new SmalltalkEnvironmentFrame(null);
        private static readonly HashSet<string> ReservedTypeNames = new HashSet<string>() { "int", "float", "symbol", "char", "string", "array" };
        private readonly int LineNumber;
        private readonly int ColumnNumber;

        public SmalltalkClass(Name className, string superClassName, List<SmalltalkVariable> classVariableList, List<SmalltalkVariable> clRep, List<SmalltalkFunctionDefinition> exportedList)
        {
            ClassName = className.Value;
            SuperClassName = superClassName;
            ClassVariableList = classVariableList;
            ClRep = clRep;
            ExportedList = exportedList;
            LineNumber = className.Line;
            ColumnNumber = className.Column;
        }

        public SmalltalkClass(string className, string superClassName, List<SmalltalkVariable> classVariableList, List<SmalltalkVariable> clRep, List<SmalltalkFunctionDefinition> exportedList)
            : this(new Name(className, 0, 0), superClassName, classVariableList, clRep, exportedList)
        {
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherClass = obj as SmalltalkClass;

            return otherClass != null && ClassName == otherClass.ClassName;
        }

        public override int GetHashCode()
        {
            return ClassName.GetHashCode();
        }

        public SmalltalkFunctionDefinition FindMethod(string methodName, out SmalltalkClass classInWhichMethodWasFound)
        {

            if (ExportedDict.ContainsKey(methodName))
            {
                classInWhichMethodWasFound = this;
                return ExportedDict[methodName];
            }
            else if (SuperClass != null)
            {
                return SuperClass.FindMethod(methodName, out classInWhichMethodWasFound);
            }
            else
            {
                classInWhichMethodWasFound = null;
                return null;
            }
        }

        public ISmalltalkValue FindClassVariableValue(SmalltalkVariable variable)
        {

            if (ClassVariableEnvFrame.Dict.ContainsKey(variable))
            {
                return ClassVariableEnvFrame.Dict[variable];
            }
            else if (SuperClass != null)
            {
                return SuperClass.FindClassVariableValue(variable);
            }
            else
            {
                return null;
            }
        }

        public bool TrySetClassVariableValue(SmalltalkVariable variable, ISmalltalkValue value)
        {

            if (ClassVariableEnvFrame.Dict.ContainsKey(variable))
            {
                ClassVariableEnvFrame.Dict[variable] = value;
                return true;
            }
            else if (SuperClass != null)
            {
                return SuperClass.TrySetClassVariableValue(variable, value);
            }
            else
            {
                return false;
            }
        }

        public void AddFunction(ITokenizer tokenizer, IParser parser, string functionAsString)
        {
            var funcDef = (SmalltalkFunctionDefinition)parser.Parse(tokenizer.Tokenize(functionAsString));

            ExportedDict[funcDef.FunctionName] = funcDef;
        }

        public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
        {

            if (ReservedTypeNames.Contains(ClassName))
            {
                throw new EvaluationException(
                    string.Format("SmalltalkClass.Evaluate() : Cannot create a class named '{0}'; reserved typename", ClassName),
                    LineNumber, ColumnNumber);
            }

            globalInfo.ClassDict[ClassName] = this;

            if (string.IsNullOrEmpty(SuperClassName))
            {
                throw new EvaluationException(
                    string.Format("SmalltalkClass.Evaluate() : Class {0} : SuperClassName is null or empty", ClassName),
                    LineNumber, ColumnNumber);
            }
            // TODO 2014/12/09 : Throw an exception if ClassName == SuperClassName
            else if (!globalInfo.ClassDict.ContainsKey(SuperClassName))
            {
                throw new EvaluationException(
                    string.Format("SmalltalkClass.Evaluate() : Class {0} : Unknown SuperClass {1}", ClassName, SuperClassName),
                    LineNumber, ColumnNumber);
            }
            
            SuperClass = globalInfo.ClassDict[SuperClassName];
            ClRep.AddRange(SuperClass.ClRep);

            foreach (var exportedFuncDef in ExportedList)
            {
                ExportedDict[exportedFuncDef.FunctionName] = exportedFuncDef;
            }

            foreach (var classVariable in ClassVariableList)
            {
                ClassVariableEnvFrame.Dict[classVariable] = globalInfo.ZeroValue;
            }

            return globalInfo.ZeroValue;
        }
    }

    #endregion

    #region SmalltalkObjectClassKeeper

    public static class SmalltalkObjectClassKeeper
    {
        public static readonly SmalltalkVariable SelfVar;   // Immutable.
        public static readonly SmalltalkClass ObjectClass;

        static SmalltalkObjectClassKeeper()
        {
            SelfVar = new SmalltalkVariable("self");
            ObjectClass = new SmalltalkClass(
                "Object",
                null, 
                new List<SmalltalkVariable>(), 
                new List<SmalltalkVariable>() { SelfVar }, 
                new List<SmalltalkFunctionDefinition>());
        }
    }

    #endregion

    #region SmalltalkGlobalInfo

    public class SmalltalkGlobalInfo : IGlobalInfoOps
    {
        private readonly ITokenizer Tokenizer;
        private readonly IParser Parser;
        public readonly ISmalltalkValue ZeroValue = new SmalltalkIntegerValue(0);
        private ISmalltalkValue FalseVal = null;
        private ISmalltalkValue TrueVal = null;
        public readonly SmalltalkEnvironmentFrame GlobalEnvironment = new SmalltalkEnvironmentFrame(null);
        public readonly Dictionary<string, SmalltalkFunctionDefinition> FunctionDefinitions = new Dictionary<string, SmalltalkFunctionDefinition>();
        public readonly Dictionary<string, SmalltalkClass> ClassDict = new Dictionary<string, SmalltalkClass>();
        public readonly SmalltalkUserValue ObjectInstance;  // Passed to Evaluate() by the interpreter; see Kamin pages 297-298.
        public readonly Random RandomNumberGenerator = new Random();
        private const string NilVariableName = "nil";
        public const string NilValueAsString = "nil";
        private const string FalseValueClassName = "FalseValue";
        private const string FalseVariableName = "false";
        public const string FalseValueAsString = "false"; //"0"; // Change this to "false"
        private const string TrueValueClassName = "TrueValue";
        private const string TrueVariableName = "true";
        public const string TrueValueAsString = "true"; //"1"; // Change this to "true"
        private readonly HashSet<string> LoadedPresets = new HashSet<string>();

        public SmalltalkGlobalInfo(ITokenizer t, IParser p)
        {
            Tokenizer = t;
            Parser = p;

            // These are temporary values for FalseVal and TrueVal; hopefully they are not used.
            //FalseVal = ZeroValue;
            //TrueVal = new SmalltalkIntegerValue(1);

            var objectClass = SmalltalkObjectClassKeeper.ObjectClass;
            var objectInstanceEnvFrame = new SmalltalkEnvironmentFrame(null);

            objectClass.AddFunction(t, p, string.Format("(define isNil () {0})", FalseVariableName));
            objectClass.AddFunction(t, p, string.Format("(define notNil () {0})", TrueVariableName));

            ClassDict[objectClass.ClassName] = objectClass;
            objectInstanceEnvFrame.Add(SmalltalkObjectClassKeeper.SelfVar, ZeroValue);
            ObjectInstance = new SmalltalkUserValue(objectClass, objectInstanceEnvFrame);
            ObjectInstance.Value.Dict[SmalltalkObjectClassKeeper.SelfVar] = ObjectInstance;
        }

        public void Clear()
        {
            GlobalEnvironment.Dict.Clear();
            FunctionDefinitions.Clear();
            ClassDict.Clear();
            LoadedPresets.Clear();

            ClassDict[SmalltalkObjectClassKeeper.ObjectClass.ClassName] = SmalltalkObjectClassKeeper.ObjectClass;
        }

        private void Evaluate(string input)
        {
            var expr = Parser.Parse(Tokenizer.Tokenize(input)) as ISmalltalkExpression;

            if (expr == null)
            {
                throw new Exception(string.Format("SmalltalkGlobalInfo.Evaluate() : Parse failed; input is: {0}", input));
            }

            expr.Evaluate(null, ObjectInstance, null, this);
        }

        public string LoadPreset(string presetName)
        {
            var presetNameToLower = presetName.ToLower();

            if (LoadedPresets.Contains(presetNameToLower))
            {
                return string.Format("The preset '{0}' has already been loaded.", presetName);
            }

            switch (presetNameToLower)
            {
                case "collection":
                    // From Kamin page 283.
                    const string collectionClass = @"
(class Collection Object ()
    () ; abstract class
    (define first () #subclassResponsibility)
    (define next () #subclassResponsibility)
    (define add: (item) #subclassResponsibility)
    (define size ()
        (let ((tempitem (first self)) ; This has been modified to use 'let'.
              (tempsize 0))
            (begin
                (while (notNil tempitem)
                    (begin
                        (set tempsize (+1 tempsize))
                        (set tempitem (next self))))
                tempsize)))
    (define isEmpty () (isNil (first self)))
    (define includes: (item)
        (let ((tempitem (first self))
              (found false))
            (begin
                (while (and (notNil tempitem) (not found))
                    (if (= tempitem item)
                        (set found true)
                        (set tempitem (next self))))
                found)))
    ; The next three methods are described in Exercise 3 on page 345.
    (define asSet ()
        (let ((result (mkSet))
              (tempitem (first self)))
            (begin
                (while (notNil tempitem)
                    (begin
                        (add: result tempitem)
                        (set tempitem (next self))))
                result)))
    (define occurrencesOf: (item)
        (let ((tempitem (first self))
              (count 0))
            (begin
                (while (notNil tempitem)
                    (begin
                        (if (= item tempitem)
                            (set count (+1 count))
                            0) ; The 0 is essentially a no-op.
                        (set tempitem (next self))))
                count)))
    (define addAll: (collection)
        (let ((tempitem (first collection)))
            (begin
                (while (notNil tempitem)
                    (begin
                        (add: self tempitem)
                        (set tempitem (next collection)))))))
)";

                    // From Kamin page 286.
                    const string keyedCollectionClass = @"
(class KeyedCollection Collection ()
    () ; abstract class
    (define at:put: (key value) #subclassResponsibility)
    (define currentKey () #subclassResponsibility)
    (define at: (key)
        (begin
            (set tempvalue (first self))
            (set found false)
            (while (and (notNil tempvalue) (not found))
                (if (= key (currentKey self))
                    (set found true)
                    (set tempvalue (next self))))
            tempvalue)) ; note: nil if key out of range
    (define includesKey: (key) (notNil (at: self key)))
    (define indexOf: (value)
        (begin
            (set tempvalue (first self))
            (set found false)
            (while (and (notNil tempvalue) (not found))
                (if (= value tempvalue)
                    (set found true)
                    (set tempvalue (next self))))
            (if (isNil tempvalue) nil (currentKey self))))
)";

                    // From Kamin page 289.
                    const string sequenceableCollectionClass = @"
(class SequenceableCollection KeyedCollection ()
    () ; abstract class
    (define firstKey () #subclassResponsibility)
    (define lastKey () #subclassResponsibility)
    (define last () (at: self (lastKey self)))
    (define at: (index)
        (begin
            (set iterations (- index (firstKey self)))
            (set result (first self))
            (while (> iterations 0)
                (begin
                    (set result (next self))
                    (set iterations (- iterations 1))))
            result))
)";

                    // From Kamin page 290.
                    const string listClass = @"
(class List SequenceableCollection ()
    (car cdr currentKey currentCell)
    (define car () car)
    (define cdr () cdr)
    (define init () (begin (set car nil) self)) ; super allows us to use a uniform init instead of initList et al.
    (define add: (item)
        (let ((temp (newEmptyCollection self))) ; See page 308.
            (begin
                (car: temp car)
                (cdr: temp cdr)
                (set cdr temp)
                (set car item))))
    (define newEmptyCollection () (init (new List))) ; See page 308.
    (define car: (x) (set car x))
    (define cdr: (x) (set cdr x))
    (define first ()
        (begin
            (set currentKey 1)
            (set currentCell self)
            car))
    (define next ()
        (if (isNil (car currentCell)) nil
            (begin
                (set currentKey (+1 currentKey))
                (set currentCell (cdr currentCell))
                (car currentCell))))
    (define firstKey () 1)
    (define lastKey () (size self))
    (define currentKey () currentKey)
    (define at:put: (n value)
        (if (= n 1) (set car value)
            (at:put: cdr (- n 1) value)))
    (define removeFirst ()
        (if (isEmpty self) self ; do nothing
            (begin
                (set car (car cdr))
                (set cdr (cdr cdr)))))
    (define zerolist (size)
        (while (> size 0)
            (begin
                (add: self 0)
                (set size (- size 1)))))
)";

                    // From Kamin page 283.
                    const string setClass = @"
(class Set Collection ()
    (members) ; list of elements
    (define init () (begin (set members (mkList)) self))
    (define first () (first members))
    (define next () (next members))
    (define add: (item)
        (if (includes: members item) self (add: members item)))
)";

                    // From Kamin page 286.
                    const string associationClass = @"
(class Association Object ()
    (fst snd)
    (define init (x y) (begin (set fst x) (set snd y) self))
    (define fst () fst)
    (define snd () snd)
    (define fst: (x) (set fst x))
    (define snd: (y) (set snd y))
)";

                    // From Kamin page 288.
                    const string dictionaryClass = @"
(class Dictionary KeyedCollection ()
    (table currentKey)
    (define init ()
        (begin (set table (mkList)) self))
    (define currentKey () currentKey)
    (define first ()
        (if (isEmpty table) nil
            (begin
                (set tempassoc (first table))
                (set currentKey (fst tempassoc))
                (snd tempassoc))))
    (define next ()
        (begin
            (set tempassoc (next table))
            (if (isNil tempassoc) nil
                (begin
                    (set currentKey (fst tempassoc))
                    (snd tempassoc)))))
    (define at:put: (key value)
        (begin
            (set tempassoc (associationAt: self key))
            (if (isNil tempassoc)
                (add: table (mkAssociation key value))
                (snd: tempassoc value))))
    (define associationAt: (key)
        (begin
            (set temptable table)
            (set found false)
            (while (not (or (isEmpty temptable) found))
                (if (= (fst (car temptable)) key)
                    (set found true)
                    (set temptable (cdr temptable))))
            (if found (car temptable) nil)))
)";

                    // From Kamin page 291.  (Not used by the Financial History example.)
#if DEAD_CODE
                    const string arrayClass = @"
(class Array SequenceableCollection ()
    (elements lobound hibound currentKey)
    (define init (lo size)
        (begin
            (set lobound lo)
            (set hibound (- (+ lo size) 1))
            (set elements (new List))
            (zerolist elements size)
            self))
    (define size () (+1 (- hibound lobound)))
    (define firstKey () lobound)
    (define lastKey () hibound)
    (define currentKey () currentKey)
    (define first ()
        (begin
            (set currentKey lobound)
            (first elements)))
    (define next ()
        (if (= currentKey hibound) nil
            (begin
                (set currentKey (+1 currentKey))
                (next elements))))
    (define at:put: (n value)
        (if (or (< n lobound) (> n hibound)) nil ; Slightly modified condition
            (at:put: elements (+1 (- n lobound)) value)))
)";
#else
                    // Re-implement the Array class using the built-in "array" value type.
                    const string arrayClass = @"
(class Array SequenceableCollection ()
    (elements lobound hibound currentKey)
    (define init (lo size)
        (begin
            (set lobound lo)
            (set hibound (- (+ lo size) 1))
            (set elements (newarray size))
            ; (zerolist elements size)
            self))
    (define size () (arraylength elements))
    (define firstKey () lobound)
    (define lastKey () hibound)
    (define currentKey () currentKey)
    (define first ()
        (begin
            (set currentKey lobound)
            (at: self lobound)))
    (define next ()
        (if (= currentKey hibound) nil
            (begin
                (set currentKey (+1 currentKey))
                (at: self currentKey))))
    (define at:put: (n value)
        (if (or (< n lobound) (> n hibound)) nil ; Slightly modified condition
            (arrayset elements (+1 (- n lobound)) value)))
    (define at: (index)
        (if (or (< index lobound) (> index hibound)) nil ; Slightly modified condition
            (arrayget elements (+1 (- index lobound))))) ; ThAW 2014/02/01 : Override the 'at:' that is in SequenceableCollection.
)";
#endif

                    // ThAW 2014/02/03 : There are some similarities between Stack and Queue.
                    const string stackClass = @"
(class Stack List () ()
    (define init () (init super))
    (define newEmptyCollection () (init (new Stack)))
    (define peek () car)
    (define push: (item) (add: self item))
    (define pop ()
        (if (isEmpty self) nil
            (let ((result car))
                (begin
                    (removeFirst self)
                    result))))
)";
                    // This Queue class is somewhat similar to the one on page 309.
                    const string queueClass = @"
(class Queue List () ()
    (define init () (init super))
    (define newEmptyCollection () (init (new Queue)))
    (define peek () car)
    (define enqueue: (item) ; Add the item to the end of the queue.
        (if (isEmpty self)
            (add: self item)
            (enqueue: cdr item)))
    (define dequeue ()
        (if (isEmpty self) nil
            (let ((result car))
                (begin
                    (removeFirst self)
                    result))))
)";
                    // See page 310.
                    const string priorityQueueClass = @"
(class PriorityQueue List () ()
    (define init () (init super))
    (define newEmptyCollection () (init (new PriorityQueue)))
    (define peek () car)
    (define enqueue: (pair) ; Insert the item at the appropriate place in the queue.
        (cond
            ((isEmpty self) (add: self pair))
            ; ThAW 2014/02/07 : I replaced these two lines...
            ;((< (fst pair) (fst car)) (add: self pair))
            ;(true (enqueue: cdr pair))))
            ; ... with these two lines:
            ((< (fst car) (fst pair)) (enqueue: cdr pair))
            (true (add: self pair))))
    (define dequeue ()
        (if (isEmpty self) nil
            (let ((result car))
                (begin
                    (removeFirst self)
                    result))))
)";

                    Evaluate(collectionClass);
                    Evaluate(keyedCollectionClass);
                    Evaluate(sequenceableCollectionClass);
                    Evaluate(listClass);
                    Evaluate("(define mkList () (init (new List)))");
                    Evaluate(setClass);
                    Evaluate("(define mkSet () (init (new Set)))");
                    Evaluate(associationClass);
                    Evaluate("(define mkAssociation (a b) (init (new Association) a b))");
                    Evaluate(dictionaryClass);
                    Evaluate("(define mkDictionary () (init (new Dictionary)))");
                    Evaluate(arrayClass);
                    Evaluate("(define mkArray (l s) (init (new Array) l s))");
                    Evaluate(stackClass);
                    Evaluate("(define mkStack () (init (new Stack)))");
                    Evaluate(queueClass);
                    Evaluate("(define mkQueue () (init (new Queue)))");
                    Evaluate(priorityQueueClass);
                    Evaluate("(define mkPriorityQueue () (init (new PriorityQueue)))");
                    break;

                default:
                    throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
            }

            LoadedPresets.Add(presetNameToLower);
            return string.Format("The preset '{0}' has been successfully loaded.", presetName);
        }

        public void LoadPresets()
        {
            GlobalEnvironment.Add(new SmalltalkVariable("e" /*, 0, 0 */), new SmalltalkFloatValue(Math.E));
            GlobalEnvironment.Add(new SmalltalkVariable("pi" /*, 0, 0 */), new SmalltalkFloatValue(Math.PI));

            Evaluate(string.Format(@"
(class {0} Object ()
    (stringValue) ; stringValue is used as the value of the object of this class when it is converted to a string.
    (define init () (begin (set stringValue '{1}') self))
    (define if (trueBlock falseBlock) falseBlock)
    (define and (x) {2})
    (define or (x) x)
    (define xor (x) x)
    (define not () {3})
)", FalseValueClassName, FalseValueAsString, FalseVariableName, TrueVariableName));
            Evaluate(string.Format(@"
(class {0} Object ()
    (stringValue) ; stringValue is used as the value of the object of this class when it is converted to a string.
    (define init () (begin (set stringValue '{1}') self))
    (define if (trueBlock falseBlock) trueBlock)
    (define and (x) x)
    (define or (x) {2})
    (define xor (x) (not x))
    (define not () {3})
)", TrueValueClassName, TrueValueAsString, TrueVariableName, FalseVariableName));
            Evaluate(string.Format("(set {0} (init (new {1})))", FalseVariableName, FalseValueClassName));
            Evaluate(string.Format("(set {0} (init (new {1})))", TrueVariableName, TrueValueClassName));
            FalseVal = GlobalEnvironment.Dict[new SmalltalkVariable(FalseVariableName)];
            TrueVal = GlobalEnvironment.Dict[new SmalltalkVariable(TrueVariableName)];

            Evaluate(string.Format(@"
(class UndefinedObject Object ()
    (stringValue) ; stringValue (#nil) is used as the value of the object of this class when it is converted to a string.
    (define init () (begin (set stringValue '{0}') self))
    (define isNil () {1})
    (define notNil () {2})
)", NilValueAsString, TrueVariableName, FalseVariableName));
            Evaluate("(set nil (init (new UndefinedObject)))");

            Evaluate("(define > (x y) (< y x))");
            //Evaluate("(define and (x y) (if x y x))");
            //Evaluate("(define or (x y) (if x x y))");
            //Evaluate(string.Format("(define not (x) (if x {0} {1}))", FalseVariableName, TrueVariableName));
            Evaluate("(define <> (x y) (not (= x y)))");
            Evaluate("(define <= (x y) (not (> x y)))");
            Evaluate("(define >= (x y) (not (< x y)))");
            Evaluate("(define +1 (x) (+ x 1))");
            Evaluate("(define mod (m n) (- m (* n (/ m n))))");
            Evaluate("(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))");
            Evaluate("(define abs (n) (if (< n 0) (- 0 n) n))");
        }

        public ISmalltalkValue FalseValue
        {
            get
            {

                if (FalseVal == null)
                {
                    throw new Exception("FalseValue was used before being defined");
                }

                return FalseVal;
            }
        }

        public ISmalltalkValue TrueValue
        {
            get
            {

                if (TrueVal == null)
                {
                    throw new Exception("TrueValue was used before being defined");
                }

                return TrueVal;
            }
        }

        public bool ValueIsFalse(ISmalltalkValue value)
        {
            //return value.Owner.Equals(ClassDict["FalseValue"]);
            return value.Owner.ClassName == FalseValueClassName;
        }

        public bool ValueIsInteger(ISmalltalkValue value)
        {
            return value is SmalltalkIntegerValue;
        }

        public int ValueAsInteger(ISmalltalkValue value)
        {
#if DEAD_CODE
            if (!ValueIsInteger(value))
            {
                throw new ArgumentException("ValueAsInteger() : value is not an integer");
            }

            var prim = (SmalltalkIntegerValue)value;

            return prim.Value;
#else
            var valueAsNumber = value as ISmalltalkNumber;

            if (valueAsNumber == null)
            {
                throw new ArgumentException("ValueAsInteger() : The value is not an ISmalltalkNumber.");
            }

            return valueAsNumber.ToInteger();
#endif
        }

        public ISmalltalkValue IntegerAsValue(int value)
        {
            return new SmalltalkIntegerValue(value);
        }

        public bool SetScoping(bool dynamicScoping)
        {
            return false;
        }

        public bool SetDebug(bool debug)
        {
            return false;
        }

        public static ISmalltalkValue UnblockValue(ISmalltalkValue value)
        {
            var block = value as SmalltalkBlock;

            if (block != null)
            {
                return block.Unblock();
            }
            else
            {
                return value;
            }
        }
    }

    #endregion
}
