using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Inference.Domain
{
    #region Enums

    public enum VariableSearchType
    {
        All,
        Bound,
        Unbound
    }

    #endregion

    #region Interfaces

    public interface IExpression
    {
        HashSet<Variable> FindVariables(VariableSearchType vst);
        HashSet<string> FindAllSkolemFunctionNames();
        IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict);
        bool ContainsVariable(Variable v);
        IExpression ApplySubstitution(Substitution sub);
    }

    public interface IUnifiable : IExpression
    {
        Substitution Unify(IUnifiable otherExpr);
    }

    public interface IArgument : IUnifiable
    {
        //string Name { get; }
    }

    public interface IBooleanExpression : IExpression
    {
        IBooleanExpression Negate();
        IBooleanExpression PushDownNegations();
        IBooleanExpression UniquefyQuantifierVariables(HashSet<string> quantifierVariableNames, ref int nextQuantifierVariableNum);
        IBooleanExpression ReplaceExistsWithSkolemFunctions(HashSet<string> skolemFuncNames, ArgumentList forAllVarsInScope, ref int nextSkolemFuncNum);
        IBooleanExpression DropUniversalQuantifiers();      // I.e. convert all "ForAll ?var (@func(?var))" to "@func(?var)".
        List<Clause> ToClausalForm();
    }

    #endregion

    #region Substitution

    public class Substitution
    {
        public Dictionary<Variable, IUnifiable> SubstitutionList { get; private set; }

        public Substitution()
        {
            SubstitutionList = new Dictionary<Variable, IUnifiable>();
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            var separator = string.Empty;

            foreach (var key in SubstitutionList.Keys)
            {
                sb.Append(separator);
                sb.Append(key.ToString());
                sb.Append(" <= ");
                sb.Append(SubstitutionList[key].ToString());

                separator = "; ";
            }

            return sb.ToString();
        }

        public Substitution Compose(Substitution otherSub)
        {
            var newSub = new Substitution();

            // 1) Apply the Src substitution to this's terms.

            foreach (var key in SubstitutionList.Keys)
            {
                var newUnifiable = SubstitutionList[key].ApplySubstitution(otherSub) as IUnifiable;

                if (newUnifiable == null)
                {
                    throw new Exception("Substitution.Compose() : The result of applying a substitution to an IUnifiable is not an IUnifiable.");
                }

                newSub.SubstitutionList[key] = newUnifiable;
            }

            // 2) Remove identities.
            var varsToRemove = new List<Variable>();

            foreach (var key in newSub.SubstitutionList.Keys)
            {

                if (key.Equals(newSub.SubstitutionList[key]))
                {
                    varsToRemove.Add(key);
                }
            }

            foreach (var v in varsToRemove)
            {
                newSub.SubstitutionList.Remove(v);
            }

            // 3) Remove duplicate variables; i.e. add substitutions from keys in otherSub that are not keys in the "this" Substitution.

            foreach (var key in otherSub.SubstitutionList.Keys)
            {

                //if (!newSub.SubstitutionList.ContainsKey(key))    // In error.
                if (!SubstitutionList.ContainsKey(key))             // Correct, according to the CS 486 course notes.
                {
                    newSub.SubstitutionList[key] = otherSub.SubstitutionList[key];
                }
            }

            return newSub;
        }

        public bool ContainsOnlyVariables()
        {
            /*
            foreach (var v in SubstitutionList.Values)
            {

                if (!(v is Variable))
                {
                    return false;
                }
            }

            return true;
             */
            return SubstitutionList.Values.All(v => v is Variable);
        }
    }

    #endregion

    #region Constant

    public class Constant : IArgument
    {
        public string Name { get; private set; }

        public Constant(string name)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A Constant cannot have a null or empty name");
            }

            Name = name;
        }

        public static string GetName(Constant c)
        {

            if (c == null)
            {
                throw new ArgumentNullException("c", "Constant.GetName() : c is null");
            }

            return c.Name;
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

            Constant otherConst = obj as Constant;

            return otherConst != null && Name == otherConst.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            return new HashSet<Variable>();
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            return new HashSet<string>();
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            return this;
        }

        public bool ContainsVariable(Variable v)
        {
            return false;
        }

        public IExpression ApplySubstitution(Substitution sub)
        {
            return this;
        }

        public Substitution Unify(IUnifiable otherExpr)
        {

            if (Equals(otherExpr))  // Do not use "if (this == otherExpr)", which just compares references.
            {
                return new Substitution();
            }
            else if (otherExpr is Variable)
            {
                return otherExpr.Unify(this);
            }

            return null;    // The Constant and the IUnifiable are not unifiable.
        }
    }

    #endregion

    #region IntegerLiteral

    public class IntegerLiteral : IArgument
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

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            return new HashSet<Variable>();
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            return new HashSet<string>();
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            return this;
        }

        public bool ContainsVariable(Variable v)
        {
            return false;
        }

        public IExpression ApplySubstitution(Substitution sub)
        {
            return this;
        }

        public Substitution Unify(IUnifiable otherExpr)
        {

            if (Equals(otherExpr))  // Do not use "if (this == otherExpr)", which just compares references.
            {
                return new Substitution();
            }
            else if (otherExpr is Variable)
            {
                return otherExpr.Unify(this);
            }

            return null;    // The IntegerLiteral and the IUnifiable are not unifiable.
        }
    }

    #endregion

    #region StringLiteral

    public class StringLiteral : IArgument
    {
        public readonly string Value;

        public StringLiteral(object value)
        {

            if (!(value is string))
            {
                throw new ArgumentException("StringLiteral constructor: value is not a string.", "value");
            }

            Value = value as string;
        }

        public override string ToString()
        {
            return string.Format("\"{0}\"", Value);
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            StringLiteral otherStrLit = obj as StringLiteral;

            return otherStrLit != null && Value == otherStrLit.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            return new HashSet<Variable>();
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            return new HashSet<string>();
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            return this;
        }

        public bool ContainsVariable(Variable v)
        {
            return false;
        }

        public IExpression ApplySubstitution(Substitution sub)
        {
            return this;
        }

        public Substitution Unify(IUnifiable otherExpr)
        {

            if (Equals(otherExpr))  // Do not use "if (this == otherExpr)", which just compares references.
            {
                return new Substitution();
            }
            else if (otherExpr is Variable)
            {
                return otherExpr.Unify(this);
            }

            return null;    // The StringLiteral and the IUnifiable are not unifiable.
        }
    }

    #endregion

    #region Variable

    public class Variable : IArgument
    {
        public string Name { get; private set; }

        public Variable(string name)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A Variable cannot have a null or empty name");
            }

            Name = name;
        }

        public override string ToString()
        {
            return "?" + Name;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            Variable otherVar = obj as Variable;

            return otherVar != null && Name == otherVar.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            var result = new HashSet<Variable>();

            if (vst != VariableSearchType.Bound)
            {
                result.Add(this);
            }

            return result;
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            return new HashSet<string>();
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            return this;
        }

        public bool ContainsVariable(Variable v)
        {
            //return this == v;
            return Equals(v);
        }

        public IExpression ApplySubstitution(Substitution sub)
        {

            if (sub.SubstitutionList.ContainsKey(this))
            {
                return sub.SubstitutionList[this];
            }

            return this;
        }

        public Substitution Unify(IUnifiable otherExpr)
        {
            Substitution sub = new Substitution();

            if (Equals(otherExpr))
            {
            }
            else if (otherExpr.ContainsVariable(this))
            {
                return null;    // This Variable and the IUnifiable are not unifiable.
            }
            else
            {
                sub.SubstitutionList[this] = otherExpr;
            }

            return sub;
        }
    }

    #endregion

    #region ArgumentList

    public class ArgumentList
    {
        public readonly List<IArgument> ArgList = null;

        public ArgumentList(List<IArgument> argList)
        {
            ArgList = argList ?? new List<IArgument>();
        }

        public ArgumentList()
            : this(null)
        {
        }

        public ArgumentList(IArgument leftArg, ArgumentList rightArgList)
            : this()
        {

            if (leftArg == null)
            {
                throw new ArgumentNullException("leftArg", "Cannot add a null Argument to an ArgumentList");
            }

            ArgList.Add(leftArg);

            if (rightArgList != null)
            {

                foreach (var arg in rightArgList.ArgList)
                {
                    ArgList.Add(arg);
                }
            }
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            var separator = string.Empty;

            foreach (var arg in ArgList)
            {
                sb.Append(separator);
                separator = ", ";
                sb.Append(arg.ToString());
            }

            return sb.ToString();
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherArgList = obj as ArgumentList;

            if (otherArgList == null)
            {
                return false;
            }

            Substitution sub = Unify(otherArgList);

            return sub != null && sub.SubstitutionList.Count == 0;
        }

        public override int GetHashCode()
        {
            /*
            int hc = 0;

            foreach (var arg in ArgList)
            {
                hc *= 101;
                hc += arg.GetHashCode();
            }

            return hc;
             */
            return ArgList
                .Select(arg => arg.GetHashCode())
                .Aggregate(0, (accumulator, hashCode) => accumulator * 101 + hashCode);
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            var result = new HashSet<Variable>();

            foreach (var arg in ArgList)
            {
                result.UnionWith(arg.FindVariables(vst));
            }

            return result;
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            var result = new HashSet<string>();

            foreach (var arg in ArgList)
            {
                result.UnionWith(arg.FindAllSkolemFunctionNames());
            }

            return result;
        }

        public ArgumentList ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            var result = new ArgumentList();

            foreach (var arg in ArgList)
            {
                result.ArgList.Add(arg.ReplaceSkolemFunctionNames(dict) as IArgument);
            }

            return result;
        }

        public bool ContainsVariable(Variable v)
        {
            /*
            foreach (var arg in ArgList)
            {

                if (arg.ContainsVariable(v))
                {
                    return true;
                }
            }

            return false;
             */
            return ArgList.Any(arg => arg.ContainsVariable(v));
        }

        public ArgumentList ApplySubstitution(Substitution sub)
        {
            var result = new ArgumentList();

            foreach (var arg in ArgList)
            {
                var newArg = arg.ApplySubstitution(sub) as IArgument;

                if (newArg == null)
                {
                    throw new Exception("The result of applying a substitution to an IArgument is not an IArgument.");
                }

                result.ArgList.Add(newArg);
            }

            return result;
        }

        public Substitution Unify(ArgumentList otherArgList)
        {

            if (ArgList.Count != otherArgList.ArgList.Count)
            {
                return null;    // Argument lists of different lengths cannot be unified.
            }

            var sub = new Substitution();

            for (int i = 0; i < ArgList.Count; ++i)
            {
                var newExpr1 = ArgList[i].ApplySubstitution(sub) as IUnifiable;
                var newExpr2 = otherArgList.ArgList[i].ApplySubstitution(sub) as IUnifiable;

                if (newExpr1 == null || newExpr2 == null)
                {
                    throw new Exception("ArgumentList.Unify() : The result of applying a substitution to an IUnifiable is not an IUnifiable.");
                }

                Substitution sub2 = newExpr1.Unify(newExpr2);

                if (sub2 == null)
                {
                    return null;
                }

                sub = sub.Compose(sub2);
            }

            return sub;
        }
    }

    #endregion

    #region BooleanConstant

    public class BooleanConstant //: IArgument
    {
        public string Name { get; private set; }

        public BooleanConstant(string name)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A BooleanConstant cannot have a null or empty name");
            }

            Name = name;
        }

        public static string GetName(BooleanConstant bc)
        {

            if (bc == null)
            {
                throw new ArgumentNullException("bc", "BooleanConstant.GetName() : bc is null");
            }

            return bc.Name;
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

            BooleanConstant otherBoolConst = obj as BooleanConstant;

            return otherBoolConst != null && Name == otherBoolConst.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }
    }

    #endregion

    #region FunctionBase

    public abstract class FunctionBase : IUnifiable
    {
        public string Name { get; private set; }
        public ArgumentList ArgumentList { get; private set; }
        private readonly int HashCodePart1;
        private readonly string NamePrefix;

        public FunctionBase(int hc1, string namePrefix, string name, ArgumentList argList)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A FunctionBase<T> name cannot be null or empty");
            }

            HashCodePart1 = hc1;
            NamePrefix = namePrefix;
            Name = name;
            ArgumentList = argList ?? new ArgumentList();
        }

        public abstract FunctionBase MakeNewFunctionBaseOfSameType(string name, ArgumentList argList);

        public abstract FunctionBase AsFunctionBaseOfSameType(object o);

        public override string ToString()
        {
            var sb = new StringBuilder();
            var separator = string.Empty;

            if (!string.IsNullOrEmpty(NamePrefix))
            {
                sb.Append(NamePrefix);
            }

            sb.Append(Name);
            sb.Append("(");
            sb.Append(ArgumentList.ToString());
            sb.Append(")");

            return sb.ToString();
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            FunctionBase otherFuncBase = AsFunctionBaseOfSameType(obj);

            return otherFuncBase != null && Name == otherFuncBase.Name && ArgumentList.Equals(otherFuncBase.ArgumentList);
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            return ArgumentList.FindVariables(vst);
        }

        public virtual HashSet<string> FindAllSkolemFunctionNames()
        {
            return ArgumentList.FindAllSkolemFunctionNames();
        }

        public virtual IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            return MakeNewFunctionBaseOfSameType(Name, ArgumentList.ReplaceSkolemFunctionNames(dict));
        }

        public bool ContainsVariable(Variable v)
        {
            return ArgumentList.ContainsVariable(v);
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode() + HashCodePart1 * ArgumentList.GetHashCode();
        }

        public IExpression ApplySubstitution(Substitution sub)
        {
            return MakeNewFunctionBaseOfSameType(Name, ArgumentList.ApplySubstitution(sub));
        }

        public Substitution Unify(IUnifiable otherExpr)
        {
            Variable v = otherExpr as Variable;

            if (v != null)
            {

                if (ContainsVariable(v))
                {
                    return null;    // Not unifiable.
                }

                Substitution sub = new Substitution();

                sub.SubstitutionList[v] = this;
                return sub;
            }

            FunctionBase otherFuncBase = AsFunctionBaseOfSameType(otherExpr);

            if (otherFuncBase == null || Name != otherFuncBase.Name)
            {
                return null;
            }

            return ArgumentList.Unify(otherFuncBase.ArgumentList);
        }
    }

    #endregion

    #region Function

    public class Function : FunctionBase, IArgument
    {
        public Function(string name, ArgumentList argList)
            : base(127, string.Empty, name, argList)
        {
        }

        public Function(Constant c, ArgumentList argList)
            : this(Constant.GetName(c), argList)
        {
        }

        public override FunctionBase MakeNewFunctionBaseOfSameType(string name, ArgumentList argList)
        {
            return new Function(name, argList);
        }

        public override FunctionBase AsFunctionBaseOfSameType(object o)
        {
            return o as Function;
        }
    }

    #endregion

    #region SkolemFunction

    public class SkolemFunction : FunctionBase, IArgument
    {
        public SkolemFunction(string name, ArgumentList argList)
            : base(151, "$", name, argList)
        {
        }

        public override FunctionBase MakeNewFunctionBaseOfSameType(string name, ArgumentList argList)
        {
            return new SkolemFunction(name, argList);
        }

        public override FunctionBase AsFunctionBaseOfSameType(object o)
        {
            return o as SkolemFunction;
        }

        public override HashSet<string> FindAllSkolemFunctionNames()
        {
            var result = base.FindAllSkolemFunctionNames(); // new HashSet<string>();

            result.Add(Name);
            //result.UnionWith(base.FindAllSkolemFunctionNames());
            return result;
        }

        public override IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {

            if (!dict.ContainsKey(Name))
            {
                return base.ReplaceSkolemFunctionNames(dict);
            }

            return new SkolemFunction(dict[Name], ArgumentList.ReplaceSkolemFunctionNames(dict));
        }
    }

    #endregion

    #region Predicate

    public class Predicate : FunctionBase, IBooleanExpression
    {
        public Predicate(string name, ArgumentList argList)
            : base(103, "@", name, argList)
        {
        }

        public Predicate(BooleanConstant bc, ArgumentList argList)
            : this(BooleanConstant.GetName(bc), argList)
        {
        }

        public override FunctionBase MakeNewFunctionBaseOfSameType(string name, ArgumentList argList)
        {
            return new Predicate(name, argList);
        }

        public override FunctionBase AsFunctionBaseOfSameType(object o)
        {
            return o as Predicate;
        }

        public IBooleanExpression Negate()
        {
            return new Negation(this);
        }

        public IBooleanExpression PushDownNegations()
        {
            return this;
        }

        public IBooleanExpression UniquefyQuantifierVariables(HashSet<string> quantifierVariableNames, ref int nextQuantifierVariableNum)
        {
            return this;
        }

        public IBooleanExpression ReplaceExistsWithSkolemFunctions(HashSet<string> skolemFuncNames, ArgumentList forAllVarsInScope, ref int nextSkolemFuncNum)
        {
            return this;
        }

        public IBooleanExpression DropUniversalQuantifiers()
        {
            return this;
        }

        public List<Clause> ToClausalForm()
        {
            /*
            List<Clause> result = new List<Clause>();

            result.Add(new Clause(new Literal(this, false)));
            return result;
             */
            return new List<Clause>() { new Clause(new Literal(this, false)) };
        }
    }

    #endregion

    #region Negation

    public class Negation : IBooleanExpression
    {
        public IBooleanExpression BoolExpr { get; private set; }

        public Negation(IBooleanExpression be)
        {

            if (be == null)
            {
                throw new ArgumentNullException("be", "A Negation cannot have a null IBooleanExpression");
            }

            BoolExpr = be;
        }

        public override string ToString()
        {
            Junction junction = BoolExpr as Junction;

            if (junction != null)
            {
                return "!" + junction.ToStringWithBrackets();
            }

            return "!" + BoolExpr.ToString();
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            Negation otherNeg = obj as Negation;

            return otherNeg != null && BoolExpr.Equals(otherNeg.BoolExpr);
        }

        public override int GetHashCode()
        {
            return 107 * BoolExpr.GetHashCode() + 73;
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            return BoolExpr.FindVariables(vst);
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            return BoolExpr.FindAllSkolemFunctionNames();
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            throw new NotImplementedException("Negation.ReplaceSkolemFunctionNames()");
        }

        public bool ContainsVariable(Variable v)
        {
            return BoolExpr.ContainsVariable(v);
        }

        public IExpression ApplySubstitution(Substitution sub)
        {
            IBooleanExpression newBoolExpr = BoolExpr.ApplySubstitution(sub) as IBooleanExpression;

            if (newBoolExpr == null)
            {
                throw new Exception("Negation.ApplySubstitution() : The result of applying a substitution to an IBooleanExpression is not an IBooleanExpression.");
            }

            return new Negation(newBoolExpr);
        }

        /*
        public Substitution Unify(IExpression otherExpr)
        {
            Negation otherNeg = otherExpr as Negation;

            if (otherNeg == null)
            {
                return null;    // TODO: Currently, a Negation can only be unified with another Negation, not a Variable.
            }

            return BoolExpr.Unify(otherNeg.BoolExpr);
        }
         */

        public IBooleanExpression Negate()
        {
            return BoolExpr;
        }

        public IBooleanExpression PushDownNegations()
        {

            if (BoolExpr is Predicate)
            {
                return this;
            }
            else if (BoolExpr is Junction)
            {
                // Apply deMorgan's laws.
                var junct = BoolExpr as Junction;
                Junction newJunct = junct.MakeNewJunctionOfOtherType();

                foreach (var component in junct.ComponentList)
                {
                    newJunct.ComponentList.Add(component.Negate().PushDownNegations());
                }

                return newJunct;
            }
            else if (BoolExpr is Quantifier)
            {
                Quantifier quant = BoolExpr as Quantifier;

                return quant.MakeNewQuantifierOfOtherType(quant.Var, quant.BoolExpr.Negate().PushDownNegations());
            }

            throw new Exception("Negation.PushDownNegations() : BoolExpr is of unhandled type " + ((BoolExpr != null) ? BoolExpr.GetType().FullName : "null"));
        }

        public IBooleanExpression UniquefyQuantifierVariables(HashSet<string> quantifierVariableNames, ref int nextQuantifierVariableNum)
        {
            return new Negation(BoolExpr.UniquefyQuantifierVariables(quantifierVariableNames, ref nextQuantifierVariableNum));
        }

        public IBooleanExpression ReplaceExistsWithSkolemFunctions(HashSet<string> skolemFuncNames, ArgumentList forAllVarsInScope, ref int nextSkolemFuncNum)
        {
            return new Negation(BoolExpr.ReplaceExistsWithSkolemFunctions(skolemFuncNames, forAllVarsInScope, ref nextSkolemFuncNum));
        }

        public IBooleanExpression DropUniversalQuantifiers()
        {
            return new Negation(BoolExpr.DropUniversalQuantifiers());
        }

        public List<Clause> ToClausalForm()
        {

            if (BoolExpr is Predicate)
            {
                /*
                List<Clause> result = new List<Clause>();

                result.Add(new Clause(new Literal(BoolExpr as Predicate, true)));
                return result;
                 */
                return new List<Clause>() { new Clause(new Literal(BoolExpr as Predicate, true)) };
            }
            /*
            else if (BoolExpr is Conjunction)
            {
                // DeMorganize: !(A && B) == !A || !B
                Conjunction conj = BoolExpr as Conjunction;
                Disjunction disj = new Disjunction(null, null);

                foreach (IBooleanExpression be2 in conj.ComponentList)
                {
                    disj.ComponentList.Add(be2.Negate());
                }

                return disj.ToClausalForm();
            }
            else if (BoolExpr is Disjunction)
            {
                // DeMorganize: !(A || B) == !A && !B
                Disjunction disj = BoolExpr as Disjunction;
                Conjunction conj = new Conjunction(null, null);

                foreach (IBooleanExpression be2 in disj.ComponentList)
                {
                    conj.ComponentList.Add(be2.Negate());
                }

                return conj.ToClausalForm();
            }

            throw new NotImplementedException("Negation.ToClausalForm() : BoolExpr is not a Predicate, a Conjunction, or a Disjunction.");
             */
            throw new NotImplementedException("Negation.ToClausalForm() : BoolExpr is not a Predicate.");
        }
    }

    #endregion

    #region Junction

    public abstract class Junction : IBooleanExpression  // Not IUnifiable because the terms may be in any order.
    {
        public readonly List<IBooleanExpression> ComponentList = new List<IBooleanExpression>();
        private readonly int hashCodePart1;
        private readonly int hashCodePart2;
        private readonly string toStringSeparator;

        protected Junction(int hc1, int hc2, string separator, IBooleanExpression expr1, IBooleanExpression expr2)
        {
            hashCodePart1 = hc1;
            hashCodePart2 = hc2;
            toStringSeparator = separator;

            AddComponent(expr1);
            AddComponent(expr2);
        }

        private void AddComponent(IBooleanExpression expr)
        {

            if (expr == null)
            {
                return;
            }

            Junction junct = AsJunctionOfSameType(expr);

            if (junct == null)
            {
                ComponentList.Add(expr);
            }
            else
            {

                foreach (var expr2 in junct.ComponentList)
                {
                    ComponentList.Add(expr2);
                }
            }
        }

        public override string ToString()
        {
            // For e.g. A && B && C, ToString() does not yield "A && (B && C)" because the parser constructed A && B && C as a single conjunction.
            /*
            var sb = new StringBuilder();
            var separator = string.Empty;

            foreach (var expr in ComponentList)
            {
                sb.Append(separator);

                var exprAsJunction = expr as Junction;

                if (exprAsJunction != null)
                {
                    sb.Append(exprAsJunction.ToStringWithBrackets());
                }
                else
                {
                    sb.Append(expr.ToString());
                }

                separator = toStringSeparator;
            }

            return sb.ToString();
             */
            return string.Join(toStringSeparator, ComponentList.Select(
                expr => (expr is Junction) ? (expr as Junction).ToStringWithBrackets() : expr.ToString()));
        }

        public string ToStringWithBrackets()
        {
            // The Inference grammar guarantees that ComponentList.Count is always greater than 1.
            return "(" + ToString() + ")";
        }

        private bool ComponentsAreSubsetOf(Junction otherJunct)
        {
            /*
            foreach (var expr in ComponentList)
            {

                if (!otherJunct.ComponentList.Contains(expr))
                {
                    return false;
                }
            }

            return true;
             */
            return ComponentList.All(expr => otherJunct.ComponentList.Contains(expr));
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            Junction otherJunct = AsJunctionOfSameType(obj);

            return otherJunct != null
                && ComponentList.Count == otherJunct.ComponentList.Count
                && ComponentsAreSubsetOf(otherJunct)
                && otherJunct.ComponentsAreSubsetOf(this);
        }

        public override int GetHashCode()
        {
            // Two objects of the same Junction type with the same terms in different orders will still have the same hash code.
            /*
            int sum = 0;

            foreach (var expr in ComponentList)
            {
                sum += expr.GetHashCode();
            }

            return hashCodePart1 * sum + hashCodePart2;
             */
            return ComponentList
                .Select(expr => expr.GetHashCode())
                .Aggregate(0, (accumulator, hashCode) => accumulator + hashCode)
                * hashCodePart1 + hashCodePart2;
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            var result = new HashSet<Variable>();

            foreach (var expr in ComponentList)
            {
                result.UnionWith(expr.FindVariables(vst));
            }

            return result;
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            var result = new HashSet<string>();

            foreach (var expr in ComponentList)
            {
                result.UnionWith(expr.FindAllSkolemFunctionNames());
            }

            return result;
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            throw new NotImplementedException("Junction.ReplaceSkolemFunctionNames()");
        }

        public bool ContainsVariable(Variable v)
        {
            // Should we use VariableSearchType.Unbound instead?
            return FindVariables(VariableSearchType.All).Contains(v);
        }

        public abstract Junction MakeNewJunctionOfSameType();

        public abstract Junction MakeNewJunctionOfOtherType();

        public abstract Junction AsJunctionOfSameType(object o);

        public IExpression ApplySubstitution(Substitution sub)
        {
            Junction newJunct = MakeNewJunctionOfSameType();

            foreach (var expr in ComponentList)
            {
                IBooleanExpression newBoolExpr = expr.ApplySubstitution(sub) as IBooleanExpression;

                if (newBoolExpr == null)
                {
                    throw new Exception("Junction.ApplySubstitution() : The result of applying a substitution to an IBooleanExpression is not an IBooleanExpression.");
                }

                newJunct.ComponentList.Add(newBoolExpr);
            }

            return newJunct;
        }

        public IBooleanExpression Negate()
        {
            return new Negation(this);
        }

        public IBooleanExpression PushDownNegations()
        {
            var newJunct = MakeNewJunctionOfSameType();

            foreach (var component in ComponentList)
            {
                newJunct.ComponentList.Add(component.PushDownNegations());
            }

            return newJunct;
        }

        public IBooleanExpression UniquefyQuantifierVariables(HashSet<string> quantifierVariableNames, ref int nextQuantifierVariableNum)
        {
            var result = MakeNewJunctionOfSameType();

            foreach (var expr in ComponentList)
            {
                result.ComponentList.Add(expr.UniquefyQuantifierVariables(quantifierVariableNames, ref nextQuantifierVariableNum));
            }

            return result;
        }

        public IBooleanExpression ReplaceExistsWithSkolemFunctions(HashSet<string> skolemFuncNames, ArgumentList forAllVarsInScope, ref int nextSkolemFuncNum)
        {
            var result = MakeNewJunctionOfSameType();

            foreach (var expr in ComponentList)
            {
                result.ComponentList.Add(expr.ReplaceExistsWithSkolemFunctions(skolemFuncNames, forAllVarsInScope, ref nextSkolemFuncNum));
            }

            return result;
        }

        public IBooleanExpression DropUniversalQuantifiers()
        {
            var result = MakeNewJunctionOfSameType();

            foreach (var expr in ComponentList)
            {
                result.ComponentList.Add(expr.DropUniversalQuantifiers());
            }

            return result;
        }

        public abstract List<Clause> ToClausalForm();
    }

    #endregion

    #region Conjunction

    public class Conjunction : Junction
    {
        public Conjunction(IBooleanExpression expr1, IBooleanExpression expr2)
            : base(89, 59, " && ", expr1, expr2)
        {
        }

        public Conjunction()
            : this(null, null)
        {
        }

        public override Junction MakeNewJunctionOfSameType()
        {
            return new Conjunction();
        }

        public override Junction MakeNewJunctionOfOtherType()
        {
            return new Disjunction();
        }

        public override Junction AsJunctionOfSameType(object o)
        {
            return o as Conjunction;
        }

        public override List<Clause> ToClausalForm()
        {
            var result = new List<Clause>();

            /*
            foreach (var expr in ComponentList)
            {
                List<Clause> resultExpr = expr.ToClausalForm();

                foreach (var clause in resultExpr)
                {
                    result.Add(clause);
                }
            }
             */

            ComponentList.ForEach(expr => expr.ToClausalForm().ForEach(clause => result.Add(clause)));
            return result;
        }
    }

    #endregion

    #region Disjunction

    public class Disjunction : Junction
    {
        public Disjunction(IBooleanExpression expr1, IBooleanExpression expr2)
            : base(83, 61, " || ", expr1, expr2)
        {
        }

        public Disjunction()
            : this(null, null)
        {
        }

        public override Junction MakeNewJunctionOfSameType()
        {
            return new Disjunction();
        }

        public override Junction MakeNewJunctionOfOtherType()
        {
            return new Conjunction();
        }

        public override Junction AsJunctionOfSameType(object o)
        {
            return o as Disjunction;
        }

        public override List<Clause> ToClausalForm()
        {
            var result = new List<Clause>();
            bool firstIteration = true;

            foreach (var expr in ComponentList)
            {
                List<Clause> resultExpr = expr.ToClausalForm();
                //List<Clause> result2;    // = new List<Clause>();

                if (firstIteration)
                {
                    /*
                    foreach (var clause2 in resultExpr)
                    {
                        result2.Add(clause2);
                    }
                     */
                    result = resultExpr;    // new List<Clause>(resultExpr);
                    firstIteration = false;
                }
                else
                {
                    /*
                    result2 = new List<Clause>();

                    foreach (var clause in result)
                    {

                        foreach (var clause2 in resultExpr)
                        {
                            result2.Add(Clause.Join(clause, clause2));
                        }
                    }
                     */
                    /*
                    result2 =
                        from clause in result
                        from clause2 in resultExpr
                        select Clause.Join(clause, clause2);
                     */
                    // Create a Cartesian product of all clauses in result and resultExpr, and join the two clauses in each pair.
                    result = result.SelectMany(clause => resultExpr, (clause, clause2) => Clause.Join(clause, clause2)).ToList();
                }

                //result = result2;
            }

            return result;
        }
    }

    #endregion

    #region Quantifier

    public abstract class Quantifier : IBooleanExpression
    {
        public readonly Variable Var;
        public readonly IBooleanExpression BoolExpr;

        protected Quantifier(Variable v, IBooleanExpression be)
        {

            if (v == null)
            {
                throw new ArgumentNullException("v", "Quantifier constructor: v is null");
            }

            if (be == null)
            {
                throw new ArgumentNullException("be", "Quantifier constructor: be is null");
            }

            if (!be.ContainsVariable(v))
            {
                throw new Exception(string.Format("Quantifier constructor: The variable {0} does not occur in the Boolean expression {1}", v, be));
            }

            if (be.FindVariables(VariableSearchType.Bound).Contains(v))
            {
                throw new Exception(string.Format("Quantifier constructor: The variable {0} is already bound in the Boolean expression {1}", v, be));
            }

            Var = v;
            BoolExpr = be;
        }

        public abstract Quantifier MakeNewQuantifierOfSameType(Variable v, IBooleanExpression be);

        public abstract Quantifier MakeNewQuantifierOfOtherType(Variable v, IBooleanExpression be);

        public abstract Quantifier AsQuantifierOfSameType(object o);

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            HashSet<Variable> varSet;

            if (vst == VariableSearchType.Bound)
            {
                varSet = new HashSet<Variable>() { Var };
                //varSet.Add(Var);
            }
            else
            {
                // The Quantifier class constructor ensures that Var occurs in BoolExpr.
                varSet = BoolExpr.FindVariables(vst);

                if (vst == VariableSearchType.Unbound)
                {
                    varSet.Remove(Var);     // Var is bound.
                }
            }

            return varSet;
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            return BoolExpr.FindAllSkolemFunctionNames();
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            throw new NotImplementedException("Quantifier.ReplaceSkolemFunctionNames()");
        }

        public bool ContainsVariable(Variable v)
        {
            return !v.Equals(Var) && BoolExpr.ContainsVariable(v);
        }

        public abstract IExpression ApplySubstitution(Substitution sub);

        public abstract IBooleanExpression Negate();

        public IBooleanExpression PushDownNegations()
        {
            return MakeNewQuantifierOfSameType(Var, BoolExpr.PushDownNegations());
        }

        public IBooleanExpression UniquefyQuantifierVariables(HashSet<string> quantifierVariableNames, ref int nextQuantifierVariableNum)
        {
            Variable v;
            IBooleanExpression be;

            if (!quantifierVariableNames.Contains(Var.Name))
            {
                v = Var;
                be = BoolExpr;
            }
            else
            {
                Substitution sub = new Substitution();
                string varName;

                do
                {
                    varName = "var" + nextQuantifierVariableNum.ToString();
                    ++nextQuantifierVariableNum;
                }
                while (quantifierVariableNames.Contains(varName));

                v = new Variable(varName);
                sub.SubstitutionList[Var] = v;
                be = BoolExpr.ApplySubstitution(sub) as IBooleanExpression;

                if (be == null)
                {
                    throw new Exception("Quantifier.UniquefyQuantifierVariables() : The result of applying a substitution to an IBooleanExpression is not an IBooleanExpression.");
                }
            }

            quantifierVariableNames.Add(v.Name);
            return MakeNewQuantifierOfSameType(v, be.UniquefyQuantifierVariables(quantifierVariableNames, ref nextQuantifierVariableNum));
        }

        public abstract IBooleanExpression ReplaceExistsWithSkolemFunctions(HashSet<string> skolemFuncNames, ArgumentList forAllVarsInScope, ref int nextSkolemFuncNum);

        public abstract IBooleanExpression DropUniversalQuantifiers();

        public List<Clause> ToClausalForm()
        {
            throw new NotImplementedException("ToClausalForm() must not be called on a Quantifier");
        }
    }

    #endregion

    #region ForAll

    public class ForAll : Quantifier
    {
        public ForAll(Variable v, IBooleanExpression be)
            : base(v, be)
        {
        }

        public override Quantifier MakeNewQuantifierOfSameType(Variable v, IBooleanExpression be)
        {
            return new ForAll(v, be);
        }

        public override Quantifier MakeNewQuantifierOfOtherType(Variable v, IBooleanExpression be)
        {
            return new Exists(v, be);
        }

        public override Quantifier AsQuantifierOfSameType(object o)
        {
            return o as ForAll;
        }

        public override string ToString()
        {
            return string.Format("A {0} ({1})", Var, BoolExpr);
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            ForAll otherForAll = obj as ForAll;

            return otherForAll != null && Var.Equals(otherForAll.Var) && BoolExpr.Equals(otherForAll.BoolExpr);
        }

        public override int GetHashCode()
        {
            return Var.GetHashCode() * 211 + BoolExpr.GetHashCode() * 223 + 227;
        }

        public override IExpression ApplySubstitution(Substitution sub)
        {

            if (sub.SubstitutionList.ContainsKey(Var))
            {
                throw new Exception("ForAll.ApplySubstitution() : Cannot replace bound variable");
            }

            IBooleanExpression newBoolExpr = BoolExpr.ApplySubstitution(sub) as IBooleanExpression;

            if (newBoolExpr == null)
            {
                throw new Exception("ForAll.ApplySubstitution() : The result of applying a substitution to an IBooleanExpression is not an IBooleanExpression.");
            }

            return new ForAll(Var, newBoolExpr);
        }

        public override IBooleanExpression Negate()
        {
            return new Exists(Var, BoolExpr.Negate());
        }

        public override IBooleanExpression ReplaceExistsWithSkolemFunctions(HashSet<string> skolemFuncNames, ArgumentList forAllVarsInScope, ref int nextSkolemFuncNum)
        {
            var newForAllVarsInScope = new ArgumentList(forAllVarsInScope.ArgList);

            newForAllVarsInScope.ArgList.Add(Var);
            return BoolExpr.ReplaceExistsWithSkolemFunctions(skolemFuncNames, newForAllVarsInScope, ref nextSkolemFuncNum);
        }

        public override IBooleanExpression DropUniversalQuantifiers()
        {
            return BoolExpr.DropUniversalQuantifiers();
        }
    }

    #endregion

    #region Exists

    public class Exists : Quantifier
    {
        public Exists(Variable v, IBooleanExpression be)
            : base(v, be)
        {
        }

        public override Quantifier MakeNewQuantifierOfSameType(Variable v, IBooleanExpression be)
        {
            return new Exists(v, be);
        }

        public override Quantifier MakeNewQuantifierOfOtherType(Variable v, IBooleanExpression be)
        {
            return new ForAll(v, be);
        }

        public override Quantifier AsQuantifierOfSameType(object o)
        {
            return o as Exists;
        }

        public override string ToString()
        {
            return string.Format("E {0} ({1})", Var, BoolExpr);
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            Exists otherExists = obj as Exists;

            return otherExists != null && Var.Equals(otherExists.Var) && BoolExpr.Equals(otherExists.BoolExpr);
        }

        public override int GetHashCode()
        {
            return Var.GetHashCode() * 229 + BoolExpr.GetHashCode() * 233 + 239;
        }

        public override IExpression ApplySubstitution(Substitution sub)
        {

            if (sub.SubstitutionList.ContainsKey(Var))
            {
                throw new Exception("ForAll.ApplySubstitution() : Cannot replace bound variable");
            }

            IBooleanExpression newBoolExpr = BoolExpr.ApplySubstitution(sub) as IBooleanExpression;

            if (newBoolExpr == null)
            {
                throw new Exception("ForAll.ApplySubstitution() : The result of applying a substitution to an IBooleanExpression is not an IBooleanExpression.");
            }

            return new Exists(Var, newBoolExpr);
        }

        public override IBooleanExpression Negate()
        {
            return new ForAll(Var, BoolExpr.Negate());
        }

        public override IBooleanExpression ReplaceExistsWithSkolemFunctions(HashSet<string> skolemFuncNames, ArgumentList forAllVarsInScope, ref int nextSkolemFuncNum)
        {
            string newSkolemFunctionName;

            do
            {
                newSkolemFunctionName = "S" + nextSkolemFuncNum.ToString();
                ++nextSkolemFuncNum;
            }
            while (skolemFuncNames.Contains(newSkolemFunctionName));

            var newSkolemFunction = new SkolemFunction(newSkolemFunctionName, forAllVarsInScope);
            var sub = new Substitution();

            sub.SubstitutionList[Var] = newSkolemFunction;

            var newBoolExpr = BoolExpr.ApplySubstitution(sub) as IBooleanExpression;

            if (newBoolExpr == null)
            {
                throw new Exception("ForAll.ApplySubstitution() : The result of applying a substitution to an IBooleanExpression is not an IBooleanExpression.");
            }

            skolemFuncNames.Add(newSkolemFunctionName);
            return newBoolExpr.ReplaceExistsWithSkolemFunctions(skolemFuncNames, forAllVarsInScope, ref nextSkolemFuncNum); // Top down
        }

        public override IBooleanExpression DropUniversalQuantifiers()
        {
            throw new NotImplementedException("Exists.DropUniversalQuantifiers() : All existential quantifiers should have been dropped by now.");
        }
    }

    #endregion

    #region Literal

    public class Literal : IUnifiable
    {
        public Predicate Predicate { get; private set; }
        public bool IsNegated { get; private set; }

        public Literal(Predicate p, bool neg)
        {

            if (p == null)
            {
                throw new ArgumentNullException("p", "Literal constructor: p is null");
            }

            Predicate = p;
            IsNegated = neg;
        }

        public override string ToString()
        {
            var sb = new StringBuilder();

            if (IsNegated)
            {
                sb.Append("!");
            }

            sb.Append(Predicate.ToString());
            return sb.ToString();
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherLiteral = obj as Literal;

            return otherLiteral != null && IsNegated == otherLiteral.IsNegated && Predicate.Equals(otherLiteral.Predicate);
        }

        public override int GetHashCode()
        {
            return 79 * Predicate.GetHashCode() + (IsNegated ? 67 : 71);
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            return Predicate.FindVariables(vst);
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            return Predicate.FindAllSkolemFunctionNames();
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            var newPred = Predicate.ReplaceSkolemFunctionNames(dict) as Predicate;

            if (newPred == null)
            {
                throw new Exception("Literal.ReplaceSkolemFunctionNames() : Expected newPred to be a Predicate.");
            }

            return new Literal(newPred, IsNegated);
        }

        public bool ContainsVariable(Variable v)
        {
            //return FindAllVariables().Contains(v);
            return Predicate.ContainsVariable(v);
        }

        public virtual IExpression ApplySubstitution(Substitution sub)
        {
            var newPredicate = Predicate.ApplySubstitution(sub) as Predicate;

            if (newPredicate == null)
            {
                throw new Exception("Literal.ApplySubstitution() : The result of applying a substitution to a Predicate is not a Predicate.");
            }

            return new Literal(newPredicate, IsNegated);
        }

        public Substitution Unify(IUnifiable otherExpr)
        {
            var otherLiteral = otherExpr as Literal;

            if (otherLiteral == null || IsNegated != otherLiteral.IsNegated)
            {
                return null;
            }

            return Predicate.Unify(otherLiteral.Predicate);
        }
    }

    #endregion

    #region Clause

    public class Clause : IExpression   // Not IUnifiable because the literals may be in any order.
    {
        public readonly List<Literal> LiteralList = new List<Literal>();

        public Clause()
        {
        }

        public Clause(Literal leftLiteral, Clause rightClause)
        {

            if (leftLiteral == null)
            {
                throw new ArgumentNullException("leftLiteral", "Cannot add a null Literal to a Clause");
            }

            LiteralList.Add(leftLiteral);

            if (rightClause != null)
            {
                /*
                foreach (var literal in rightClause.LiteralList)
                {
                    LiteralList.Add(literal);
                }
                 */
                LiteralList.AddRange(rightClause.LiteralList);
            }
        }

        public Clause(Literal leftLiteral)
            : this(leftLiteral, null)
        {
        }

        public static List<Clause> ConvertBooleanExpressionToClausalForm(IBooleanExpression be)
        {
            // From Rich and Knight, "Artificial Intelligence", second edition, pages 145-147.
            // Step 1: Replace A -> B with !A || B : Already done by the parser.
            // Step 2: Reduce the scope of each negation (!) to a single term, using deMorgan's laws and the standard correspondences between quantifiers.
            be = be.PushDownNegations();
            // Step 3: Standardize variables so that each quantifier binds to a unique variable.
            int nextQuantifierVariableNum = 1;

            be = be.UniquefyQuantifierVariables(new HashSet<string>(), ref nextQuantifierVariableNum);
            // Step 4: Move all quantifiers to the left of the formula without changing their relative order.
            // Step 5: Eliminate existential quantifiers using Skolem functions.
            int nextSkolemFuncNum = 1;

            be = be.ReplaceExistsWithSkolemFunctions(be.FindAllSkolemFunctionNames(), new ArgumentList(), ref nextSkolemFuncNum);
            // Step 6: Drop the prefix (i.e. the remaining universal quantifiers).
            be = be.DropUniversalQuantifiers();
            // Step 7: Convert the matrix (i.e. the remaining part of the formula) into a conjunction of disjuncts.
            // Step 8: Create a separate clause corresponding to each conjunct.
            // Step 9: Rename the variables so that no two clauses make reference to the same variable (this is done before unification, by RenameSharedVariables()).
            return be.ToClausalForm();
        }

        public static Clause Join(Clause c1, Clause c2)
        {
            var result = new Clause();

            /*
            foreach (var literal in c1.LiteralList)
            {
                result.LiteralList.Add(literal);
            }

            foreach (var literal in c2.LiteralList)
            {
                result.LiteralList.Add(literal);
            }
             */
            result.LiteralList.AddRange(c1.LiteralList);
            result.LiteralList.AddRange(c2.LiteralList);
            return result;
        }

        public override string ToString()
        {
            /*
            var sb = new StringBuilder();
            var separator = string.Empty;

            foreach (var literal in LiteralList)
            {
                sb.Append(separator);
                separator = " || ";
                sb.Append(literal.ToString());
            }

            return sb.ToString();
             */
            return string.Join(" || ", LiteralList.Select(literal => literal.ToString()));
        }

        private bool LiteralsAreSubsetOf(Clause otherClause)
        {
            /*
            foreach (var literal in LiteralList)
            {

                if (!otherClause.LiteralList.Contains(literal))
                {
                    return false;
                }
            }

            return true;
             */
            return LiteralList.All(literal => otherClause.LiteralList.Contains(literal));
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherClause = obj as Clause;

            return otherClause != null
                && LiteralList.Count == otherClause.LiteralList.Count
                && LiteralsAreSubsetOf(otherClause)
                && otherClause.LiteralsAreSubsetOf(this);
        }

        public override int GetHashCode()
        {
            // Two clauses with the same literals in different orders will still have the same hash code.
            /*
            int sum = 0;

            foreach (var literal in LiteralList)
            {
                sum += literal.GetHashCode();
            }

            return 97 * sum + 53;
             */
            return LiteralList
                .Select(literal => literal.GetHashCode())
                .Aggregate(0, (accumulator, hashCode) => accumulator + hashCode)
                * 97 + 53;
        }

        private Clause RemoveLiteral(Literal literalToRemove)
        {
            var result = new Clause();

            foreach (Literal literal in LiteralList)
            {

                if (!literal.Equals(literalToRemove))
                {
                    result.LiteralList.Add(literal);
                }
            }

            return result;
        }

        private Substitution IsEquivalentTo_Helper(int i, Clause otherClause)
        {

            if (i >= LiteralList.Count)
            {

                if (otherClause.LiteralList.Count == 0)
                {
                    return new Substitution();
                }
                else
                {
                    return null;    // There are unmatched literals remaining in otherClause.
                }
            }

            Literal literal1 = LiteralList[i];

            foreach (Literal literal2 in otherClause.LiteralList)
            {
                Substitution sub = literal1.Unify(literal2);

                if (sub == null || !sub.ContainsOnlyVariables())
                {
                    continue;
                }

                Clause newOtherClause = otherClause.RemoveLiteral(literal2);  //.ApplySubstitution(sub);
                Substitution sub2 = IsEquivalentTo_Helper(i + 1, newOtherClause);

                if (sub2 != null)
                {
                    return sub.Compose(sub2);
                }
            }

            return null;
        }

        public bool IsEquivalentTo(Clause otherClause)  // I.e. is equal to, except for differences in variable names and literal order.
        {
            otherClause = otherClause.RenameSharedVariables(this);

            // We can use VariableSearchType.All below because there are no bound variables in a Clause.
            int numVarsInClause = FindVariables(VariableSearchType.All).Count;
            int numVarsInOtherClause = otherClause.FindVariables(VariableSearchType.All).Count;

            if (numVarsInClause != numVarsInOtherClause)
            {
                return false;
            }

            Substitution sub = IsEquivalentTo_Helper(0, otherClause);

            //return sub != null && sub.SubstitutionList.Count == FindAllVariables().Count && sub.IsOneToOne();
            return sub != null && sub.SubstitutionList.Count == numVarsInClause;
        }

        public bool IsContradiction()   // I.e. the Clause is always false
        {
            return LiteralList.Count == 0;
        }

        public bool IsTautology()       // I.e. the Clause is always true
        {

            for (int i = 0; i < LiteralList.Count; ++i)
            {
                Literal literal1 = LiteralList[i];

                for (int j = i + 1; j < LiteralList.Count; ++j)
                {
                    Literal literal2 = LiteralList[j];
                    //Substitution sub = literal1.Predicate.Unify(literal2.Predicate);

                    //if (sub != null && sub.SubstitutionList.Count == 0 && literal1.IsNegated != literal2.IsNegated)
                    if (literal1.Predicate.Equals(literal2.Predicate) && literal1.IsNegated != literal2.IsNegated)
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        public Clause RemoveRedundantLiterals()
        {
            var result = new Clause();

            for (int i = 0; i < LiteralList.Count; ++i)
            {
                Literal literal = LiteralList[i];
                bool keepLiteral = true;

                for (int j = i + 1; j < LiteralList.Count; ++j)
                {

                    if (literal.Equals(LiteralList[j]))
                    {
                        keepLiteral = false;
                        break;
                    }
                }

                if (keepLiteral)
                {
                    result.LiteralList.Add(literal);
                }
            }

            return result;
        }

        public IExpression ApplySubstitution(Substitution sub)
        {
            var result = new Clause();

            foreach (Literal literal in LiteralList)
            {
                var pred = literal.Predicate.ApplySubstitution(sub) as Predicate;

                if (pred == null)
                {
                    throw new Exception("Clause.ApplySubstitution() : The result of applying a substitution to a Predicate is not a Predicate.");
                }

                result.LiteralList.Add(new Literal(pred, literal.IsNegated));
            }

            return result;
        }

        public HashSet<Variable> FindVariables(VariableSearchType vst)
        {
            var result = new HashSet<Variable>();

            foreach (Literal literal in LiteralList)
            {
                result.UnionWith(literal.FindVariables(vst));
            }

            return result;
        }

        public HashSet<string> FindAllSkolemFunctionNames()
        {
            var result = new HashSet<string>();

            foreach (Literal literal in LiteralList)
            {
                result.UnionWith(literal.FindAllSkolemFunctionNames());
            }

            return result;
        }

        public IExpression ReplaceSkolemFunctionNames(Dictionary<string, string> dict)
        {
            var result = new Clause();

            foreach (Literal literal in LiteralList)
            {
                result.LiteralList.Add(literal.ReplaceSkolemFunctionNames(dict) as Literal);
            }

            return result;
        }

        public bool ContainsVariable(Variable v)
        {
            // We can use VariableSearchType.All below because there are no bound variables in a Clause.
            return FindVariables(VariableSearchType.All).Contains(v);
        }

        private Clause RenameSharedVariables(Clause otherClause)
        {
            var sub = new Substitution();
            // We can use VariableSearchType.All below because there are no bound variables in a Clause.
            HashSet<Variable> otherVars = otherClause.FindVariables(VariableSearchType.All);
            int varNum = 0;

            foreach (Variable v in otherVars)
            {

                if (ContainsVariable(v))
                {
                    Variable newVar = null;

                    do
                    {
                        ++varNum;
                        newVar = new Variable("var" + varNum.ToString());
                    }
                    while (ContainsVariable(newVar) || otherVars.Contains(newVar));

                    sub.SubstitutionList[v] = newVar;
                }
            }

            var newClause = ApplySubstitution(sub) as Clause;

            if (newClause == null)
            {
                throw new Exception("Clause.RenameSharedVariables() : The result of applying a substitution to an IUnifiable is not an IUnifiable.");
            }

            return newClause;
        }

        private Clause RenameSharedSkolemFunctionNames(Clause otherClause)
        {
            var dict = new Dictionary<string, string>();
            HashSet<string> skolemFunctionNames = FindAllSkolemFunctionNames();
            HashSet<string> otherSkolemFunctionNames = otherClause.FindAllSkolemFunctionNames();
            int nextSkolemFunctionNumber = 1;

            foreach (string skolemFunctionName in skolemFunctionNames)
            {

                if (otherSkolemFunctionNames.Contains(skolemFunctionName))
                {
                    string newSkolemFunctionName;

                    do
                    {
                        newSkolemFunctionName = "S" + nextSkolemFunctionNumber.ToString();
                        ++nextSkolemFunctionNumber;
                    }
                    while (skolemFunctionNames.Contains(newSkolemFunctionName) || otherSkolemFunctionNames.Contains(newSkolemFunctionName));

                    dict[skolemFunctionName] = newSkolemFunctionName;
                }
            }

            return ReplaceSkolemFunctionNames(dict) as Clause;
        }

        public List<Clause> Resolve(Clause otherClause)
        {
            var result = new List<Clause>();

            otherClause = otherClause.RenameSharedVariables(this);
            otherClause = otherClause.RenameSharedSkolemFunctionNames(this);

            for (int i = 0; i < LiteralList.Count; ++i)
            {
                Literal literal1 = LiteralList[i];

                for (int j = 0; j < otherClause.LiteralList.Count; ++j)
                {
                    Literal literal2 = otherClause.LiteralList[j];

                    if (literal1.IsNegated == literal2.IsNegated)
                    {
                        continue;
                    }

                    Substitution sub = literal1.Predicate.Unify(literal2.Predicate);

                    if (sub == null)
                    {
                        continue;
                    }

                    //Console.WriteLine("Resolve() : Substitution: " + sub.ToString());

                    var newClause = new Clause();

                    for (int k = 0; k < LiteralList.Count; ++k)
                    {

                        if (k != i)
                        {
                            var pred = LiteralList[k].Predicate.ApplySubstitution(sub) as Predicate;

                            if (pred == null)
                            {
                                throw new Exception("Resolve() : The result of applying a substitution to a Predicate is not a Predicate.");
                            }

                            newClause.LiteralList.Add(new Literal(pred, LiteralList[k].IsNegated));
                        }
                    }

                    for (int k = 0; k < otherClause.LiteralList.Count; ++k)
                    {

                        if (k != j)
                        {
                            var pred = otherClause.LiteralList[k].Predicate.ApplySubstitution(sub) as Predicate;

                            if (pred == null)
                            {
                                throw new Exception("Resolve() : The result of applying a substitution to a Predicate is not a Predicate.");
                            }

                            newClause.LiteralList.Add(new Literal(pred, otherClause.LiteralList[k].IsNegated));
                        }
                    }

                    result.Add(newClause);
                }
            }

            return result;
        }
    }

    #endregion
}
