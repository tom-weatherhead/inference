//#define CONSOLE_WRITELINE
//#define SUBSTITUTION_COMPOSITION_VERIFICATION
//#define NAME_EXPRESSION_EQUALITY
//#define SUBSTITUTION_KEY_COUNT_LIMIT

#define SUPPORT_USER_DEFINED_OPERATORS // Begun 2014/05/01.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
//using System.Threading.Tasks;
using Inference.Parser;
using Inference.Utilities; // For the extension method List<T>.AddRangeUnique()

namespace Inference.Interpreter.Prolog
{
    #region Enums

    public enum SolutionCollectionMode
    {
        None,
        FindAll,
        BagOfOrSetOf
    }

#if SUPPORT_USER_DEFINED_OPERATORS
    public enum OperatorType
    {
        // Infix operator types:
        xfx,
        xfy,
        yfx,
        // Prefix operator types:
        fx,
        fy,
        // Postfix operator types:
        xf,
        yf
    }
#endif

    #endregion

    #region Interfaces

    public interface IPrologExpression
    {
        HashSet<PrologVariable> FindBindingVariables();     // Finds only binding variables; ignores non-binding variables such as _
        List<PrologVariable> GetListOfBindingVariables();   // As above, but this returns a list, which is ordered, and contains no duplicates
        bool ContainsVariable(PrologVariable v);
        IPrologExpression ApplySubstitution(PrologSubstitution substitution);
        PrologSubstitution Unify(IPrologExpression otherExpr);
        bool IsGround { get; }  // True iff the expression contains no variables.
        IPrologNumber EvaluateToNumber();
    }

    public interface IPrologNumber : IPrologExpression
    {
        int ToInteger();
        double ToDouble();
    }

    #endregion

    #region PrologIntegerLiteral

    public class PrologIntegerLiteral : IPrologNumber
    {
        public readonly int Value;

        public PrologIntegerLiteral(int value)
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

            var otherIntLit = obj as PrologIntegerLiteral;

            return otherIntLit != null && Value == otherIntLit.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {
            return new HashSet<PrologVariable>();
        }

        public List<PrologVariable> GetListOfBindingVariables()
        {
            return new List<PrologVariable>();
        }

        public bool ContainsVariable(PrologVariable v)
        {
            return false;
        }

        public IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            return this;
        }

        public PrologSubstitution Unify(IPrologExpression otherExpr)
        {

            if (Equals(otherExpr))  // Do not use "if (this == otherExpr)", which just compares references.
            {
                return new PrologSubstitution();
            }
            else if (otherExpr is PrologVariable)
            {
                return otherExpr.Unify(this);
            }

            return null;    // The PrologIntegerLiteral and the IPrologExpression are not unifiable.
        }

        public bool IsGround
        {
            get
            {
                return true;
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            return this;
        }

        public int ToInteger()
        {
            return Value;
        }

        public double ToDouble()
        {
            return Convert.ToDouble(Value);
        }
    }

    #endregion

    #region PrologFloatLiteral

    public class PrologFloatLiteral : IPrologNumber
    {
        public readonly double Value;

        public PrologFloatLiteral(double value)
        {
            Value = value;
        }

        public override string ToString()
        {
            // E.g. If d == 13.0, we want the string to be "13.0" rather than just "13", so that we can distinguish it from an integer.
            // Note from Lisp: d == (tan (/ pi 4)) is a good test case; it should yield "1.0", not "1".
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

            var otherFloatLit = obj as PrologFloatLiteral;

            return otherFloatLit != null && Value == otherFloatLit.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {
            return new HashSet<PrologVariable>();
        }

        public List<PrologVariable> GetListOfBindingVariables()
        {
            return new List<PrologVariable>();
        }

        public bool ContainsVariable(PrologVariable v)
        {
            return false;
        }

        public IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            return this;
        }

        public PrologSubstitution Unify(IPrologExpression otherExpr)
        {

            if (Equals(otherExpr))  // Do not use "if (this == otherExpr)", which just compares references.
            {
                return new PrologSubstitution();
            }
            else if (otherExpr is PrologVariable)
            {
                return otherExpr.Unify(this);
            }

            return null;    // The PrologIntegerLiteral and the IPrologExpression are not unifiable.
        }

        public bool IsGround
        {
            get
            {
                return true;
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            return this;
        }

        public int ToInteger()
        {
            return Convert.ToInt32(Math.Floor(Value));
        }

        public double ToDouble()
        {
            return Value;
        }
    }

    #endregion

    #region PrologStringLiteral

    // This class is deprecated.  A Prolog string is a list of integer character codes.

    public class PrologStringLiteral : IPrologExpression
    {
        public readonly string Value;

        public PrologStringLiteral(string value)
        {
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

            PrologStringLiteral otherStrLit = obj as PrologStringLiteral;

            return otherStrLit != null && Value == otherStrLit.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {
            return new HashSet<PrologVariable>();
        }

        public List<PrologVariable> GetListOfBindingVariables()
        {
            return new List<PrologVariable>();
        }

        public bool ContainsVariable(PrologVariable v)
        {
            return false;
        }

        public IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            return this;
        }

        public PrologSubstitution Unify(IPrologExpression otherExpr)
        {

            if (Equals(otherExpr))  // Do not use "if (this == otherExpr)", which just compares references.
            {
                return new PrologSubstitution();
            }
            else if (otherExpr is PrologVariable)
            {
                return otherExpr.Unify(this);
            }

            return null;    // The PrologIntegerLiteral and the IPrologExpression are not unifiable.
        }

        public bool IsGround
        {
            get
            {
                return true;
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            return null;
        }
    }

    #endregion

    #region PrologNameBase

    public class PrologNameBase
    {
        public readonly string Name;

        protected PrologNameBase(string name)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A PrologNameBase cannot have a null or empty name");
            }

            Name = name;
        }

        public override string ToString()
        {
            return Name;
        }

        public override bool Equals(object obj)
        {

            if (obj == null || !GetType().Equals(obj.GetType()))
            {
                return false;
            }

            var otherNameBase = obj as PrologNameBase;

            return otherNameBase != null && Name == otherNameBase.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }
    }

    #endregion

    #region PrologFunctor

    public class PrologFunctor : PrologNameBase
    {
        public PrologFunctor(string name)
            : base(name)
        {
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

#if NAME_EXPRESSION_EQUALITY
            var otherNameExpr = obj as PrologNameExpression<PrologFunctor>;

            if (otherNameExpr != null)
            {
                return otherNameExpr.Equals(this);
            }
#endif

            /*
            var otherFunctor = obj as PrologFunctor;

            // == is used to test Name equality because Name is a string.
            return otherFunctor != null && Name == otherFunctor.Name;
             */
            return base.Equals(obj);
        }

        public override int GetHashCode()   // We override this function in order to satisfy Visual Studio, since we overrode this class's Equals().
        {
            return base.GetHashCode() + 1;
        }
    }

    #endregion

    #region PrologPredicate

    public class PrologPredicate : PrologNameBase
    {
        public PrologPredicate(string name)
            : base(name)
        {
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

#if NAME_EXPRESSION_EQUALITY
            var otherNameExpr = obj as PrologNameExpression<PrologPredicate>;

            if (otherNameExpr != null)
            {
                return otherNameExpr.Equals(this);
            }
#endif

            /*
            var otherPredicate = obj as PrologPredicate;

            // == is used to test Name equality because Name is a string.
            return otherPredicate != null && Name == otherPredicate.Name;
             */
            return base.Equals(obj);
        }

        public override int GetHashCode()   // We override this function in order to satisfy Visual Studio, since we overrode this class's Equals().
        {
            return base.GetHashCode() + 2;
        }
    }

    #endregion

    #region PrologVariable

    public class PrologVariable : IPrologExpression
    {
        public readonly string Name;

        public PrologVariable(string name)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new ArgumentNullException("name", "A PrologVariable cannot have a null or empty name");
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

            PrologVariable otherVar = obj as PrologVariable;

            // We can compare the Name members with == because Name is a string.
            return otherVar != null && Name == otherVar.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        public bool IsNonBinding
        {
            get
            {
                // The following supports non-binding variables such as _ and _Foo .
                // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_3.html

                // This may contradict http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse2 ,
                // which implies that only _ is non-binding, and that any other variable that begins with _ is a normal, binding variable.
                return Name.StartsWith("_");
            }
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {

            if (IsNonBinding) // This allows the test Prolog2Parser_Fixture.DoNotRenameNonBindingVariablesTest() to pass.
            {
                return new HashSet<PrologVariable>();   // Ignore non-binding variables; we don't want to rename them to binding variables.
            }

            return new HashSet<PrologVariable>() { this };
        }

        public List<PrologVariable> GetListOfBindingVariables()
        {
            var result = new List<PrologVariable>();

            if (!IsNonBinding)
            {
                result.Add(this);
            }

            return result;
        }

        public bool ContainsVariable(PrologVariable v)
        {
            return Equals(v);
        }

        public IPrologExpression ApplySubstitution(PrologSubstitution sub)
        {

            if (sub.SubstitutionList.ContainsKey(this))
            {
                return sub.SubstitutionList[this];
            }

            return this;
        }

        public PrologSubstitution Unify(IPrologExpression otherExpr)
        {
            var otherVariable = otherExpr as PrologVariable;

            if (Equals(otherExpr) || IsNonBinding
                // 2014/03/13 : Don't add the binding { X = _ } to any substitution.
                // But what about a binding such as { X = foo(_) } ?
                || (otherVariable != null && otherVariable.IsNonBinding))
            {
                return new PrologSubstitution();
            }
            else if (otherExpr is PrologGoal ||
                otherExpr is PrologClause ||
                otherExpr.ContainsVariable(this))  // This is the "occurs" check.
            {
                return null;    // This PrologVariable and the IPrologExpression are not unifiable.
            }
            else
            {
                return new PrologSubstitution(this, otherExpr);
            }
        }

        public bool IsGround
        {
            get
            {
                return false;
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            return null;
        }
    }

    #endregion

    #region PrologNameExpression<T>

    public class PrologNameExpression<T> : IPrologExpression where T : PrologNameBase
    {
        public readonly GrammarSelector gs;
        public readonly T Name;
        public readonly List<IPrologExpression> ExpressionList;
        public bool DCGDoNotAddExtraArguments = false; // Part of Definite Clause Grammar support.

        public PrologNameExpression(GrammarSelector gs, T name, List<IPrologExpression> expressionList = null)
        {
            this.gs = gs;
            Name = name;
            ExpressionList = expressionList ?? new List<IPrologExpression>();
        }

        private string ListToString(IPrologExpression expr)
        {
            var functorExpression = expr as PrologNameExpression<PrologFunctor>;

            if (functorExpression != null && functorExpression.Name.Name == "[]" && functorExpression.ExpressionList.Count == 0)
            {
                return string.Empty;
            }
            else if (functorExpression != null && functorExpression.Name.Name == "." && functorExpression.ExpressionList.Count == 2)
            {
                return string.Format(", {0}{1}", functorExpression.ExpressionList[0], ListToString(functorExpression.ExpressionList[1]));
            }
            else
            {
                return string.Format(" | {0}", expr);
            }
        }

        private string SequenceToString(IPrologExpression expr)
        {
            var functorExpression = expr as PrologNameExpression<PrologFunctor>;

            if (functorExpression != null && functorExpression.Name.Name == "consSeq" && functorExpression.ExpressionList.Count == 2)
            {
                return string.Format("{0}, {1}", functorExpression.ExpressionList[0], SequenceToString(functorExpression.ExpressionList[1]));
            }
            else
            {
                return expr.ToString();
            }
        }

        public override string ToString()
        {
            var nameAsString = Name.ToString();
            var isProlog2FunctorExpression = gs == GrammarSelector.Prolog2 && Name is PrologFunctor;

            if (ExpressionList.Count == 0)
            {
#if DEAD_CODE
                if (isProlog2FunctorExpression && nameAsString == "nil")
                {
                    return "[]";
                }
#endif
                return nameAsString;
            }
            else if (gs == GrammarSelector.Prolog)
            {
                return string.Format("({0} {1})", Name, string.Join(" ", ExpressionList.Select(expr => expr.ToString())));
            }
            else
            {

                if (isProlog2FunctorExpression && ExpressionList.Count == 2)
                {

                    if (nameAsString == ".")
                    {
                        return string.Format("[{0}{1}]", ExpressionList[0], ListToString(ExpressionList[1]));
                    }
                    else if (nameAsString == "consSeq")
                    {
#if DEAD_CODE
                        return string.Format("{0}, {1}", ExpressionList[0], SequenceToString(ExpressionList[1]));
#else
                        // ThAW 2014/03/28 : I added the brackets here because without them, ?- X = [(1, 2), (3, 4)], print(X). yielded [1, 2, 3, 4],
                        // which was misleading.
                        return string.Format("({0}, {1})", ExpressionList[0], SequenceToString(ExpressionList[1]));
#endif
                    }
                }

                return string.Format("{0}({1})", Name, string.Join(", ", ExpressionList.Select(expr => expr.ToString())));
            }
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

#if NAME_EXPRESSION_EQUALITY
            var otherName = obj as T;

            if (otherName != null)
            {
                return ExpressionList.Count == 0 && Name.Equals(otherName);
            }
#endif

            var otherNameExpr = obj as PrologNameExpression<T>;

            if (otherNameExpr == null || !Name.Equals(otherNameExpr.Name) || ExpressionList.Count != otherNameExpr.ExpressionList.Count)
            {
                return false;
            }

            for (var i = 0; i < ExpressionList.Count; ++i)
            {

                if (!ExpressionList[i].Equals(otherNameExpr.ExpressionList[i]))
                {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode()
        {
            return ExpressionList
                .Select(expr => expr.GetHashCode())
                .Aggregate(Name.GetHashCode(), (accumulator, hashCode) => accumulator * 101 + hashCode);
        }

        public virtual HashSet<PrologVariable> FindBindingVariables()
        {
            var result = new HashSet<PrologVariable>();

            foreach (var expr in ExpressionList)
            {
                result.UnionWith(expr.FindBindingVariables());
            }

            return result;
        }

        public virtual List<PrologVariable> GetListOfBindingVariables()
        {
            var result = new List<PrologVariable>();

            foreach (var expr in ExpressionList)
            {
                result.AddRangeUnique(expr.GetListOfBindingVariables());
            }

            return result;
        }

        public virtual bool ContainsVariable(PrologVariable v)
        {
            return ExpressionList.Any(expr => expr.ContainsVariable(v));
        }

        public virtual IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            return new PrologNameExpression<T>(gs, Name, ExpressionList.Select(expr => expr.ApplySubstitution(substitution)).ToList());
        }

        public virtual PrologSubstitution Unify(IPrologExpression otherExpr)
        {

            if (otherExpr is PrologVariable)
            {
                return otherExpr.Unify(this);
            }

            if (!GetType().Equals(otherExpr.GetType()))
            {
                // A PrologFunctorExpression can unify with a PrologFunctorExpression;
                // a PrologGoal can unify with a PrologGoal,
                // but a PrologFunctorExpression cannot unify with a PrologGoal.
                return null;
            }

            var otherNameExpression = otherExpr as PrologNameExpression<T>;

            if (!Name.Equals(otherNameExpression.Name) || ExpressionList.Count != otherNameExpression.ExpressionList.Count)
            {
                return null;
            }

            var substitution = new PrologSubstitution();

            for (var i = 0; i < ExpressionList.Count; ++i)
            {
                var newExpr1 = ExpressionList[i].ApplySubstitution(substitution);
                var newExpr2 = otherNameExpression.ExpressionList[i].ApplySubstitution(substitution);
                var substitution2 = newExpr1.Unify(newExpr2);

                if (substitution2 == null)
                {
                    return null;
                }

                substitution = substitution.Compose(substitution2);
            }

            return substitution;
        }

        public virtual bool IsGround
        {
            get
            {
                return ExpressionList.All(expr => expr.IsGround);
            }
        }

        private static IPrologNumber EvaluateUnaryOperatorToNumber(PrologNameExpression<PrologFunctor> thisFunctorExpression)
        {
            var arg1Evaluated = thisFunctorExpression.ExpressionList[0].EvaluateToNumber();

            if (arg1Evaluated == null)
            {
                return null;
            }

            if (arg1Evaluated is PrologIntegerLiteral)
            {
                var arg1Value = arg1Evaluated.ToInteger();
                int result;

                switch (thisFunctorExpression.Name.Name)
                {
                    case "+":
                        result = arg1Value;
                        break;

                    case "-":
                        result = -arg1Value;
                        break;

                    default:
                        return null;
                }

                return new PrologIntegerLiteral(result);
            }
            else
            {
                var arg1Value = arg1Evaluated.ToDouble();
                double result;

                switch (thisFunctorExpression.Name.Name)
                {
                    case "+":
                        result = arg1Value;
                        break;

                    case "-":
                        result = -arg1Value;
                        break;

                    default:
                        return null;
                }

                return new PrologFloatLiteral(result);
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            var thisFunctorExpression = this as PrologNameExpression<PrologFunctor>;

            if (thisFunctorExpression == null)
            {
                return null;
            }
            else if (ExpressionList.Count == 1)
            {
                return EvaluateUnaryOperatorToNumber(thisFunctorExpression);
            }
            else if (ExpressionList.Count != 2)
            {
                return null;
            }

            var arg1Evaluated = ExpressionList[0].EvaluateToNumber();
            var arg2Evaluated = ExpressionList[1].EvaluateToNumber();

            if (arg1Evaluated == null || arg2Evaluated == null)
            {
                return null;
            }

            if (arg1Evaluated is PrologIntegerLiteral && arg2Evaluated is PrologIntegerLiteral)
            {
                var arg1Value = arg1Evaluated.ToInteger();
                var arg2Value = arg2Evaluated.ToInteger();
                int result;

                switch (Name.Name)
                {
                    case "+":
                        result = arg1Value + arg2Value;
                        break;

                    case "-":
                        result = arg1Value - arg2Value;
                        break;

                    case "*":
                        result = arg1Value * arg2Value;
                        break;

                    case "/":
                        result = arg1Value / arg2Value;
                        break;

                    case "mod":
                        result = arg1Value % arg2Value;
                        break;

                    default:
                        return null;
                }

                return new PrologIntegerLiteral(result);
            }
            else
            {
                var arg1Value = arg1Evaluated.ToDouble();
                var arg2Value = arg2Evaluated.ToDouble();
                double result;

                switch (Name.Name)
                {
                    case "+":
                        result = arg1Value + arg2Value;
                        break;

                    case "-":
                        result = arg1Value - arg2Value;
                        break;

                    case "*":
                        result = arg1Value * arg2Value;
                        break;

                    case "/":
                        result = arg1Value / arg2Value;
                        break;

                    case "mod":
                        result = arg1Value % arg2Value;
                        break;

                    default:
                        return null;
                }

                return new PrologFloatLiteral(result);
            }
        }

        public PrologGoal ToGoal()
        {
            return new PrologGoal(gs, new PrologPredicate(Name.Name), ExpressionList);
        }
    }

    #endregion

//#if !SUPPORT_USER_DEFINED_OPERATORS

    #region CaretList

    public class CaretList : IPrologExpression // Deprecated.  Objects of this class are only created by the Prolog2 LL(1) grammar.
    {
        public readonly List<PrologVariable> VariableList;
        public readonly PrologNameExpression<PrologFunctor> FunctorExpression;

        public CaretList(List<PrologVariable> vl, PrologNameExpression<PrologFunctor> fe)
        {
            VariableList = vl;
            FunctorExpression = fe;
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {
            var result = new HashSet<PrologVariable>(VariableList);

            result.UnionWith(FunctorExpression.FindBindingVariables());
            return result;
        }

        public List<PrologVariable> GetListOfBindingVariables()
        {
            var result = new List<PrologVariable>();

#if DEAD_CODE
            foreach (var v in VariableList)
            {

                if (!result.Contains(v))
                {
                    result.Add(v);
                }
            }
#else
            result.AddRangeUnique(VariableList);
#endif
            result.AddRangeUnique(FunctorExpression.GetListOfBindingVariables());
            return result;
        }

        public bool ContainsVariable(PrologVariable v)
        {
            return VariableList.Contains(v) || FunctorExpression.ContainsVariable(v);
        }

        public IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            var newVariableList = new List<PrologVariable>();

            foreach (var v in VariableList)
            {
                var new_v = v.ApplySubstitution(substitution) as PrologVariable;

                if (new_v == null)
                {
                    throw new Exception("CaretList.ApplySubstitution() : The variable list was compromised.");
                }

                newVariableList.Add(new_v);
            }

            return new CaretList(newVariableList, (PrologNameExpression<PrologFunctor>)(FunctorExpression.ApplySubstitution(substitution)));
        }

        public PrologSubstitution Unify(IPrologExpression otherExpr)
        {
            var otherExprAsVariable = otherExpr as PrologVariable;
            var otherExprAsCaretList = otherExpr as CaretList;

            if (otherExprAsVariable != null)
            {
                return otherExprAsVariable.Unify(this);
            }
            else if (otherExprAsCaretList == null || VariableList.Count != otherExprAsCaretList.VariableList.Count)
            {
                return null;
            }

            var unifier = new PrologSubstitution();

            for (var i = 0; i < VariableList.Count; ++i)
            {
                var expr1 = VariableList[i].ApplySubstitution(unifier);
                var expr2 = otherExprAsCaretList.VariableList[i].ApplySubstitution(unifier);
                var subst = expr1.Unify(expr2);

                if (subst == null)
                {
                    return null;
                }

                unifier = unifier.Compose(subst);
            }

            var fe1 = FunctorExpression.ApplySubstitution(unifier);
            var fe2 = otherExprAsCaretList.FunctorExpression.ApplySubstitution(unifier);
            var substLast = fe1.Unify(fe2);

            if (substLast == null)
            {
                return null;
            }

            return unifier.Compose(substLast);
        }

        public bool IsGround
        {
            get
            {
                // This will always return false, since VariableList will never be empty.
                return VariableList.Count == 0 && FunctorExpression.IsGround;
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            return null;
        }
    }

    #endregion

//#endif

    #region ExpressionListAsKey

    // Hashable (for use as a Dictionary key) and comparable (for List sorting).

    public class ExpressionListAsKey : IComparable<ExpressionListAsKey>
    {
        public readonly List<IPrologExpression> ExpressionList;

        public ExpressionListAsKey(List<IPrologExpression> el)
        {
            ExpressionList = el;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherExpressionListAsKey = obj as ExpressionListAsKey;

            if (otherExpressionListAsKey == null || ExpressionList.Count != otherExpressionListAsKey.ExpressionList.Count)
            {
                return false;
            }

            for (var i = 0; i < ExpressionList.Count; ++i)
            {

                if (!ExpressionList[i].Equals(otherExpressionListAsKey.ExpressionList[i]))
                {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode()
        {
            var result = 0;

            foreach (var expr in ExpressionList)
            {
                result = result * 101 + expr.GetHashCode();
            }

            return result;
        }

        public int CompareTo(ExpressionListAsKey otherExpressionListAsKey)
        {

            if (ExpressionList.Count != otherExpressionListAsKey.ExpressionList.Count)
            {
                throw new Exception("ExpressionListAsKey.CompareTo() : Expression lists have different lengths.");
            }

            for (var i = 0; i < ExpressionList.Count; ++i)
            {
                var comp = ExpressionList[i].ToString().CompareTo(otherExpressionListAsKey.ExpressionList[i].ToString());

                if (comp != 0)
                {
                    return comp;
                }
            }

            return 0;
        }
    }

    #endregion

    #region SetOfComparer

    // 2014/04/15 : This is a rather basic Comparer for IPrologExpression; e.g. it will not compare lists of numbers properly.

    public class SetOfComparer : Comparer<IPrologExpression>
    {
        public override int Compare(IPrologExpression x, IPrologExpression y)
        {
            var x_number = x as IPrologNumber;
            var y_number = y as IPrologNumber;

            if (x_number != null && y_number != null)
            {
                return x_number.ToDouble().CompareTo(y_number.ToDouble());
            }

            return x.ToString().CompareTo(y.ToString());
        }
    }

    #endregion

    #region PrologFileReader

    public class PrologFileReader : IPrologExpression
    {
        private StreamReader sr = null;
        private readonly PrologGlobalInfo globalInfo;
        private List<Token> tokenBuffer = null;

        public PrologFileReader(string filename, PrologGlobalInfo globalInfo)
        {
            this.globalInfo = globalInfo;

            if (string.IsNullOrEmpty(filename))
            {
                throw new Exception("PrologFileReader constructor: The filename is null or empty.");
            }

            if (!string.IsNullOrEmpty(globalInfo.PathToDefaultDirectory) && filename.IndexOf('\\') < 0)
            {
                filename = Path.Combine(globalInfo.PathToDefaultDirectory, filename);
            }

            try
            {
                sr = File.OpenText(filename);
            }
            catch
            {
                // If any exception was thrown, sr will be null.
            }
        }

        public override bool Equals(object obj)
        {
            return object.ReferenceEquals(this, obj);
        }

        public override int GetHashCode()
        {
            return (sr != null) ? sr.GetHashCode() : 0;
        }

        public override string ToString()
        {
            return "__file_reader__";   // Warning: If we write this out and then read it in again, it will be interpreted as a variable.
        }

        public bool IsNull
        {
            get
            {
                return sr == null;
            }
        }

        public void Close()
        {

            if (sr != null)
            {
                sr.Dispose();
                sr = null;
            }

            tokenBuffer = null;
        }

        private void SkipWhitespace()
        {
            
            if (sr == null || tokenBuffer != null)
            {
                return;
            }

            for (; ;)
            {
                
                if (sr.EndOfStream)
                {
                    Close();
                    return;
                }

                var lineOfText = sr.ReadLine();

                if (lineOfText == null)
                {
                    Close();
                    return;
                }

                var tokenList = globalInfo.tokenizer.Tokenize(lineOfText);

                if (tokenList.Count > 1) // The list will always contain an EOF token.
                {
                    tokenBuffer = tokenList;
                    return;
                }
            }
        }

        public bool IsEOF // Only use this property if we are reading atoms via Read(), since it skips whitespace.
        {
            get
            {
                SkipWhitespace();
                return tokenBuffer == null && (sr == null || sr.EndOfStream);
            }
        }

        public PrologNameExpression<PrologFunctor> Read( /* StringBuilder sbOutput */ )
        {
            SkipWhitespace();

            if ( /* IsEOF || */ tokenBuffer == null)
            {
                //sbOutput.AppendLine("Read error 1");
                return null;
            }

            List<Token> tokenListToParse = null;

            for (var i = 0; i < tokenBuffer.Count; ++i)
            {

                if (tokenBuffer[i].TokenType == TokenType.T_Dot)
                {
                    tokenListToParse = tokenBuffer.Take(i + 1).ToList();
                    tokenListToParse.Add(new Token(TokenType.T_EOF, "EOF", 0, 0));
                    tokenBuffer = tokenBuffer.Skip(i + 1).ToList();
                    break;
                }
            }

            if (tokenListToParse == null) // If no dot was found
            {
                //sbOutput.AppendLine("Read error 2");
                return null;
            }

            if (tokenBuffer.All(t => t.TokenType == TokenType.T_EOF))
            {
                //SkipWhitespace();
                tokenBuffer = null;
            }

#if DEAD_CODE
            if (tokenListToParse.Count != 3)
            {
                sbOutput.AppendFormat("Read error 4 (token count is {0})", tokenListToParse.Count);
            }
            else if (tokenListToParse[1].TokenType != TokenType.T_Dot || tokenListToParse[2].TokenType != TokenType.T_EOF)
            {
                sbOutput.AppendLine("Read error 5");
            }
#endif

            PrologClause clause = null;

            try
            {
                clause = globalInfo.parser.Parse(tokenListToParse) as PrologClause;
            }
            catch
            {
            }

            if (clause == null || clause.Rhs.Count > 0 || clause.Lhs.ExpressionList.Count > 0)
            {
                //sbOutput.AppendLine("Read error 3");
                return null;
            }

            return new PrologNameExpression<PrologFunctor>(globalInfo.gs, new PrologFunctor(clause.Lhs.Name.Name));
        }

        public int GetCode()
        {

            if (sr == null || sr.EndOfStream)
            {
                return -1;
            }

            return sr.Read();
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {
            return new HashSet<PrologVariable>();
        }

        public List<PrologVariable> GetListOfBindingVariables()
        {
            return new List<PrologVariable>();
        }

        public bool ContainsVariable(PrologVariable v)
        {
            return false;
        }

        public IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            return this;
        }

        public PrologSubstitution Unify(IPrologExpression otherExpr)
        {
            var otherVariable = otherExpr as PrologVariable;

            if (Equals(otherExpr))
            {
                return new PrologSubstitution();
            }
            else if (otherVariable != null)
            {
                return otherVariable.Unify(this);
            }
            else
            {
                return null;
            }
        }

        public bool IsGround
        {
            get
            {
                return true;    // True iff the expression contains no variables.
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            return null;
        }
    }

    #endregion

    #region PrologFileWriter

    public class PrologFileWriter : IPrologExpression
    {
        private StreamWriter sw = null;

        public PrologFileWriter(string filename, bool append, PrologGlobalInfo globalInfo)
        {

            if (string.IsNullOrEmpty(filename))
            {
                throw new Exception("PrologFileWriter constructor: The filename is null or empty.");
            }

            if (!string.IsNullOrEmpty(globalInfo.PathToDefaultDirectory) && filename.IndexOf('\\') < 0)
            {
                filename = Path.Combine(globalInfo.PathToDefaultDirectory, filename);
            }

            try
            {
                sw = new StreamWriter(filename, append);
            }
            catch
            {
                // If any exception was thrown, sw will be null.
            }
        }

        public override bool Equals(object obj)
        {
            return object.ReferenceEquals(this, obj);
        }

        public override int GetHashCode()
        {
            return (sw != null) ? sw.GetHashCode() : 0;
        }

        public override string ToString()
        {
            return "__file_writer__";   // Warning: If we write this out and then read it in again, it will be interpreted as a variable.
        }

        public bool IsNull
        {
            get
            {
                return sw == null;
            }
        }

        public void Close()
        {

            if (sw != null)
            {
                sw.Dispose();
                sw = null;
            }
        }

        public bool Write(IPrologExpression expr)
        {

            if (sw == null)
            {
                return false;
            }

            sw.Write(expr.ToString());
            return true;
        }

        public bool Tab(int amount)
        {

            if (sw == null || amount <= 0)
            {
                return false;
            }

            for (var i = 0; i < amount; ++i)
            {
                sw.Write(' '); // Not '\t'.  See http://www.swi-prolog.org/pldoc/man?predicate=tab/1
            }

            return true;
        }

        public bool NewLine()
        {

            if (sw == null)
            {
                return false;
            }

            sw.WriteLine();
            return true;
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {
            return new HashSet<PrologVariable>();
        }

        public List<PrologVariable> GetListOfBindingVariables()
        {
            return new List<PrologVariable>();
        }

        public bool ContainsVariable(PrologVariable v)
        {
            return false;
        }

        public IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            return this;
        }

        public PrologSubstitution Unify(IPrologExpression otherExpr)
        {
            var otherVariable = otherExpr as PrologVariable;

            if (Equals(otherExpr))
            {
                return new PrologSubstitution();
            }
            else if (otherVariable != null)
            {
                return otherVariable.Unify(this);
            }
            else
            {
                return null;
            }
        }

        public bool IsGround
        {
            get
            {
                return true;    // True iff the expression contains no variables.
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            return null;
        }
    }

    #endregion

    #region PrologGoal

    public class PrologGoal : PrologNameExpression<PrologPredicate>
    {
        //public bool DCGDoNotAddExtraArguments = false; // Part of Definite Clause Grammar support.

        public PrologGoal(GrammarSelector gsParam, PrologPredicate predicate, List<IPrologExpression> expressionList)
            : base(gsParam, predicate, expressionList)
        {
        }

        public override IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            return new PrologGoal(gs, Name, ExpressionList.Select(expr => expr.ApplySubstitution(substitution)).ToList());
        }

        public bool IsCut
        {
            get
            {
                return Name.Name == "!";
            }
        }

#if DEAD_CODE
        public bool IsIsomorphicTo(PrologGoal otherGoal)
        {
            // TODO: If we are going to use this function, we should ensure that the two goals have no variables in common.
            var unifier = Unify(otherGoal);

            return unifier != null && unifier.IsOneToOne;
        }
#endif
    }

    #endregion

    #region PrologClause

    public class PrologClause : IPrologExpression
    {
        public readonly PrologGoal Lhs;
        public readonly List<PrologGoal> Rhs;

        public PrologClause(PrologGoal lhs, List<PrologGoal> rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }

        public override string ToString()
        {
            //return string.Format("{0} <= {1}", Lhs, string.Join(" ", Rhs));

            // Prolog2 clause format:

            if (Rhs.Count == 0)
            {
                return string.Format("{0}.", Lhs);
            }
            else
            {
                return string.Format("{0} :- {1}.", Lhs, string.Join(", ", Rhs));
            }
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherClause = obj as PrologClause;

            if (otherClause == null || !Lhs.Equals(otherClause.Lhs) || Rhs.Count != otherClause.Rhs.Count)
            {
                return false;
            }

            for (var i = 0; i < Rhs.Count; ++i)
            {

                if (!Rhs[i].Equals(otherClause.Rhs[i]))
                {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode()
        {
            return Rhs
                .Select(subgoal => subgoal.GetHashCode())
                .Aggregate(Lhs.GetHashCode(), (accumulator, hashCode) => accumulator * 101 + hashCode);
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {
            var result = new HashSet<PrologVariable>(Lhs.FindBindingVariables());

            foreach (var subgoal in Rhs)
            {
                result.UnionWith(subgoal.FindBindingVariables());
            }

            return result;
        }

        public List<PrologVariable> GetListOfBindingVariables()
        {
            var result = Lhs.GetListOfBindingVariables();

            foreach (var subgoal in Rhs)
            {
                result.AddRangeUnique(subgoal.GetListOfBindingVariables());
            }

            return result;
        }

        public bool ContainsVariable(PrologVariable v)
        {
            return Lhs.ContainsVariable(v) || Rhs.Any(goal => goal.ContainsVariable(v));
        }

        public IPrologExpression ApplySubstitution(PrologSubstitution substitution)
        {
            return new PrologClause((PrologGoal)Lhs.ApplySubstitution(substitution),
                Rhs.Select(subgoal => (PrologGoal)subgoal.ApplySubstitution(substitution)).ToList());
        }

        public PrologClause RenameVariables(HashSet<PrologVariable> variablesToAvoid, PrologGlobalInfo globalInfo)
        {
            var oldVariables = FindBindingVariables();
            var substitution = new PrologSubstitution();

            foreach (var oldVariable in oldVariables)
            {

                if (!variablesToAvoid.Contains(oldVariable))
                {
                    continue;
                }

                PrologVariable newVariable;

                do
                {
                    newVariable = globalInfo.GetNextUniqueVariable();
                }
                while (oldVariables.Contains(newVariable) || variablesToAvoid.Contains(newVariable));

                substitution.SubstitutionList[oldVariable] = newVariable; // This is safe because all of the oldVariables and newVariables are unique.
                //substitution.SubstitutionList[oldVariable] = globalInfo.GetNextUniqueVariable();    // This would probably work too.
            }

            return (PrologClause)ApplySubstitution(substitution);
        }

        public PrologSubstitution Unify(IPrologExpression otherExpr)
        {
            PrologClause otherClause = otherExpr as PrologClause;

            if (otherClause == null || Rhs.Count != otherClause.Rhs.Count)
            {
                return null;
            }

            var substitution = Lhs.Unify(otherClause.Lhs);

            if (substitution == null)
            {
                return null;
            }

            for (var i = 0; i < Rhs.Count; ++i)
            {
                var newGoal1 = Rhs[i].ApplySubstitution(substitution);
                var newGoal2 = otherClause.Rhs[i].ApplySubstitution(substitution);
                var substitution2 = newGoal1.Unify(newGoal2);

                if (substitution2 == null)
                {
                    return null;
                }

                substitution = substitution.Compose(substitution2);
            }

            return substitution;
        }

        public bool IsIsomorphicTo(PrologClause otherClause, HashSet<PrologVariable> variablesToAvoid, PrologGlobalInfo globalInfo)
        {

            if (variablesToAvoid == null)
            {
                variablesToAvoid = FindBindingVariables();
            }

            otherClause = otherClause.RenameVariables(variablesToAvoid, globalInfo);

            var unifier = Unify(otherClause);

            return unifier != null && unifier.IsOneToOne;
        }

        public bool IsGround
        {
            get
            {
                return Lhs.IsGround && Rhs.All(goal => goal.IsGround);
            }
        }

        public IPrologNumber EvaluateToNumber()
        {
            return null;
        }
    }

    #endregion

    #region PrologSubstitution

    public class PrologSubstitution
    {
        public readonly Dictionary<PrologVariable, IPrologExpression> SubstitutionList = new Dictionary<PrologVariable, IPrologExpression>();

        public PrologSubstitution()
        {
        }

        public PrologSubstitution(PrologVariable v, IPrologExpression expr) // To conveniently make a substitution with a single entry.
        {
            SubstitutionList[v] = expr;
        }

        public override string ToString()
        {
            return string.Join("; ", SubstitutionList.Keys.Select(key => string.Format("{0} <= {1}", key, SubstitutionList[key])));
        }

        public PrologSubstitution Compose(PrologSubstitution otherSub)
        {
            var newSub = new PrologSubstitution();

            // 1) Apply the Src substitution to this's terms.

            foreach (var key in SubstitutionList.Keys)
            {
                var newUnifiable = SubstitutionList[key].ApplySubstitution(otherSub) as IPrologExpression;

                if (newUnifiable == null)
                {
                    throw new Exception("PrologSubstitution.Compose() : The result of applying a substitution to an IUnifiable is not an IUnifiable.");
                }

                newSub.SubstitutionList[key] = newUnifiable;
            }

            // 2) Remove identities.
            var varsToRemove = new List<PrologVariable>();

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

#if SUBSTITUTION_COMPOSITION_VERIFICATION
            // According to Kamin, we should ensure that no member of newSub.SubstitutionList.Keys appears in newSub.SubstitutionList.Values .
            var variablesInValues = new HashSet<PrologVariable>();

            foreach (var value in newSub.SubstitutionList.Values)
            {
                variablesInValues.UnionWith(value.FindBindingVariables());
            }

            foreach (var key in newSub.SubstitutionList.Keys)
            {

                if (variablesInValues.Contains(key))
                {
#if CONSOLE_WRITELINE
                    Console.WriteLine("PrologSubstitution.Compose() : Unacceptable substitution; returning null.");
#endif
                    throw new Exception(string.Format("Unacceptable substitution; key == {0}; this == {1}; otherSub == {2}; newSub = {3}",
                        key, this, otherSub, newSub));
                }
            }
#endif

            return newSub;
        }

        public bool ContainsOnlyVariables()
        {
            return SubstitutionList.Values.All(v => v is PrologVariable);
        }

        public HashSet<PrologVariable> FindBindingVariables()
        {
            var result = new HashSet<PrologVariable>();

            foreach (var key in SubstitutionList.Keys)
            {
                result.Add(key);
                result.UnionWith(SubstitutionList[key].FindBindingVariables());
            }

            return result;
        }

        public bool IsOneToOne
        {
            get
            {
                var values = new HashSet<PrologVariable>();

                foreach (var value in SubstitutionList.Values)
                {

                    if (!(value is PrologVariable))
                    {
                        return false;
                    }

                    values.Add((PrologVariable)value);
                }

                return values.Count == SubstitutionList.Count;
            }
        }
    }

    #endregion

    #region CutBacktrackException

    public class CutBacktrackException : Exception
    {
        public readonly Guid Guid;

        public CutBacktrackException(Guid g)
        {
            Guid = g;
        }
    }

    #endregion

    #region CutDetector

    public class CutDetector
    {
        public readonly Guid Guid;
        //public bool CutDetected { get; set; }

        public CutDetector()
        {
            Guid = Guid.NewGuid();
            //CutDetected = false;
        }
    }

    #endregion

    #region StringIntKey

    public class StringIntKey
    {
        private readonly string str;
        private readonly int n;

        public StringIntKey(string str, int n)
        {

            if (string.IsNullOrEmpty(str))
            {
                throw new Exception("StringIntKey constructor: str is null or empty.");
            }

            this.str = str;
            this.n = n;
        }

        public override bool Equals(object obj)
        {
            
            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherKey = obj as StringIntKey;

            return otherKey != null && otherKey.str == str && otherKey.n == n;
        }

        public override int GetHashCode()
        {
            return str.GetHashCode() * 101 + n.GetHashCode();
        }
    }

    #endregion

#if SUPPORT_USER_DEFINED_OPERATORS

    #region PrologOperator

    public class PrologOperator
    {
        public readonly int Precedence;
        public readonly OperatorType OpType; // We didn't call it "Type" because of the class System.Type
        public readonly string Name;

        public PrologOperator(int p, OperatorType t, string n)
        {
            
            if (string.IsNullOrEmpty(n))
            {
                throw new Exception("PrologOperator constructor: The operator name is null or empty.");
            }
            else if (p < 0 || p > 1200)
            {
                throw new Exception(string.Format("PrologOperator constructor: The operator precedence is {0}; it must be in the range 0 to 1200.", p));
            }

            Precedence = p;
            OpType = t;
            Name = n;
        }

        public bool IsPrefix
        {
            get
            {
                return OpType == OperatorType.fx || OpType == OperatorType.fy;
            }
        }

        public bool IsInfix
        {
            get
            {
                return OpType == OperatorType.xfx || OpType == OperatorType.xfy || OpType == OperatorType.yfx;
            }
        }

        public bool IsPostfix
        {
            get
            {
                return OpType == OperatorType.xf || OpType == OperatorType.yf;
            }
        }
    }

    #endregion

    #region PrologOperatorUsage

    public class PrologOperatorUsage
    {
        public readonly PrologOperator Op;
        public readonly PrologNameExpression<PrologFunctor> FunctorExpression;
        public readonly int ExpectedNumberOfArguments;
        public readonly List<object> Arguments = new List<object>();

        public PrologOperatorUsage(PrologOperator op)
        {
            Op = op;
            FunctorExpression = new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor(Op.Name));
            ExpectedNumberOfArguments = Op.IsInfix ? 2 : 1;
        }

        public bool IsComplete
        {
            get
            {

                if (Arguments.Count > ExpectedNumberOfArguments)
                {
                    throw new Exception(string.Format("Operator {0} : Too many arguments ({1} instead of the expected {2}).",
                        Op.Name, Arguments.Count, ExpectedNumberOfArguments));
                }
                else if (Arguments.Count < ExpectedNumberOfArguments)
                {
                    return false;
                }

#if DEAD_CODE
                foreach (var arg in Arguments)
                {
                    var argAsOperatorUsage = arg as PrologOperatorUsage;

                    if (argAsOperatorUsage != null && !argAsOperatorUsage.IsComplete)
                    {
                        return false;
                    }
                }
#endif

                return true;
            }
        }

        public PrologNameExpression<PrologFunctor> ToFunctorExpression()
        {

            if (!IsComplete)
            {
                throw new Exception(string.Format("Operator {0}: ToFunctorExpression(): Operator usage not complete.", Op.Name));
            }

            FunctorExpression.ExpressionList.Clear();

            foreach (var arg in Arguments)
            {
                var argAsPrologExpression = arg as IPrologExpression;
                var argAsOperatorUsage = arg as PrologOperatorUsage;

                if (argAsPrologExpression != null)
                {
                    FunctorExpression.ExpressionList.Add(argAsPrologExpression);
                }
                else if (argAsOperatorUsage != null)
                {
                    FunctorExpression.ExpressionList.Add(argAsOperatorUsage.ToFunctorExpression());
                }
                else
                {
                    throw new Exception(string.Format("Operator {0}: Invalid argument type.", Op.Name));
                }
            }

            return FunctorExpression;
        }

        public PrologOperatorUsage Clone()
        {
            var clone = new PrologOperatorUsage(Op);

            foreach (var arg in Arguments)
            {
                var argAsOperatorUsage = arg as PrologOperatorUsage;

                if (argAsOperatorUsage != null)
                {
                    clone.Arguments.Add(argAsOperatorUsage.Clone());
                }
                else
                {
                    clone.Arguments.Add(arg);
                }
            }

            return clone;
        }
    }

    #endregion

#endif

    #region PrologModule

    public class PrologModule
    {
        public readonly List<StringIntKey> ExportList; // = new List<StringIntKey>();
        public readonly List<KeyValuePair<StringIntKey, PrologModule>> ImportList = new List<KeyValuePair<StringIntKey, PrologModule>>();
        public readonly List<PrologClause> ClauseList = new List<PrologClause>();

        public PrologModule(List<StringIntKey> exportList = null)
        {
            ExportList = exportList ?? new List<StringIntKey>();
        }

        public void Clear()
        {
            ImportList.Clear();
            ClauseList.Clear();
        }
    }

    #endregion

    #region PrologGlobalInfo

    public class PrologGlobalInfo : IGlobalInfoOps, IParser
    {
        public const string ClauseAdded = "Clause added.";
        public const string ClauseAlreadyExists = "An identical clause is already in the clause list.";
        public const string IsomorphicClauseAlreadyExists = "An isomorphic clause is already in the clause list.";
        public const string IsomorphicOrMoreGeneralClauseAlreadyExists = "An isomorphic or more general clause is already in the clause list.";
        public const string OperatorAdded = "Operator added.";
        public const string InvalidCommand = "Invalid command.";
        public const string Satisfied = "Satisfied";
        public const string NotSatisfied = "Not satisfied";
        //public readonly List<PrologClause> ClauseList = new List<PrologClause>();
        public readonly GrammarSelector gs;
        public readonly ITokenizer tokenizer;
        public readonly IParser parser;
        private int variableRenameNum = 0;
        private bool allMode = false;    // Determines how many solutions we will search for.  false means "first" mode; true means "all" mode.
        private readonly StringBuilder sbOutput = new StringBuilder();
        private readonly Random random = new Random();
        private readonly HashSet<string> LoadedPresets = new HashSet<string>();
        private SolutionCollectionMode solutionCollectionMode = SolutionCollectionMode.None;
        private IPrologExpression findAll_Expression = null;
        private List<IPrologExpression> findAll_ResultList = null;
        private List<PrologVariable> caretListVariables = null;
        private Dictionary<ExpressionListAsKey, List<IPrologExpression>> dictSolutions = null;
        public string PathToDefaultDirectory = null;
        private readonly Dictionary<StringIntKey, Func<PrologGoal, PrologSubstitution>> dictBuiltInPredicates
            = new Dictionary<StringIntKey, Func<PrologGoal, PrologSubstitution>>();
#if SUPPORT_USER_DEFINED_OPERATORS
        private readonly List<PrologOperator> Operators = new List<PrologOperator>();
#endif
        public IInterpreterFileLoader FileLoader = null;
        private readonly PrologModule DefaultModule = new PrologModule();
        private readonly Dictionary<string, PrologModule> dictModules = new Dictionary<string, PrologModule>(); // The keys are file paths.

        public PrologGlobalInfo(GrammarSelector gs, ITokenizer t, IParser p)
        {
            this.gs = gs;
            tokenizer = t;
            parser = p;

            if (gs == GrammarSelector.Prolog)
            {
                dictBuiltInPredicates[new StringIntKey("plus", 3)] = KaminPlus3;
                dictBuiltInPredicates[new StringIntKey("minus", 3)] = KaminMinus3;
                dictBuiltInPredicates[new StringIntKey("less", 2)] = LessThan2;
                dictBuiltInPredicates[new StringIntKey("not-equal", 2)] = NotEquals2;
            }

            dictBuiltInPredicates[new StringIntKey("is", 2)] = Is2;
            dictBuiltInPredicates[new StringIntKey("<", 2)] = LessThan2;
            dictBuiltInPredicates[new StringIntKey(">", 2)] = GreaterThan2;
            dictBuiltInPredicates[new StringIntKey("=<", 2)] = EqualOrLessThan2;
            dictBuiltInPredicates[new StringIntKey(">=", 2)] = GreaterThanOrEqual2;
            dictBuiltInPredicates[new StringIntKey("=:=", 2)] = ArithmeticEqual2;
            dictBuiltInPredicates[new StringIntKey(@"=\=", 2)] = ArithmeticNotEqual2;
            dictBuiltInPredicates[new StringIntKey("=", 2)] = Unifiable2;
            dictBuiltInPredicates[new StringIntKey(@"\=", 2)] = NotUnifiable2;
            dictBuiltInPredicates[new StringIntKey("==", 2)] = Equals2;
            dictBuiltInPredicates[new StringIntKey(@"\==", 2)] = NotEquals2;
            dictBuiltInPredicates[new StringIntKey("open", 3)] = Open3;
            dictBuiltInPredicates[new StringIntKey("close", 1)] = Close1;
            dictBuiltInPredicates[new StringIntKey("at_end_of_stream", 1)] = AtEndOfStream1;
            dictBuiltInPredicates[new StringIntKey("read", 2)] = Read2;
            dictBuiltInPredicates[new StringIntKey("get_code", 2)] = GetCode2;
            dictBuiltInPredicates[new StringIntKey("write", 1)] = Write1; // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_3.html
            dictBuiltInPredicates[new StringIntKey("write", 2)] = Write2;
            dictBuiltInPredicates[new StringIntKey("tab", 1)] = Tab1;
            dictBuiltInPredicates[new StringIntKey("tab", 2)] = Tab2;
            dictBuiltInPredicates[new StringIntKey("nl", 0)] = NL0;
            dictBuiltInPredicates[new StringIntKey("nl", 1)] = NL1;
            dictBuiltInPredicates[new StringIntKey("atom", 1)] = Atom1;
            dictBuiltInPredicates[new StringIntKey("integer", 1)] = Integer1;
            dictBuiltInPredicates[new StringIntKey("float", 1)] = Float1;
            dictBuiltInPredicates[new StringIntKey("number", 1)] = Number1;
            dictBuiltInPredicates[new StringIntKey("atomic", 1)] = Atomic1;
            // var: See usage in http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/5_3.html
            // See also http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/4.html#4.8
            dictBuiltInPredicates[new StringIntKey("var", 1)] = Var1;
            dictBuiltInPredicates[new StringIntKey("nonvar", 1)] = NonVar1;
            dictBuiltInPredicates[new StringIntKey("ground", 1)] = Ground1; // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/4.html#4.8
            dictBuiltInPredicates[new StringIntKey("random", 2)] = Random2;
            dictBuiltInPredicates[new StringIntKey("fail", 0)] = Fail0;
            // listing/0 and listing/1: See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse4
            dictBuiltInPredicates[new StringIntKey("listing", 0)] = Listing0;
            dictBuiltInPredicates[new StringIntKey("listing", 1)] = Listing1;
            dictBuiltInPredicates[new StringIntKey("arg", 3)] = Arg3; // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
            dictBuiltInPredicates[new StringIntKey("=..", 2)] = Univ2; // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
            dictBuiltInPredicates[new StringIntKey("atom_codes", 2)] = AtomicCodes;     // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
            dictBuiltInPredicates[new StringIntKey("number_codes", 2)] = AtomicCodes;   // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
            dictBuiltInPredicates[new StringIntKey("name", 2)] = AtomicCodes;
            dictBuiltInPredicates[new StringIntKey("atom_chars", 2)] = AtomicChars;
            dictBuiltInPredicates[new StringIntKey("number_chars", 2)] = AtomicChars;   // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
            dictBuiltInPredicates[new StringIntKey("char_code", 2)] = CharCode;         // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
            dictBuiltInPredicates[new StringIntKey("atom_number", 2)] = AtomNumber;     // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
            dictBuiltInPredicates[new StringIntKey("atom_length", 2)] = AtomLength;     // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
            //dictBuiltInPredicates[new StringIntKey("findall", 3)] = FindAll3;           // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49
            dictBuiltInPredicates[new StringIntKey("assert", 1)] = AssertA1;
            dictBuiltInPredicates[new StringIntKey("asserta", 1)] = AssertA1;
            dictBuiltInPredicates[new StringIntKey("assertz", 1)] = AssertZ1;

#if SUPPORT_USER_DEFINED_OPERATORS
            CreateBuiltInOperators();
#endif
        }

#if SUPPORT_USER_DEFINED_OPERATORS
        private void CreateBuiltInOperators()
        {
            // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse40
            /*
            :-  op(  1200,  xfx,  [  :-,  -->  ]).
            :-  op(  1200,    fx,  [  :-,  ?-  ]).
            :-  op(  1100,  xfy,  [  ;  ]).
            :-  op(  1000,  xfy,  [  ’,’  ]).
            :-  op(    700,  xfx,  [  =,  is,  =..,  ==,  \==,  =:=,  =\=,  <,  >,  =<,  >=  ]).
            :-  op(    500,  yfx,  [  +,  -]).
            :-  op(    500,    fx,  [  +,  -  ]).
            :-  op(    300,  xfx,  [  mod  ]).
            :-  op(    200,  xfy,  [  ^  ]). 
             */
            // See also http://www.swi-prolog.org/pldoc/man?section=operators
            Operators.Add(new PrologOperator(1200, OperatorType.xfx, ":-"));
            Operators.Add(new PrologOperator(1200, OperatorType.xfx, "-->"));
            Operators.Add(new PrologOperator(1200, OperatorType.fx, ":-"));
            Operators.Add(new PrologOperator(1200, OperatorType.fx, "?-"));
            Operators.Add(new PrologOperator(1100, OperatorType.xfy, ";"));
            Operators.Add(new PrologOperator(1100, OperatorType.xfy, "|"));
            Operators.Add(new PrologOperator(1050, OperatorType.xfy, "->"));
            Operators.Add(new PrologOperator(1000, OperatorType.xfy, ","));
            Operators.Add(new PrologOperator(900, OperatorType.fy, @"\+"));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, "="));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, "is"));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, "=.."));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, "=="));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, @"\=="));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, "=:="));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, @"=\="));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, "<"));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, ">"));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, "=<"));
            Operators.Add(new PrologOperator(700, OperatorType.xfx, ">="));
            Operators.Add(new PrologOperator(600, OperatorType.xfy, ":"));
            Operators.Add(new PrologOperator(500, OperatorType.yfx, "+"));
            Operators.Add(new PrologOperator(500, OperatorType.yfx, "-"));
            //Operators.Add(new PrologOperator(500, OperatorType.fx, "+"));
            //Operators.Add(new PrologOperator(500, OperatorType.fx, "-"));
            //Operators.Add(new PrologOperator(500, OperatorType.fx, "?")); // Do we use this?
            Operators.Add(new PrologOperator(400, OperatorType.yfx, "*"));
            Operators.Add(new PrologOperator(400, OperatorType.yfx, "/"));
            Operators.Add(new PrologOperator(400, OperatorType.yfx, "mod"));    // According to SWI-Prolog.
            //Operators.Add(new PrologOperator(300, OperatorType.xfx, "mod"));
            Operators.Add(new PrologOperator(200, OperatorType.xfy, "^"));
            Operators.Add(new PrologOperator(200, OperatorType.fy, "+"));       // According to SWI-Prolog.
            Operators.Add(new PrologOperator(200, OperatorType.fy, "-"));       // According to SWI-Prolog.
            //Operators.Add(new PrologOperator(0, OperatorType.fx, "("));
        }
#endif

        public void Clear()
        {
            //ClauseList.Clear();
            variableRenameNum = 0;
            FindFirstSolution();
            LoadedPresets.Clear();
#if SUPPORT_USER_DEFINED_OPERATORS
            Operators.Clear();
            CreateBuiltInOperators();
#endif
            DefaultModule.Clear();
            dictModules.Clear();
        }

        public void FindFirstSolution()
        {
            allMode = false;
        }

        public void FindAllSolutions()
        {
            allMode = true;
        }

        public string LoadPreset(string presetName)
        {

            if (LoadedPresets.Contains(presetName))
            {
                return string.Format("The preset '{0}' has already been loaded.", presetName);
            }

            switch (presetName)
            {
                case "<=":

                    if (gs == GrammarSelector.Prolog)
                    {
                        ProcessInputString("(infer (<= X X))");
                        ProcessInputString("(infer (<= X Y) from (less X Y))");
                    }
                    else
                    {
                        ProcessInputString("'<='(X, Y) :- X =< Y.");
                    }

                    break;

                case "addtoend":

                    if (gs == GrammarSelector.Prolog)
                    {
                        // (addtoend L X M) means that M is the list obtained by adding X to the end of L.
                        ProcessInputString("(infer (addtoend nil X (cons X nil)))");
                        ProcessInputString("(infer (addtoend (cons Y L) X (cons Y M)) from (addtoend L X M))");
                    }
                    else
                    {
                        ProcessInputString("addtoend([], X, [X]).");
                        ProcessInputString("addtoend([Y | L], X, [Y | M]) :- addtoend(L, X, M).");
                    }

                    break;

                case "append":
                //case "append2": // The preset name "append2" is deprecated.

                    if (gs == GrammarSelector.Prolog)
                    {
                        // (append L M N) means that N is the list obtained by appending M onto the end of L.
                        ProcessInputString("(infer (append nil L L))");
                        ProcessInputString("(infer (append (cons X L) M (cons X N)) from (append L M N))");
                    }
                    else
                    {
                        ProcessInputString("append([], L, L).");
                        ProcessInputString("append([X | Y], L, [X | Z]) :- append(Y, L, Z).");
                    }

                    break;

                case "member":
                //case "member2": // The preset name "member2" is deprecated.

                    if (gs == GrammarSelector.Prolog)
                    {
                        ProcessInputString("(infer (member X (cons X L)))");
                        ProcessInputString("(infer (member X (cons Y M)) from (member X M))");
                    }
                    else
                    {
                        ProcessInputString("member(X, [X | _]).");
                        ProcessInputString("member(X, [_ | T]) :- member(X, T).");
                    }

                    break;

                case "permutation":
                    LoadPreset("append");
                    ProcessInputString("permutation([], []).");
                    ProcessInputString("permutation(L, [H | T]) :- append(V, [H | U], L), append(V, U, W), permutation(W, T).");
                    break;

                case "rev": // Reverse a list.
                    ProcessInputString("accRev([H | T], A, R):-  accRev(T, [H | A], R).");
                    ProcessInputString("accRev([], A, A).");
                    ProcessInputString("rev(L, R) :- accRev(L, [], R).");
                    break;

                case "succ":
                    ProcessInputString("intToSucc(0, 0).");
                    ProcessInputString("intToSucc(N, succ(L)) :- N > 0, M is N - 1, intToSucc(M, L).");
                    ProcessInputString("succToInt(0, 0).");
                    ProcessInputString("succToInt(succ(L), N) :- succToInt(L, M), N is M + 1.");
                    break;

                case "atom_concat":
                    LoadPreset("append");
                    // We want to use a cut in one of these clauses to avoid repeated results in the case where A1, A2, and A3 are all atomic.
                    // We can use a cut in this clause because it can produce at most one result, even in "all" mode:
                    ProcessInputString("atom_concat(A1, A2, A3) :- atomic(A1), atomic(A2), atom_chars(A1, L1), atom_chars(A2, L2), append(L1, L2, L3), atom_chars(A3, L3), !.");
                    // We don't use a cut in ths clause because it can produce multiple results in "all" mode:
                    ProcessInputString("atom_concat(A1, A2, A3) :- atomic(A3), atom_chars(A3, L3), append(L1, L2, L3), atom_chars(A1, L1), atom_chars(A2, L2).");
                    break;

                case "concat_atom":
                    LoadPreset("atom_concat");
                    ProcessInputString("concat_atomAcc(Acc, [], Acc).");
                    ProcessInputString("concat_atomAcc(Acc, [H | T], Result) :- atom_concat(Acc, H, Acc2), concat_atomAcc(Acc2, T, Result).");
                    // We can use a cut in this clause to avoid reporting results using both this clause and the next clause when in "all" mode.
                    // This clause is not satisfied when there are variables in the first parameter.
                    ProcessInputString("concat_atom([H | T], Result) :- concat_atomAcc(H, T, Result), !.");
                    // We don't use a cut in ths clause because it can produce multiple results in "all" mode:
                    ProcessInputString("concat_atom([X, Y], Result) :- atom_concat(X, Y, Result).");
                    break;

                default:
                    throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
            }

            LoadedPresets.Add(presetName);
            return string.Format("The preset '{0}' has been successfully loaded.", presetName);
        }

        public void LoadPresets()
        {
            //ProcessInputString("");
        }

        private PrologNameExpression<PrologFunctor> CreateAtom(PrologFunctor f)
        {
            return new PrologNameExpression<PrologFunctor>(gs, f);
        }

        private PrologNameExpression<PrologFunctor> CreateAtom(string name)
        {

            if (string.IsNullOrEmpty(name))
            {
                throw new Exception("CreateAtom() : The atom name is null or empty.");
            }

            return CreateAtom(new PrologFunctor(name));
        }

        public PrologVariable GetNextUniqueVariable()
        {
            ++variableRenameNum;
            return new PrologVariable("Var" + variableRenameNum.ToString());
        }

        private HashSet<PrologVariable> GetVariablesFromGoalList(List<PrologGoal> goalListParam)
        {
            var result = new HashSet<PrologVariable>();

            foreach (var goal in goalListParam)
            {
                result.UnionWith(goal.FindBindingVariables());
            }

            return result;
        }

        public List<PrologVariable> GetListOfBindingVariablesFromGoalList(List<PrologGoal> goalListParam)
        {
            var result = new List<PrologVariable>();

            foreach (var goal in goalListParam)
            {
                result.AddRangeUnique(goal.GetListOfBindingVariables());
            }

            return result;
        }

        public static PrologNameExpression<PrologFunctor> ConvertToFunctorExpression(object obj)
        {
            PrologNameExpression<PrologFunctor> functorExpression;

            if (obj is PrologNameExpression<PrologFunctor>)
            {
                functorExpression = (PrologNameExpression<PrologFunctor>)obj;
            }
            else if (obj is PrologFunctor)
            {
                functorExpression = new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, (PrologFunctor)obj);
            }
            else if (obj is PrologVariable)
            {
                var v = (PrologVariable)obj;

                functorExpression = new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor(v.Name));
            }
            else if (obj is string)
            {
                functorExpression = new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor((string)obj));
            }
            else
            {
                functorExpression = null;
            }

            return functorExpression;
        }

        private PrologSubstitution ApplyAssert1(PrologGoal goal, bool asserta)
        {
            var clause = CreateClause_General(goal.ExpressionList[0]);

            if (clause == null)
            {
                return null;
            }

            if (asserta)
            {
                DefaultModule.ClauseList.Insert(0, clause);
            }
            else
            {
                DefaultModule.ClauseList.Add(clause);
            }

            return new PrologSubstitution();
        }

        private PrologSubstitution AssertA1(PrologGoal goal)
        {
            return ApplyAssert1(goal, true);
        }

        private PrologSubstitution AssertZ1(PrologGoal goal)
        {
            return ApplyAssert1(goal, false);
        }

        // Question: Should retract be able to remove multiple clauses when retracting a clause that uses non-binding variables?
        // See the definition of "undo" at the bottom of http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_17.html
        // Answer: No, we provide retractall (further below) for removing multiple clauses without binding any variables.

        private PrologSubstitution Retract1(PrologGoal goal, PrologSubstitution oldSubstitution, HashSet<PrologVariable> parentVariablesToAvoid)
        {
            var clause = CreateClause_General(goal.ExpressionList[0]);

            if (clause == null)
            {
                return null;
            }

            var variablesToAvoid = clause.FindBindingVariables();

            variablesToAvoid.UnionWith(parentVariablesToAvoid);
            variablesToAvoid.UnionWith(oldSubstitution.FindBindingVariables());

            for (var clauseNum = 0; clauseNum < DefaultModule.ClauseList.Count; ++clauseNum)
            {
                var newClause = DefaultModule.ClauseList[clauseNum].RenameVariables(variablesToAvoid, this);
                var unifier = newClause.Unify(clause);

                if (unifier != null)
                {
                    DefaultModule.ClauseList.RemoveAt(clauseNum);

                    return oldSubstitution.Compose(unifier);
                }
            }

            // No clause was retracted.
            // TODO: Should we really return null if nothing was retracted, or should we say that "retract" successfully did nothing?
            return null;
        }

        private PrologSubstitution RetractAll1(PrologGoal goal, PrologSubstitution oldSubstitution, HashSet<PrologVariable> parentVariablesToAvoid)
        {
            var clause = CreateClause_General(goal.ExpressionList[0]);

            if (clause == null)
            {
                return null;
            }

            var variablesToAvoid = clause.FindBindingVariables();

            variablesToAvoid.UnionWith(parentVariablesToAvoid);
            variablesToAvoid.UnionWith(oldSubstitution.FindBindingVariables());

            var clausesToRemove = DefaultModule.ClauseList
                .Where(cl => clause.RenameVariables(variablesToAvoid, this).Unify(cl) != null)
                .ToList();

            if (clausesToRemove.Count == 0)
            {
                // No clause was retracted.
                // TODO: Should we really return null if nothing was retracted, or should we say that "retractall" successfully did nothing?
                return null;
            }

            foreach (var cl in clausesToRemove)
            {
                DefaultModule.ClauseList.Remove(cl);
            }

            return new PrologSubstitution();
        }

        // This function is used by Kamin's Prolog only.

        private PrologSubstitution KaminApplyBuiltInArithmeticOperator(
            PrologGoal goal,
            Func<int, int, int> arithmeticOperator)
        {
            var intlit1 = goal.ExpressionList[0] as PrologIntegerLiteral;
            var intlit2 = goal.ExpressionList[1] as PrologIntegerLiteral;

            if (intlit1 == null || intlit2 == null)
            {
                return null;
            }

            var sum = arithmeticOperator(intlit1.Value, intlit2.Value);

            return goal.ExpressionList[2].Unify(new PrologIntegerLiteral(sum));
        }

        private PrologSubstitution ApplyBuiltInComparisonOperator(
            PrologGoal goal,
            Func<int, int, bool> intComparisonOperator,
            Func<double, double, bool> floatComparisonOperator)
        {
            var lhsEvalated = goal.ExpressionList[0].EvaluateToNumber();
            var rhsEvalated = goal.ExpressionList[1].EvaluateToNumber();

            if (lhsEvalated == null || rhsEvalated == null)
            {
                return null;
            }

            bool comparisonResult;

            if (lhsEvalated is PrologIntegerLiteral && rhsEvalated is PrologIntegerLiteral)
            {
                comparisonResult = intComparisonOperator(lhsEvalated.ToInteger(), rhsEvalated.ToInteger());
            }
            else
            {
                comparisonResult = floatComparisonOperator(lhsEvalated.ToDouble(), rhsEvalated.ToDouble());
            }

            if (comparisonResult)
            {
                return new PrologSubstitution();
            }

            return null;
        }

        private PrologSubstitution GoalDisjunction2(
            PrologGoal goal,
            List<PrologGoal> goalList,
            List<CutDetector> cutDetectorList,
            int goalNum,
            PrologSubstitution oldSubstitution,
            HashSet<PrologVariable> parentVariablesToAvoid,
            List<PrologVariable> variablesInQuery,
            List<PrologModule> listOfCurrentModules)
        {
            var goal1 = ExpressionToGoal(goal.ExpressionList[0]);
            var goal2 = ExpressionToGoal(goal.ExpressionList[1]);
            var nextGoalNum = goalNum + 1;
            var cutDetector = cutDetectorList[goalNum];
            var currentModule = listOfCurrentModules[goalNum];
            PrologSubstitution localSubstitution;

            if (goal1 == null)
            {
                return null;
            }

            goalList.Insert(nextGoalNum, goal1);
            cutDetectorList.Insert(nextGoalNum, cutDetector);
            listOfCurrentModules.Insert(nextGoalNum, currentModule);

            try
            {
                localSubstitution = ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
                    listOfCurrentModules);
            }
            finally // In case a CutBacktrackException is thrown.
            {
                goalList.RemoveAt(nextGoalNum);
                cutDetectorList.RemoveAt(nextGoalNum);
                listOfCurrentModules.RemoveAt(nextGoalNum);
            }

            if (localSubstitution != null)
            {
                return localSubstitution;
            }

            if (goal2 == null)
            {
                return null;
            }

            goalList.Insert(nextGoalNum, goal2);
            cutDetectorList.Insert(nextGoalNum, cutDetector);
            listOfCurrentModules.Insert(nextGoalNum, currentModule);

            try
            {
                localSubstitution = ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
                    listOfCurrentModules);
            }
            finally // In case a CutBacktrackException is thrown.
            {
                goalList.RemoveAt(nextGoalNum);
                cutDetectorList.RemoveAt(nextGoalNum);
                listOfCurrentModules.RemoveAt(nextGoalNum);
            }

            return localSubstitution;
        }

        private PrologSubstitution IfThenElse3(
            //PrologGoal goal,
            IPrologExpression ifPart,
            IPrologExpression thenPart,
            IPrologExpression elsePart,
            List<PrologGoal> goalList,
            List<CutDetector> cutDetectorList,
            int goalNum,
            PrologSubstitution oldSubstitution,
            HashSet<PrologVariable> parentVariablesToAvoid,
            List<PrologVariable> variablesInQuery,
            List<PrologModule> listOfCurrentModules)
        {
            var conditionGoal = ExpressionToGoal(ifPart);

            if (conditionGoal == null)
            {
                return null;
            }

            var nextGoalNum = goalNum + 1;
            IPrologExpression chosenGoalAsExpression;
            var cutDetector = cutDetectorList[goalNum];
            var currentModule = listOfCurrentModules[goalNum];
            var cachedAllMode = allMode;
            var cachedSolutionCollectionMode = solutionCollectionMode;
            var tempGoalList = new List<PrologGoal>() { conditionGoal };
            // goalIfThenElse.ConditionGoal had better not be a cut;
            // it would not make much sense if it was, since it would always be satisfied, and it would do nothing.
            var tempCutDetectorList = new List<CutDetector>() { cutDetector };
            var tempListOfCurrentModules = new List<PrologModule>() { currentModule };
            PrologSubstitution localSubstitution;
            PrologSubstitution result;

            allMode = false;
            solutionCollectionMode = SolutionCollectionMode.None;

            try
            {
                localSubstitution = ProveGoalList(tempGoalList, tempCutDetectorList, 0, oldSubstitution, parentVariablesToAvoid, null, tempListOfCurrentModules);
            }
            finally
            {
                allMode = cachedAllMode;
                solutionCollectionMode = cachedSolutionCollectionMode;
            }

            if (localSubstitution != null)
            {
                chosenGoalAsExpression = thenPart;
                //localSubstitution = oldSubstitution.Compose(localSubstitution);
            }
            else
            {
                chosenGoalAsExpression = elsePart;
                localSubstitution = oldSubstitution;
            }

            var chosenGoal = ExpressionToGoal(chosenGoalAsExpression);

            if (chosenGoal == null)
            {
                return null;
            }

            goalList.Insert(nextGoalNum, chosenGoal);
            cutDetectorList.Insert(nextGoalNum, cutDetector);
            listOfCurrentModules.Insert(nextGoalNum, currentModule);

            try
            {
                result = ProveGoalList(goalList, cutDetectorList, nextGoalNum, localSubstitution, parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
            }
            finally
            {
                goalList.RemoveAt(nextGoalNum);
                cutDetectorList.RemoveAt(nextGoalNum);
                listOfCurrentModules.RemoveAt(nextGoalNum);
            }

            return result;
        }

        private PrologSubstitution Functor3(PrologGoal goal, HashSet<PrologVariable> parentVariablesToAvoid)
        {
            var firstArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
            var firstArgAsVariable = goal.ExpressionList[0] as PrologVariable;
            var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;
            var secondArgAsVariable = goal.ExpressionList[1] as PrologVariable;
            var thirdArgAsInteger = goal.ExpressionList[2] as PrologIntegerLiteral;
            var thirdArgAsVariable = goal.ExpressionList[2] as PrologVariable;
            var functorSubstitution = new PrologSubstitution();

            if (firstArgAsFunctorExpression != null)
            {
                var functorName = firstArgAsFunctorExpression.Name.Name;
                var functorArity = firstArgAsFunctorExpression.ExpressionList.Count;

                if (secondArgAsFunctorExpression != null)
                {

                    if (secondArgAsFunctorExpression.Name.Name != functorName || secondArgAsFunctorExpression.ExpressionList.Count != 0)
                    {
                        return null;
                    }
                }
                else if (secondArgAsVariable != null)
                {
                    //functorSubstitution.SubstitutionList[secondArgAsVariable] = CreateAtom(firstArgAsFunctorExpression.Name);
                    functorSubstitution = secondArgAsVariable.Unify(CreateAtom(firstArgAsFunctorExpression.Name));
                }
                else
                {
                    return null;
                }

                if (thirdArgAsVariable != null && functorSubstitution.SubstitutionList.Count > 0)
                {
                    var newThirdArg = thirdArgAsVariable.ApplySubstitution(functorSubstitution);

                    thirdArgAsInteger = newThirdArg as PrologIntegerLiteral;
                    thirdArgAsVariable = newThirdArg as PrologVariable;
                }

                if (thirdArgAsInteger != null)
                {

                    if (thirdArgAsInteger.Value != functorArity)
                    {
                        return null;
                    }
                }
                else if (thirdArgAsVariable != null)
                {
                    //var sub = new PrologSubstitution(thirdArgAsVariable, new PrologIntegerLiteral(functorArity));
                    // Use Unify() because thirdArgAsVariable could be a non-binding variable.
                    var sub = thirdArgAsVariable.Unify(new PrologIntegerLiteral(functorArity));

                    functorSubstitution = functorSubstitution.Compose(sub);
                }
                else
                {
                    return null;
                }
            }
            else if (firstArgAsVariable != null)
            {

                if (secondArgAsFunctorExpression != null && secondArgAsFunctorExpression.ExpressionList.Count == 0 &&
                    thirdArgAsInteger != null && thirdArgAsInteger.Value >= 0)
                {
                    var variablesToAvoid = goal.FindBindingVariables();

                    variablesToAvoid.UnionWith(parentVariablesToAvoid);

                    var exprList = new List<IPrologExpression>();

                    for (var i = 0; i < thirdArgAsInteger.Value; ++i)
                    {
                        // TODO: This code is similar to code in ProveGoalList().  Factor out the common code.
                        PrologVariable v;

                        do
                        {
                            v = GetNextUniqueVariable();
                        }
                        while (variablesToAvoid.Contains(v));

                        exprList.Add(v);
                    }

                    functorSubstitution = firstArgAsVariable.Unify(new PrologNameExpression<PrologFunctor>(
                        gs,
                        secondArgAsFunctorExpression.Name,
                        exprList));
                }
                else
                {
                    return null;
                }
            }
            else
            {
                // The first argument is neither a functor expression nor a variable.
                // The second argument must be either a variable or firstArg.ToString().
                // The third argument must be either a variable or the integer zero.
                var firstArgAsString = goal.ExpressionList[0].ToString();

                if (secondArgAsFunctorExpression != null && secondArgAsFunctorExpression.Name.Name == firstArgAsString &&
                    secondArgAsFunctorExpression.ExpressionList.Count == 0)
                {
                }
                else if (secondArgAsVariable != null)
                {
                    functorSubstitution = secondArgAsVariable.Unify(CreateAtom(firstArgAsString));
                }
                else
                {
                    return null;
                }

                if (thirdArgAsVariable != null && functorSubstitution.SubstitutionList.Count > 0)
                {
                    var newThirdArg = thirdArgAsVariable.ApplySubstitution(functorSubstitution);

                    thirdArgAsInteger = newThirdArg as PrologIntegerLiteral;
                    thirdArgAsVariable = newThirdArg as PrologVariable;
                }

                if (thirdArgAsInteger != null && thirdArgAsInteger.Value == 0)
                {
                }
                else if (thirdArgAsVariable != null)
                {
                    //var sub = new PrologSubstitution(thirdArgAsVariable, new PrologIntegerLiteral(0));
                    // Use Unify() because thirdArgAsVariable could be a non-binding variable.
                    var sub = thirdArgAsVariable.Unify(new PrologIntegerLiteral(0));

                    functorSubstitution = functorSubstitution.Compose(sub);
                }
                else
                {
                    return null;
                }
            }

            return functorSubstitution;
        }

        public static List<IPrologExpression> PrologListToCSharpList(IPrologExpression expr)
        {
            var functorExpression = expr as PrologNameExpression<PrologFunctor>;

            if (functorExpression == null)
            {
                return null;
            }
            else if (functorExpression.Name.Name == "[]" && functorExpression.ExpressionList.Count == 0)
            {
                return new List<IPrologExpression>();
            }
            else if (functorExpression.Name.Name == "." && functorExpression.ExpressionList.Count == 2)
            {
                var result = PrologListToCSharpList(functorExpression.ExpressionList[1]);

                if (result == null)
                {
                    return null;
                }

                result.Insert(0, functorExpression.ExpressionList[0]);
                return result;
            }
            else
            {
                return null;
            }
        }

        // This is used only in Prolog2 (standard Prolog notation):

        public static PrologNameExpression<PrologFunctor> CSharpListToPrologList(List<IPrologExpression> exprList, int i = 0)
        {

            if (i >= exprList.Count)
            {
                return new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor("[]"), null);
            }
            else
            {
                return new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor("."),
                    new List<IPrologExpression>() { exprList[i], CSharpListToPrologList(exprList, i + 1) });
            }
        }

        public static PrologGoal ExpressionToGoal(IPrologExpression expr)
        {
            //var fe = expr as PrologNameExpression<PrologFunctor>;
            var fe = ConvertToFunctorExpression(expr);

            if (fe == null)
            {
                return null;
            }

            return fe.ToGoal();
        }

        public static List<PrologGoal> CSharpListToGoalList(List<IPrologExpression> exprList)
        {
            var goalList = new List<PrologGoal>();

            foreach (var e in exprList)
            {
                var goal = ExpressionToGoal(e);

                if (goal == null)
                {
                    //throw new Exception(string.Format("CSharpListToGoalList() : '{0}' is not a functor expression.", e));
                    return null;
                }

                goalList.Add(goal);
            }

            return goalList;
        }

        public static List<PrologGoal> PrologListToGoalList(IPrologExpression expr)
        {
            return CSharpListToGoalList(PrologListToCSharpList(expr));
        }

        public static PrologNameExpression<PrologFunctor> CreateClauseAsFunctorExpression(IPrologExpression lhs, IPrologExpression rhs)
        {
            return new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor("clause"),
                new List<IPrologExpression>() { lhs, rhs });
        }

        public static PrologClause CreateClause(IPrologExpression expr)
        {
            var fe = expr as PrologNameExpression<PrologFunctor>;

            if (fe == null || (fe.Name.Name != "clause" && fe.Name.Name != ":-") || fe.ExpressionList.Count != 2)
            {
                //Console.WriteLine("CreateClause 1: null");
                return null;
            }

            var lhs = ExpressionToGoal(fe.ExpressionList[0]);
#if SUPPORT_USER_DEFINED_OPERATORS
            var rhs = CSharpListToGoalList(ExpressionToCSharpList(fe.ExpressionList[1]));
#else
            List<PrologGoal> rhs;

            if (fe.Name.Name == "clause")
            {
                rhs = PrologListToGoalList(fe.ExpressionList[1]);
            }
            else
            {
                rhs = CSharpListToGoalList(CommaSeparatedListToCSharpList(fe.ExpressionList[1]));

                if (rhs == null)
                {
                    Console.WriteLine("CreateClause 3: rhs is null");
                }
            }
#endif

            if (lhs == null || rhs == null)
            {
                //Console.WriteLine("CreateClause 2: null");
                return null;
            }

            return new PrologClause(lhs, rhs);
        }

        private PrologClause CreateClause_General(IPrologExpression expr)
        {
            var clause = CreateClause(expr);

            if (clause != null)
            {
                return clause;
            }

            var fe = ConvertToFunctorExpression(expr);

            if (fe == null)
            {
                return null;
            }

            return new PrologClause(fe.ToGoal(), new List<PrologGoal>());
        }

        public static string PrologCodeListToCSharpString(IPrologExpression expr)
        {
            var csharpList = PrologListToCSharpList(expr);

            if (csharpList == null)
            {
                return null;
            }

            var sb = new StringBuilder();

            foreach (var value in csharpList)
            {
                var intValue = value as PrologIntegerLiteral;

                if (intValue == null || intValue.Value < 0)
                {
                    return null;
                }

                sb.Append((char)intValue.Value);
            }

            return sb.ToString();
        }

        public static PrologNameExpression<PrologFunctor> CSharpStringToPrologCodeList(string str)
        {
            var listOfIntCodes = new List<IPrologExpression>();

            for (var i = 0; i < str.Length; ++i)
            {
                listOfIntCodes.Add(new PrologIntegerLiteral((int)str[i]));
            }

            return PrologGlobalInfo.CSharpListToPrologList(listOfIntCodes);
        }

        public static string PrologCharListToCSharpString(IPrologExpression expr)
        {
            var csharpList = PrologListToCSharpList(expr);

            if (csharpList == null)
            {
                return null;
            }

            var sb = new StringBuilder();

            foreach (var value in csharpList)
            {
                var functorExpr = value as PrologNameExpression<PrologFunctor>;

                if (functorExpr == null || functorExpr.Name.Name.Length != 1 || functorExpr.ExpressionList.Count != 0)
                {
                    return null;
                }

                sb.Append(functorExpr.Name.Name);
            }

            return sb.ToString();
        }

        public static PrologNameExpression<PrologFunctor> CSharpStringToPrologCharList(string str)
        {
            var listOfChars = new List<IPrologExpression>();

            for (var i = 0; i < str.Length; ++i)
            {
                listOfChars.Add(new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor(str.Substring(i, 1)), null));
            }

            return PrologGlobalInfo.CSharpListToPrologList(listOfChars);
        }

        private PrologSubstitution AtomicCodes(PrologGoal goal)
        {
            var goalName = goal.Name.Name;
            var firstArg = goal.ExpressionList[0];
            var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
            var firstArgIsAtom = firstArgAsFunctorExpression != null && firstArgAsFunctorExpression.ExpressionList.Count == 0;
            var secondArg = goal.ExpressionList[1];
            var secondArgAsCSharpString = PrologCodeListToCSharpString(secondArg);

            if (secondArgAsCSharpString == string.Empty)
            {
                return null;
            }
            else if (secondArgAsCSharpString != null)
            {
                var canGenerateNumber = goalName == "number_codes" || goalName == "name";
                var canGenerateAtom = goalName == "atom_codes" || goalName == "name";
                int secondArgAsCSharpInt;
                double secondArgAsCSharpDouble;

                if (canGenerateNumber && int.TryParse(secondArgAsCSharpString, out secondArgAsCSharpInt))
                {
                    // Use Unify() because firstArgAsVariable could be a non-binding variable.
                    return firstArg.Unify(new PrologIntegerLiteral(secondArgAsCSharpInt));
                }
                else if (canGenerateNumber && double.TryParse(secondArgAsCSharpString, out secondArgAsCSharpDouble))
                {
                    // Use Unify() because firstArgAsVariable could be a non-binding variable.
                    return firstArg.Unify(new PrologFloatLiteral(secondArgAsCSharpDouble));
                }
                else if (canGenerateAtom)
                {
                    // Use Unify() because firstArgAsVariable could be a non-binding variable.
                    return firstArg.Unify(CreateAtom(secondArgAsCSharpString));
                }
            }
            else if (firstArgIsAtom || (firstArg is IPrologNumber))
            {
                var firstArgAsCodesList = CSharpStringToPrologCodeList(firstArg.ToString());

                return secondArg.Unify(firstArgAsCodesList);
            }

            return null;
        }

        private PrologSubstitution AtomicChars(PrologGoal goal)
        {
            var goalName = goal.Name.Name;
            var firstArg = goal.ExpressionList[0];
            var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
            var firstArgIsAtom = firstArgAsFunctorExpression != null && firstArgAsFunctorExpression.ExpressionList.Count == 0;
            var secondArg = goal.ExpressionList[1];
            var secondArgAsCSharpString = PrologCharListToCSharpString(secondArg);

            if (secondArgAsCSharpString == string.Empty)
            {
                return null;
            }
            else if (secondArgAsCSharpString != null)
            {
                var canGenerateNumber = goalName == "number_chars";
                var canGenerateAtom = goalName == "atom_chars";
                int secondArgAsCSharpInt;
                double secondArgAsCSharpDouble;

                if (canGenerateNumber && int.TryParse(secondArgAsCSharpString, out secondArgAsCSharpInt))
                {
                    // Use Unify() because firstArgAsVariable could be a non-binding variable.
                    return firstArg.Unify(new PrologIntegerLiteral(secondArgAsCSharpInt));
                }
                else if (canGenerateNumber && double.TryParse(secondArgAsCSharpString, out secondArgAsCSharpDouble))
                {
                    // Use Unify() because firstArgAsVariable could be a non-binding variable.
                    return firstArg.Unify(new PrologFloatLiteral(secondArgAsCSharpDouble));
                }
                else if (canGenerateAtom)
                {
                    // Use Unify() because firstArgAsVariable could be a non-binding variable.
                    return firstArg.Unify(CreateAtom(secondArgAsCSharpString));
                }
            }
            else if (firstArgIsAtom || (firstArg is IPrologNumber))
            {
                var firstArgAsCodesList = CSharpStringToPrologCharList(firstArg.ToString());

                return secondArg.Unify(firstArgAsCodesList);
            }

            return null;
        }

        private PrologSubstitution CharCode(PrologGoal goal)
        {
            var goalName = goal.Name.Name;
            var firstArg = goal.ExpressionList[0];
            var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
            var firstArgIsChar = firstArgAsFunctorExpression != null &&
                firstArgAsFunctorExpression.ExpressionList.Count == 0 &&
                firstArgAsFunctorExpression.Name.Name.Length == 1;
            var firstArgAsVariable = firstArg as PrologVariable;
            var secondArg = goal.ExpressionList[1];
            var secondArgAsInteger = secondArg as PrologIntegerLiteral;

            if (firstArgIsChar)
            {
                var firstArgCharCode = new PrologIntegerLiteral((int)firstArgAsFunctorExpression.Name.Name[0]);

                return firstArgCharCode.Unify(secondArg);
            }
            else if (firstArgAsVariable != null && secondArgAsInteger != null && secondArgAsInteger.Value >= 0)
            {
                var c = (char)secondArgAsInteger.Value;

                // Use Unify() because firstArgAsVariable could be a non-binding variable.
                return firstArgAsVariable.Unify(CreateAtom(c.ToString()));
            }

            return null;
        }

        private PrologSubstitution AtomNumber(PrologGoal goal)
        {
            var firstArg = goal.ExpressionList[0];
            var secondArg = goal.ExpressionList[1];
            var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
            var firstArgIsAtom = firstArgAsFunctorExpression != null && firstArgAsFunctorExpression.ExpressionList.Count == 0;
            var firstArgAsVariable = firstArg as PrologVariable;
            var secondArgAsNumber = secondArg as IPrologNumber;

            if (firstArgIsAtom)
            {
                var firstArgAsCSharpString = firstArg.ToString();
                int firstArgAsCSharpInt;
                double firstArgAsCSharpDouble;

                if (int.TryParse(firstArgAsCSharpString, out firstArgAsCSharpInt))
                {
                    // Use Unify() because secondArg could be a non-binding variable.
                    return secondArg.Unify(new PrologIntegerLiteral(firstArgAsCSharpInt));
                }
                else if (double.TryParse(firstArgAsCSharpString, out firstArgAsCSharpDouble))
                {
                    // Use Unify() because secondArg could be a non-binding variable.
                    return secondArg.Unify(new PrologFloatLiteral(firstArgAsCSharpDouble));
                }
            }
            else if (firstArgAsVariable != null && secondArgAsNumber != null)
            {
                // Use Unify() because firstArgAsVariable could be a non-binding variable.
                return firstArgAsVariable.Unify(CreateAtom(secondArgAsNumber.ToString()));
            }

            return null;
        }

        private PrologSubstitution AtomLength(PrologGoal goal)
        {
            var firstArg = goal.ExpressionList[0];
            var secondArg = goal.ExpressionList[1];
            // firstArg can be an atom or a string.
            var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
            var firstArgIsAtom = firstArgAsFunctorExpression != null && firstArgAsFunctorExpression.ExpressionList.Count == 0;
            var firstArgAsCSharpString = PrologCodeListToCSharpString(firstArg);

            if (firstArgAsCSharpString != null)
            {
                // firstArg may be the empty list, in which case we want to yield 0 (the length of "") rather than 2 (the length of "[]").
            }
            else if (firstArgIsAtom)
            {
                firstArgAsCSharpString = firstArgAsFunctorExpression.Name.Name;
            }
            else
            {
                return null;
            }

            // firstArgAsCSharpString may be string.Empty
            return secondArg.Unify(new PrologIntegerLiteral(firstArgAsCSharpString.Length));
        }

        private void Print(PrologGoal unsubstitutedGoal, PrologGoal goal)
        {

            if (sbOutput.Length > 0)
            {
                sbOutput.AppendLine();
            }

#if OLD_PRINT
            sbOutput.Append(string.Join(" ", goal.ExpressionList));
#else
            var resultList = new List<string>();

            for (var i = 0; i < goal.ExpressionList.Count; ++i)
            {
                resultList.Add(string.Format("{0} = {1}", unsubstitutedGoal.ExpressionList[i], goal.ExpressionList[i]));
            }

            sbOutput.Append(string.Join(", ", resultList));

            if (allMode)
            {
                sbOutput.Append(";");
            }
#endif
        }

        private void AutomaticPrint(List<PrologVariable> variablesInQuery, PrologSubstitution substitution)
        {

            if (variablesInQuery == null)
            {
                return;
            }

            var resultList = new List<string>();

            foreach (var v in variablesInQuery)
            {
                var value = v.ApplySubstitution(substitution);

                if (!value.Equals(v)) // Avoid printing identity results such as "X = X".
                {
                    resultList.Add(string.Format("{0} = {1}", v, value));
                }
            }

            if (resultList.Count == 0)
            {
                return;
            }

            if (sbOutput.Length > 0)
            {
                sbOutput.AppendLine();
            }

            sbOutput.Append(string.Join(", ", resultList));

            if (allMode)
            {
                sbOutput.Append(";");
            }
        }

        private PrologSubstitution Is2(PrologGoal goal)
        {
            var rhsEvaluated = goal.ExpressionList[1].EvaluateToNumber();

            if (rhsEvaluated == null)
            {
                return null;
            }

            if (goal.ExpressionList[0] is IPrologNumber)
            {

                if (goal.ExpressionList[0].Equals(rhsEvaluated)) // Remember that the int 1 does not equal the double 1.0 according to this code.
                {
                    return new PrologSubstitution();
                }
            }
            else if (goal.ExpressionList[0] is PrologVariable)
            {
                //var newSubstitution = new PrologSubstitution((PrologVariable)goal.ExpressionList[0], rhsEvaluated);
                // Use Unify() because goal.ExpressionList[0] could be a non-binding variable.
                return goal.ExpressionList[0].Unify(rhsEvaluated);
            }

            return null;
        }

        private PrologSubstitution Unifiable2(PrologGoal goal)
        {
            return goal.ExpressionList[0].Unify(goal.ExpressionList[1]);
        }

        private PrologSubstitution NotUnifiable2(PrologGoal goal)
        {
            
            if (goal.ExpressionList[0].Unify(goal.ExpressionList[1]) != null)
            {
                return null;
            }

            return new PrologSubstitution();
        }

        private PrologSubstitution Equals2(PrologGoal goal)
        {

            if (goal.ExpressionList[0].Equals(goal.ExpressionList[1]))
            {
                return new PrologSubstitution();
            }

            return null;
        }

        private PrologSubstitution NotEquals2(PrologGoal goal)
        {

            if (goal.ExpressionList[0].Equals(goal.ExpressionList[1]))
            {
                return null;
            }

            return new PrologSubstitution();
        }

        private PrologSubstitution Open3(PrologGoal goal)
        {
            var firstArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
            var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;

            if (firstArgAsFunctorExpression == null || secondArgAsFunctorExpression == null ||
                firstArgAsFunctorExpression.ExpressionList.Count > 0 || secondArgAsFunctorExpression.ExpressionList.Count > 0)
            {
                //sbOutput.AppendLine("open error 1.");
                return null;
            }

            switch (secondArgAsFunctorExpression.Name.Name)
            {
                case "read":
                    var fileReader = new PrologFileReader(firstArgAsFunctorExpression.Name.Name, this);

                    if (fileReader.IsNull)
                    {
                        //sbOutput.AppendLine("open error 2.");
                        break;
                    }

                    return goal.ExpressionList[2].Unify(fileReader);

                case "write":
                case "append":
                    var fileWriter = new PrologFileWriter(firstArgAsFunctorExpression.Name.Name,
                        secondArgAsFunctorExpression.Name.Name == "append", this);

                    if (fileWriter.IsNull)
                    {
                        break;
                    }

                    return goal.ExpressionList[2].Unify(fileWriter);

                default:
                    break;
            }

            return null;
        }

        private PrologSubstitution Close1(PrologGoal goal)
        {
            var firstArgAsFileReader = goal.ExpressionList[0] as PrologFileReader;
            var firstArgAsFileWriter = goal.ExpressionList[0] as PrologFileWriter;

            if (firstArgAsFileReader != null)
            {
                firstArgAsFileReader.Close();
                return new PrologSubstitution();
            }
            else if (firstArgAsFileWriter != null)
            {
                firstArgAsFileWriter.Close();
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution AtEndOfStream1(PrologGoal goal)
        {
            var firstArgAsFileReader = goal.ExpressionList[0] as PrologFileReader;

            if (firstArgAsFileReader != null && firstArgAsFileReader.IsEOF)
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Read2(PrologGoal goal)
        {
            var firstArgAsFileReader = goal.ExpressionList[0] as PrologFileReader;

            if (firstArgAsFileReader == null)
            {
                //sbOutput.AppendLine("read error 1.");
                return null;
            }

            var readAtom = firstArgAsFileReader.Read( /* sbOutput */ );

            if (readAtom == null)
            {
                //sbOutput.AppendLine("read error 2.");
                return null;
            }

            return goal.ExpressionList[1].Unify(readAtom);
        }

        private PrologSubstitution GetCode2(PrologGoal goal)
        {
            var firstArgAsFileReader = goal.ExpressionList[0] as PrologFileReader;

            if (firstArgAsFileReader == null)
            {
                return null;
            }

            var readCharacterCode = new PrologIntegerLiteral(firstArgAsFileReader.GetCode());

            return goal.ExpressionList[1].Unify(readCharacterCode);
        }

        private PrologSubstitution Write1(PrologGoal goal)
        {

            if (goal.ExpressionList[0] is PrologVariable)
            {
                return null;
            }
            else
            {
                sbOutput.Append(goal.ExpressionList[0].ToString());
                return new PrologSubstitution();
            }
        }

        private PrologSubstitution Write2(PrologGoal goal)
        {
            var firstArgAsFileWriter = goal.ExpressionList[0] as PrologFileWriter;

            if (firstArgAsFileWriter != null && !(goal.ExpressionList[1] is PrologVariable) &&
                firstArgAsFileWriter.Write(goal.ExpressionList[1]))
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Tab1(PrologGoal goal) // See http://www.swi-prolog.org/pldoc/man?predicate=tab/1
        {
            var firstArgAsIntegerLiteral = goal.ExpressionList[0] as PrologIntegerLiteral;

            if (firstArgAsIntegerLiteral == null || firstArgAsIntegerLiteral.Value <= 0)
            {
                return null;
            }

            for (var i = 0; i < firstArgAsIntegerLiteral.Value; ++i)
            {
                sbOutput.Append(' ');
            }

            return new PrologSubstitution();
        }

        private PrologSubstitution Tab2(PrologGoal goal)
        {
            var firstArgAsFileWriter = goal.ExpressionList[0] as PrologFileWriter;
            var secondArgAsIntegerLiteral = goal.ExpressionList[1] as PrologIntegerLiteral;

            if (firstArgAsFileWriter != null && secondArgAsIntegerLiteral != null && //secondArgAsIntegerLiteral.Value > 0 &&
                firstArgAsFileWriter.Tab(secondArgAsIntegerLiteral.Value))
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution NL0(PrologGoal goal)
        {
            sbOutput.AppendLine();
            return new PrologSubstitution();
        }

        private PrologSubstitution NL1(PrologGoal goal)
        {
            var firstArgAsFileWriter = goal.ExpressionList[0] as PrologFileWriter;

            if (firstArgAsFileWriter != null && firstArgAsFileWriter.NewLine())
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Atom1(PrologGoal goal)
        {
            var firstAndOnlyArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;

            if (firstAndOnlyArgAsFunctorExpression != null && firstAndOnlyArgAsFunctorExpression.ExpressionList.Count == 0)
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Integer1(PrologGoal goal)
        {

            if (goal.ExpressionList[0] is PrologIntegerLiteral)
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Float1(PrologGoal goal)
        {

            if (goal.ExpressionList[0] is PrologFloatLiteral)
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Number1(PrologGoal goal)
        {

            if (goal.ExpressionList[0] is IPrologNumber)
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Atomic1(PrologGoal goal)
        {
            var firstAndOnlyArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
            var firstAndOnlyArgIsAtom = firstAndOnlyArgAsFunctorExpression != null && firstAndOnlyArgAsFunctorExpression.ExpressionList.Count == 0;

            if (firstAndOnlyArgIsAtom || goal.ExpressionList[0] is IPrologNumber)
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Var1(PrologGoal goal)
        {

            if (goal.ExpressionList[0] is PrologVariable)
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution NonVar1(PrologGoal goal)
        {

            if (goal.ExpressionList[0] is PrologVariable)
            {
                return null;
            }
            else
            {
                return new PrologSubstitution();
            }
        }

        private PrologSubstitution Ground1(PrologGoal goal)
        {

            if (goal.ExpressionList[0].IsGround)
            {
                return new PrologSubstitution();
            }
            else
            {
                return null;
            }
        }

        private PrologSubstitution Random2(PrologGoal goal)
        {
            var firstArgAsIntegerLiteral = goal.ExpressionList[0] as PrologIntegerLiteral;

            if (firstArgAsIntegerLiteral == null || firstArgAsIntegerLiteral.Value <= 0 || !(goal.ExpressionList[1] is PrologVariable))
            {
                return null;
            }

            var randomNumber = random.Next(firstArgAsIntegerLiteral.Value);

            // Use Unify() because goal.ExpressionList[1] could be a non-binding variable.
            return goal.ExpressionList[1].Unify(new PrologIntegerLiteral(randomNumber));
        }

        private PrologSubstitution Fail0(PrologGoal goal)
        {
            return null;
        }

        // "listing" and "listing(targetName)"; see http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse4

        private PrologSubstitution Listing0(PrologGoal goal)
        {

            foreach (var moduleName in dictModules.Keys)
            {
                sbOutput.AppendLine(string.Format("Module '{0}' :", moduleName));
                dictModules[moduleName].ClauseList.ForEach(clause => sbOutput.AppendLine(clause.ToString()));
                sbOutput.AppendLine();
            }

            sbOutput.AppendLine("Default module:");
            DefaultModule.ClauseList.ForEach(clause => sbOutput.AppendLine(clause.ToString()));
            return new PrologSubstitution();
        }

        private PrologSubstitution Listing1(PrologGoal goal)
        {
            string targetName = null;
            var functorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
            var variable = goal.ExpressionList[0] as PrologVariable;

            if (functorExpression != null && functorExpression.ExpressionList.Count == 0)
            {
                targetName = functorExpression.Name.Name; // The name probably does not begin with a capital letter or an underscore, unless it was single quoted.
            }
            else if (variable != null)
            {
                targetName = variable.Name; // The name begins with a capital letter or an underscore.
            }

            if (targetName == null)
            {
                return null;
            }

            // TODO? Should we include clauses that contain mentions of targetName on their RHSs?

            foreach (var moduleName in dictModules.Keys)
            {
                sbOutput.AppendLine(string.Format("Module '{0}' :", moduleName));
                dictModules[moduleName].ClauseList
                    .Where(clause => clause.Lhs.Name.Name == targetName)
                    .ToList()
                    .ForEach(clause => sbOutput.AppendLine(clause.ToString()));
                sbOutput.AppendLine();
            }

            sbOutput.AppendLine("Default module:");
            DefaultModule.ClauseList
                .Where(clause => clause.Lhs.Name.Name == targetName)
                .ToList()
                .ForEach(clause => sbOutput.AppendLine(clause.ToString()));
            return new PrologSubstitution();
        }

        private PrologSubstitution Arg3(PrologGoal goal)
        {
            var firstArgAsInteger = goal.ExpressionList[0] as PrologIntegerLiteral;
            var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;

            if (firstArgAsInteger == null || firstArgAsInteger.Value <= 0 ||
                secondArgAsFunctorExpression == null || secondArgAsFunctorExpression.ExpressionList.Count < firstArgAsInteger.Value)
            {
                return null;
            }

            return secondArgAsFunctorExpression.ExpressionList[firstArgAsInteger.Value - 1].Unify(goal.ExpressionList[2]);
        }

        private PrologSubstitution Univ2(PrologGoal goal)
        {
            var firstArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
            var firstArgAsVariable = goal.ExpressionList[0] as PrologVariable;
            var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;
            var secondArgAsVariable = goal.ExpressionList[1] as PrologVariable;

            if (firstArgAsFunctorExpression != null)
            {
                var f = CreateAtom(firstArgAsFunctorExpression.Name);
                var list = new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor("."),
                    new List<IPrologExpression>() { f, CSharpListToPrologList(firstArgAsFunctorExpression.ExpressionList) });

                return list.Unify(goal.ExpressionList[1]);
            }
            else if (firstArgAsVariable != null)
            {
                var list = PrologListToCSharpList(goal.ExpressionList[1]);

                if (list == null || list.Count == 0)
                {
                    return null;
                }

                var f = list[0] as PrologNameExpression<PrologFunctor>;

                if (f == null || f.ExpressionList.Count != 0)
                {
                    return null;
                }

                // Unify instead of explicitly creating the substitution, because list may contain firstArgAsVariable,
                // or firstArgAsVariable may be non-binding.
                return firstArgAsVariable.Unify(new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, f.Name, list.Skip(1).ToList()));
            }
            else if (goal.ExpressionList[0] is IPrologNumber)
            {
                // Note: This case is not described in http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
                var f = CreateAtom(goal.ExpressionList[0].ToString());
                var list = CSharpListToPrologList(new List<IPrologExpression>() { f });

                return list.Unify(goal.ExpressionList[1]);
            }

            return null;
        }

        private PrologSubstitution FindAll3(PrologGoal goal, HashSet<PrologVariable> parentVariablesToAvoid, PrologModule currentModule)
        {
            var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;

            if (secondArgAsFunctorExpression == null)
            {
                return null;
            }

            var tempGoalList = new List<PrologGoal>() { secondArgAsFunctorExpression.ToGoal() }; // oldSubstitution has already been applied.
            var tempCutDetectorList = new List<CutDetector>() { null };
            var tempListOfCurrentModules = new List<PrologModule>() { currentModule };
            var cachedSolutionCollectionMode = solutionCollectionMode;
            var cachedFindAll_Expression = findAll_Expression;
            var cachedFindAll_ResultList = findAll_ResultList;
            var cachedAllMode = allMode;

            solutionCollectionMode = SolutionCollectionMode.FindAll;
            findAll_Expression = goal.ExpressionList[0];
            findAll_ResultList = new List<IPrologExpression>();
            allMode = true;

            try
            {
                ProveGoalList(tempGoalList, tempCutDetectorList, 0, new PrologSubstitution(), parentVariablesToAvoid, null, tempListOfCurrentModules);
                return goal.ExpressionList[2].Unify(CSharpListToPrologList(findAll_ResultList));
            }
            finally
            {
                solutionCollectionMode = cachedSolutionCollectionMode;
                findAll_Expression = cachedFindAll_Expression;
                findAll_ResultList = cachedFindAll_ResultList;
                allMode = cachedAllMode;
            }
        }

        private PrologSubstitution KaminPlus3(PrologGoal goal)
        {
            return KaminApplyBuiltInArithmeticOperator(goal, (x, y) => x + y);
        }

        private PrologSubstitution KaminMinus3(PrologGoal goal)
        {
            return KaminApplyBuiltInArithmeticOperator(goal, (x, y) => x - y);
        }

        private PrologSubstitution LessThan2(PrologGoal goal)
        {
            return ApplyBuiltInComparisonOperator(goal, (x, y) => x < y, (x, y) => x < y);
        }

        private PrologSubstitution GreaterThan2(PrologGoal goal)
        {
            return ApplyBuiltInComparisonOperator(goal, (x, y) => x > y, (x, y) => x > y);
        }

        private PrologSubstitution EqualOrLessThan2(PrologGoal goal)
        {
            return ApplyBuiltInComparisonOperator(goal, (x, y) => x <= y, (x, y) => x <= y);
        }

        private PrologSubstitution GreaterThanOrEqual2(PrologGoal goal)
        {
            return ApplyBuiltInComparisonOperator(goal, (x, y) => x >= y, (x, y) => x >= y);
        }

        private PrologSubstitution ArithmeticEqual2(PrologGoal goal)
        {
            return ApplyBuiltInComparisonOperator(goal, (x, y) => x == y, (x, y) => x == y);
        }

        private PrologSubstitution ArithmeticNotEqual2(PrologGoal goal)
        {
            return ApplyBuiltInComparisonOperator(goal, (x, y) => x != y, (x, y) => x != y);
        }

#if SUPPORT_USER_DEFINED_OPERATORS
        private PrologGoal ParseCaretList(IPrologExpression expr, List<PrologVariable> variablesToExcludeFromKey)
        {
            var fe = expr as PrologNameExpression<PrologFunctor>;

            if (fe != null && fe.Name.Name == "^" && fe.ExpressionList.Count == 2)
            {
                var v = fe.ExpressionList[0] as PrologVariable;

                if (v == null)
                {
                    //throw new Exception(string.Format("ParseCaretList() : '{0}' is not a variable.", fe.ExpressionList[0]));
                    return null;
                }

                variablesToExcludeFromKey.Add(v);
                return ParseCaretList(fe.ExpressionList[1], variablesToExcludeFromKey);
            }

            return ExpressionToGoal(expr);
        }
#endif

        private PrologSubstitution BagOfSetOf3(
            PrologGoal goal,
            List<PrologGoal> goalList,
            List<CutDetector> cutDetectorList,
            int goalNum,
            PrologSubstitution oldSubstitution,
            HashSet<PrologVariable> parentVariablesToAvoid,
            List<PrologVariable> variablesInQuery,
            List<PrologModule> listOfCurrentModules)
        {
            var isSetOf = goal.Name.Name == "setof";
#if SUPPORT_USER_DEFINED_OPERATORS
            var variablesToExcludeFromKey = new List<PrologVariable>();
            var tempGoal = ParseCaretList(goal.ExpressionList[1], variablesToExcludeFromKey);

            if (tempGoal == null)
            {
                return null;
            }
#else
            // The second argument may be a PrologNameExpression<PrologFunctor> or a CaretList.
            var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;
            var secondArgAsCaretList = goal.ExpressionList[1] as CaretList;
            List<PrologVariable> variablesToExcludeFromKey;
            PrologGoal tempGoal;

            if (secondArgAsFunctorExpression != null)
            {
                variablesToExcludeFromKey = new List<PrologVariable>();
                tempGoal = secondArgAsFunctorExpression.ToGoal();
            }
            else if (secondArgAsCaretList != null)
            {
                variablesToExcludeFromKey = secondArgAsCaretList.VariableList;
                tempGoal = secondArgAsCaretList.FunctorExpression.ToGoal();
            }
            else
            {
                return null;
            }
#endif

            var tempGoalList = new List<PrologGoal>() { tempGoal };
            // Get the list of variables in the temp goal, and from that list remove all variables in the caret list (Var1 ^ Var2 ^ ... ^ goal).
            // This resulting variable list will be used, after substitutions, as the keys for the dictionary below.
            var tempCutDetectorList = new List<CutDetector>() { cutDetectorList[goalNum] };
            var tempListOfCurrentModules = new List<PrologModule>() { listOfCurrentModules[goalNum] };
            var cachedSolutionCollectionMode = solutionCollectionMode;
            var cachedFindAll_Expression = findAll_Expression;
            var cachedCaretListVariables = caretListVariables;
            var cachedDictSolutions = dictSolutions;
            var cachedAllMode = allMode;
            // The results will be stored in a dictionary, where the key type is a hashable wrapper for List<IPrologExpression>,
            // and the value type is List<IPrologExpression>.
            //PrologSubstitution unifier = null;
            List<ExpressionListAsKey> resultKeys;
            List<List<IPrologExpression>> resultValues;
            var findAll_Expression_asVariable = goal.ExpressionList[0] as PrologVariable;

            solutionCollectionMode = SolutionCollectionMode.BagOfOrSetOf;
            findAll_Expression = goal.ExpressionList[0];
            // caretListVariables is actually the binding variables in tempGoal that do not occur in variablesToExcludeFromKey.
            // Perhaps a better name would be solutionDictKeyVariables.
            caretListVariables = tempGoal.GetListOfBindingVariables().Where(v => !variablesToExcludeFromKey.Contains(v)).ToList();

            if (findAll_Expression_asVariable != null)
            {
                caretListVariables = caretListVariables.Where(v => !v.Equals(findAll_Expression_asVariable)).ToList();
            }

            dictSolutions = new Dictionary<ExpressionListAsKey, List<IPrologExpression>>();
            allMode = true;

            var solutionDictKeyVariables = caretListVariables;

            try
            {
                ProveGoalList(tempGoalList, tempCutDetectorList, 0, new PrologSubstitution(),
                    parentVariablesToAvoid, null, tempListOfCurrentModules);
                // Use the dictionary to create a sorted List<> of keys from the dictionary,
                // along with a corresponding list of values.
                resultKeys = new List<ExpressionListAsKey>(dictSolutions.Keys);
                resultKeys.Sort();
                resultValues = resultKeys.Select(k => dictSolutions[k]).ToList();

                if (isSetOf)
                {
                    // Process each list of results to remove duplicates and to sort.
                    var newResultValues = new List<List<IPrologExpression>>();
                    var comparer = new SetOfComparer();

                    foreach (var rv in resultValues)
                    {
                        var new_rv = new List<IPrologExpression>();

                        new_rv.AddRangeUnique(rv);
                        new_rv.Sort(comparer);
                        newResultValues.Add(new_rv);
                    }

                    resultValues = newResultValues;
                }
            }
            finally
            {
                solutionCollectionMode = cachedSolutionCollectionMode;
                findAll_Expression = cachedFindAll_Expression;
                caretListVariables = cachedCaretListVariables;
                dictSolutions = cachedDictSolutions;
                allMode = cachedAllMode;
            }

            // Foreach key in the sorted list of keys:
            // - Create a substitution that unifies each part of the key with the corresponding variable from above,
            //   and compose it with the unification of goal.ExpressionList[2] and the value that corresponds to the key (from the dictionary).
            // - Compose this substitution with oldSubstitution and call ProveGoalList().  If non-null is returned, return it.
            // End foreach.
            // return null.

            for (var i = 0; i < resultKeys.Count; ++i)
            {
                var key = resultKeys[i];
                var value = resultValues[i];
                var unifier = new PrologSubstitution();

                for (var j = 0; j < solutionDictKeyVariables.Count; ++j)
                {
                    var expr1 = solutionDictKeyVariables[j].ApplySubstitution(unifier);
                    var expr2 = key.ExpressionList[j].ApplySubstitution(unifier);
                    var subst = expr1.Unify(expr2);

                    if (subst == null)
                    {
                        unifier = null;
                        break;
                    }

                    unifier = unifier.Compose(subst);
                }

                if (unifier == null)
                {
                    continue;
                }

                var expr3 = goal.ExpressionList[2].ApplySubstitution(unifier);
                var expr4 = CSharpListToPrologList(value).ApplySubstitution(unifier);
                var subst2 = expr3.Unify(expr4);

                if (subst2 == null)
                {
                    continue;
                }

                unifier = unifier.Compose(subst2);

                var result = ProveGoalList(goalList, cutDetectorList, goalNum + 1, oldSubstitution.Compose(unifier), parentVariablesToAvoid,
                    variablesInQuery, listOfCurrentModules);

                if (result != null)
                {
                    return result;
                }
            }

            return null;
        }

        private PrologSubstitution ProveGoalList(
            List<PrologGoal> goalList,
            List<CutDetector> cutDetectorList,
            int goalNum,
            PrologSubstitution oldSubstitution,
            HashSet<PrologVariable> parentVariablesToAvoid,
            List<PrologVariable> variablesInQuery, // Print these variables and their values automatically upon success if there is no print() goal at the end
            List<PrologModule> listOfCurrentModules)
        {

            if (goalNum >= goalList.Count)
            {
                // The goal list has been satisfied.

                // **** Begin automatic printing ****
                var lastGoal = (goalList.Count > 0) ? goalList[goalList.Count - 1] : null;

                if (lastGoal != null && lastGoal.Name.Name != "print") // Don't do automatic printing if the last goal was a print() goal.
                {
                    AutomaticPrint(variablesInQuery, oldSubstitution);
                }

                // **** End automatic printing ****

                // **** Begin findall/3, bagof/3, setof/3 support ****

                switch (solutionCollectionMode)
                {
                    case SolutionCollectionMode.FindAll:

                        if (findAll_Expression != null && findAll_ResultList != null)
                        {
                            findAll_ResultList.Add(findAll_Expression.ApplySubstitution(oldSubstitution));
                        }

                        break;

                    case SolutionCollectionMode.BagOfOrSetOf:

                        if (findAll_Expression != null && caretListVariables != null && dictSolutions != null)
                        {
                            var key = new ExpressionListAsKey(caretListVariables.Select(v => v.ApplySubstitution(oldSubstitution)).ToList());
                            var result = findAll_Expression.ApplySubstitution(oldSubstitution);

                            if (dictSolutions.ContainsKey(key))
                            {
                                dictSolutions[key].Add(result);
                            }
                            else
                            {
                                dictSolutions[key] = new List<IPrologExpression>() { result };
                            }
                        }

                        break;

                    default:
                        break;
                }

                // **** End findall/3, bagof/3, setof/3 support ****

                // To continue searching for other solutions (i.e. if we are operating in "all" mode rather than "first" mode), return null.

                if (allMode)
                {
                    return null;
                }

                return oldSubstitution;
            }

#if SUBSTITUTION_KEY_COUNT_LIMIT
            if (oldSubstitution.SubstitutionList.Keys.Count > 10)
            {
                Console.WriteLine("**** Aborting because the substitution is too long. ****");
                return null;
            }
#endif

            var unsubstitutedGoal = goalList[goalNum];
            var currentModule = listOfCurrentModules[goalNum];
            var nextGoalNum = goalNum + 1;

#if CONSOLE_WRITELINE
            Console.WriteLine("ProveGoal: unsubstitutedGoal = {0}; subst = {1}", unsubstitutedGoal, oldSubstitution);
#endif

            if (unsubstitutedGoal.IsCut)
            {
                // The "cut" goal always succeeds.
#if CONSOLE_WRITELINE
                Console.WriteLine("ProveGoal: Detected a cut.");
#endif

                // 2014/03/07
                var cutDetector = cutDetectorList[goalNum];
                var cutSubstitution = ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid,
                    variablesInQuery, listOfCurrentModules);

                if (cutSubstitution == null && cutDetector != null)
                {
                    // We may not backtrack through a cut.
                    throw new CutBacktrackException(cutDetector.Guid);
                }

                return cutSubstitution;
            }

            var goal = (PrologGoal)unsubstitutedGoal.ApplySubstitution(oldSubstitution);

#if CONSOLE_WRITELINE
            Console.WriteLine("ProveGoal: goal after substitution = {0}", goal);
#endif

            var numArgsInGoal = goal.ExpressionList.Count;
            var functionKey = new StringIntKey(goal.Name.Name, numArgsInGoal);

            if (dictBuiltInPredicates.ContainsKey(functionKey))
            {
                var unifier = dictBuiltInPredicates[functionKey](goal);

                if (unifier == null)
                {
                    return null;
                }

                return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
                    parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
            }

            if (gs == GrammarSelector.Prolog) // Built-in predicated that are used by Kamin's Prolog only.
            {

                switch (goal.Name.Name)
                {
                    case "print": // This can take any number of arguments.
                        Print(unsubstitutedGoal, goal);
                        return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
                            listOfCurrentModules);

                    default:
                        break;
                }
            }

            switch (goal.Name.Name)
            {
                case "not":
                case @"\+":

                    if (numArgsInGoal == 1)
                    {
                        //var fe = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
                        var fe = ConvertToFunctorExpression(goal.ExpressionList[0]);

                        if (fe == null)
                        {
                            return null;
                        }

                        var innerGoal = fe.ToGoal();
                        var tempGoalList = new List<PrologGoal>() { innerGoal };
                        // This next line prevents us from adding "not" to the built-in predicates dictionary:
                        var tempCutDetectorList = new List<CutDetector>() { cutDetectorList[goalNum] };
                        var tempListOfCurrentModules = new List<PrologModule>() { listOfCurrentModules[goalNum] };
                        var cachedAllMode = allMode;
                        var cachedSolutionCollectionMode = solutionCollectionMode;
                        PrologSubstitution localSubstitution = null;

                        allMode = false;
                        solutionCollectionMode = SolutionCollectionMode.None;

                        try
                        {
                            // We don't need to use parentVariablesToAvoid here, since we don't propagate localSubstitution.
                            localSubstitution = ProveGoalList(tempGoalList, tempCutDetectorList, 0, new PrologSubstitution(),
                                innerGoal.FindBindingVariables(), null, tempListOfCurrentModules);
                        }
                        finally
                        {
                            allMode = cachedAllMode;
                            solutionCollectionMode = cachedSolutionCollectionMode;
                        }

                        if (localSubstitution != null)
                        {
                            return null;
                        }
                        else
                        {
                            return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid,
                                variablesInQuery, listOfCurrentModules);
                        }
                    }

                    break;

                case "functor":
                    // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39

                    if (numArgsInGoal == 3) // functor/3
                    {
                        var functorSubstitution = Functor3(goal, parentVariablesToAvoid);

                        if (functorSubstitution == null)
                        {
                            return null;
                        }

                        return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(functorSubstitution),
                            parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
                    }

                    break;

                case "findall": // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49

                    if (numArgsInGoal == 3) // findall/3
                    {
                        var unifier = FindAll3(goal, parentVariablesToAvoid, currentModule);

                        if (unifier == null)
                        {
                            return null;
                        }

                        return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
                            parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
                    }

                    break;

                case "bagof": // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49
                case "setof":

                    if (numArgsInGoal == 3) // bagof/3 or setof/3
                    {
                        return BagOfSetOf3(goal, goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
                            listOfCurrentModules);
                    }

                    break;

                case "goal_disjunction":
                case ";":

                    if (numArgsInGoal == 2)
                    {
                        return GoalDisjunction2(goal, goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
                            listOfCurrentModules);
                    }

                    break;

                case "if_then_else":

                    if (numArgsInGoal == 3)
                    {
                        return IfThenElse3(goal.ExpressionList[0], goal.ExpressionList[1], goal.ExpressionList[2],
                            goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
                    }

                    break;

                case "->":

                    if (numArgsInGoal == 2)
                    {
                        var thenElseGoal = ExpressionToGoal(goal.ExpressionList[1]);

                        if (thenElseGoal != null && thenElseGoal.Name.Name == ":" && thenElseGoal.ExpressionList.Count == 2)
                        {
                            return IfThenElse3(goal.ExpressionList[0], thenElseGoal.ExpressionList[0], thenElseGoal.ExpressionList[1],
                                goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
                        }
                    }

                    break;

                case "retract":

                    if (numArgsInGoal == 1)
                    {
                        var unifier = Retract1(goal, oldSubstitution, parentVariablesToAvoid);

                        if (unifier == null)
                        {
                            return null;
                        }

                        return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
                            parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
                    }

                    break;

                case "retractall":

                    if (numArgsInGoal == 1)
                    {
                        var unifier = RetractAll1(goal, oldSubstitution, parentVariablesToAvoid);

                        if (unifier == null)
                        {
                            return null;
                        }

                        return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
                            parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
                    }

                    break;

                default:
                    break;
            }

            var resultSubstitution = ProveGoalListUsingModule(goal, goalList, cutDetectorList, nextGoalNum, oldSubstitution,
                parentVariablesToAvoid, variablesInQuery, currentModule, listOfCurrentModules);

            if (resultSubstitution != null)
            {
                return resultSubstitution;
            }

            var goalSignature = new StringIntKey(goal.Name.Name, goal.ExpressionList.Count);

            foreach (var import in currentModule.ImportList)
            {

                if (import.Key.Equals(goalSignature))
                {
                    resultSubstitution = ProveGoalListUsingModule(goal, goalList, cutDetectorList, nextGoalNum, oldSubstitution,
                        parentVariablesToAvoid, variablesInQuery, import.Value, listOfCurrentModules);

                    if (resultSubstitution != null)
                    {
                        return resultSubstitution;
                    }
                }
            }

            return null;
        }

        private PrologSubstitution ProveGoalListUsingModule(
            PrologGoal goal,
            List<PrologGoal> goalList,
            List<CutDetector> cutDetectorList,
            int nextGoalNum,
            PrologSubstitution oldSubstitution,
            HashSet<PrologVariable> parentVariablesToAvoid,
            List<PrologVariable> variablesInQuery, // Print these variables and their values automatically upon success if there is no print() goal at the end
            PrologModule currentModule,
            List<PrologModule> listOfCurrentModules)
        {
            var variablesToAvoid = goal.FindBindingVariables();

            variablesToAvoid.UnionWith(parentVariablesToAvoid);
            variablesToAvoid.UnionWith(oldSubstitution.FindBindingVariables());

#if CONSOLE_WRITELINE
            Console.WriteLine("ProveGoal: *** Trying to prove goal {0}", goal);
            Console.WriteLine("ProveGoal: oldSubstitution is: {0}", oldSubstitution);
#endif

            // Iterate over a copy of the ClauseList to protect against InvalidOperationExceptions due to assert*/retract*.
            var clauseListCopy = new List<PrologClause>(currentModule.ClauseList);

            foreach (var clause in clauseListCopy)
            {
                var newClause = clause.RenameVariables(variablesToAvoid, this);

#if CONSOLE_WRITELINE
                Console.WriteLine("ProveGoal: Trying to unify goal {0} with Lhs of clause {1}", goal, newClause);
#endif

                var unifier = newClause.Lhs.Unify(goal);

                if (unifier == null)
                {
#if CONSOLE_WRITELINE
                    Console.WriteLine("ProveGoal: Unification failed.");
#endif
                    continue;
                }

#if CONSOLE_WRITELINE
                Console.WriteLine("ProveGoal: goal unifies with Lhs of clause: {0}", newClause);
                Console.WriteLine("ProveGoal: unifier is: {0}", unifier);
                //Console.WriteLine("ProveGoal: Composing unifier with substitution: {0}", oldSubstitution);
#endif

                var localSubstitution = oldSubstitution.Compose(unifier);

#if CONSOLE_WRITELINE
                Console.WriteLine("ProveGoal: Composed substitution: {0}", localSubstitution);
#endif

                // See the program F2.16.txt for a test of the cut.
                var newVariablesToAvoid = GetVariablesFromGoalList(newClause.Rhs);

                newVariablesToAvoid.UnionWith(variablesToAvoid);

                // ThAW 2014/03/06 : We want to support cuts in goal disjunctions and if/then/else constructs.
                var cutDetector = new CutDetector();

                goalList.InsertRange(nextGoalNum, newClause.Rhs);

                // Insert as many copies of the cutDetector reference as we have subgoals in newClause.Rhs .
                cutDetectorList.InsertRange(nextGoalNum, newClause.Rhs.Select(g => cutDetector));
                listOfCurrentModules.InsertRange(nextGoalNum, newClause.Rhs.Select(g => currentModule));

                try
                {
                    localSubstitution = ProveGoalList(goalList, cutDetectorList, nextGoalNum, localSubstitution, newVariablesToAvoid,
                        variablesInQuery, listOfCurrentModules);
                }
                catch (CutBacktrackException ex) // 2014/03/07
                {
                    // Clean up the lists before we return or re-throw.
                    //goalList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
                    //cutDetectorList.RemoveRange(nextGoalNum, newClause.Rhs.Count);

                    if (ex.Guid.Equals(cutDetector.Guid))
                    {
                        return null;
                    }

                    throw;
                }
                finally
                {
                    goalList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
                    cutDetectorList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
                    listOfCurrentModules.RemoveRange(nextGoalNum, newClause.Rhs.Count);
                }

                if (localSubstitution != null)
                {
                    return localSubstitution;
                }
            }

#if CONSOLE_WRITELINE
            Console.WriteLine("ProveGoal: *** Could not prove goal {0}", goal);
            Console.WriteLine("ProveGoal: oldSubstitution is: {0}", oldSubstitution);
#endif
            return null;
        }

        private bool ClauseIsIsomorphicToMemberOfClauseList(PrologClause clause, PrologModule currentModule)
        {
            var variablesToAvoid = clause.FindBindingVariables();

            return currentModule.ClauseList.Any(otherClause => clause.IsIsomorphicTo(otherClause, variablesToAvoid, this));
        }

        private bool ClauseIsNoMoreGeneralThanMemberOfClauseList(PrologClause clause, PrologModule currentModule)
        {
            var variablesToAvoid = clause.FindBindingVariables();

            foreach (var otherClause in currentModule.ClauseList)
            {
                var renamedClause = otherClause.RenameVariables(variablesToAvoid, this);
                var unifier = renamedClause.Unify(clause);

                if (unifier != null && renamedClause.ApplySubstitution(unifier).Equals(clause))
                {
                    return true;
                }
            }

            return false;
        }

#if SUPPORT_USER_DEFINED_OPERATORS
        private OperatorType? ExpressionToOperatorType(IPrologExpression expr)
        {
            var fe = expr as PrologNameExpression<PrologFunctor>;

            if (fe == null || fe.ExpressionList.Count != 0)
            {
                return null;
            }

            switch (fe.Name.Name)
            {
                case "fx": return OperatorType.fx;
                case "fy": return OperatorType.fy;
                case "xfx": return OperatorType.xfx;
                case "xfy": return OperatorType.xfy;
                case "yfx": return OperatorType.yfx;
                case "xf": return OperatorType.xf;
                case "yf": return OperatorType.yf;
                default: return null;
            }
        }

        private bool AddOperator(int precedence, OperatorType opType, IPrologExpression expr)
        {
            var fe = expr as PrologNameExpression<PrologFunctor>;

            if (fe == null || fe.ExpressionList.Count != 0)
            {
                return false;
            }

            Operators.Add(new PrologOperator(precedence, opType, fe.Name.Name));
            return true;
        }

        public string LoadFile(string filename, string currentModuleName = "")
        {

            if (FileLoader == null)
            {
                throw new Exception("Load file command: The file loader is null.");
            }

            if (!filename.Contains('.'))
            {
                filename = filename + ".pl";
            }

            if (!filename.StartsWith(@"\") && !filename.Contains(':') // I.e. if  filename is relative, not absolute.
                && !string.IsNullOrEmpty(PathToDefaultDirectory))
            {
                filename = Path.Combine(PathToDefaultDirectory, filename);
            }

            return FileLoader.LoadFileUsingCompletedPath(filename, currentModuleName);
        }

        private string ProcessCommand(IPrologExpression expr, ref string currentModuleName)
        {
            var exprList = PrologListToCSharpList(expr);

            if (exprList != null)
            {

                foreach (var exprFromList in exprList)
                {
                    var filename_fe = exprFromList as PrologNameExpression<PrologFunctor>;

                    if (filename_fe == null || filename_fe.ExpressionList.Count != 0)
                    {
                        return string.Format("Load file command: Invalid filename '{0}'.", exprFromList);
                    }

#if !DEAD_CODE
                    LoadFile(filename_fe.Name.Name, currentModuleName);
#else
                    var filename = filename_fe.Name.Name;

                    //if (!filename.Contains('\\')
                    if (!filename.StartsWith(@"\") && !filename.Contains(':') // I.e. if  filename is relative, not absolute.
                        && !string.IsNullOrEmpty(PathToDefaultDirectory))
                    {
                        filename = Path.Combine(PathToDefaultDirectory, filename);
                    }

                    FileLoader.LoadFileUsingCompletedPath(filename, currentModuleName);
#endif
                }

                return string.Format("{0} file(s) loaded.", exprList.Count);
            }

            var fe = expr as PrologNameExpression<PrologFunctor>;

            if (fe != null)
            {

                if (fe.Name.Name == "op" && fe.ExpressionList.Count == 3)
                {
                    var opPredecence = fe.ExpressionList[0] as PrologIntegerLiteral;
                    var opType = ExpressionToOperatorType(fe.ExpressionList[1]);
                    var nameList = PrologListToCSharpList(fe.ExpressionList[2]);

                    if (opPredecence == null || !opType.HasValue /* || name == null */ )
                    {
                    }
                    else if (nameList != null)
                    {

                        foreach (var name in nameList)
                        {

                            if (!AddOperator(opPredecence.Value, opType.Value, name))
                            {
                                return InvalidCommand;
                            }
                        }

                        return OperatorAdded;
                    }
                    else if (AddOperator(opPredecence.Value, opType.Value, fe.ExpressionList[2]))
                    {
                        return OperatorAdded;
                    }
                }
                else if (fe.Name.Name == "module" && fe.ExpressionList.Count == 2)
                {
                    var name_fe = ConvertToFunctorExpression(fe.ExpressionList[0]);
                    var exportList = PrologListToCSharpList(fe.ExpressionList[1]);

                    if (name_fe == null || name_fe.ExpressionList.Count != 0 || exportList == null)
                    {
                        return "module/2 command: Syntax error.";
                    }

                    var processedExportList = new List<StringIntKey>();

                    foreach (var ex in exportList)
                    {
                        var ex_fe = ex as PrologNameExpression<PrologFunctor>;

                        if (ex_fe == null || ex_fe.Name.Name != "/" || ex_fe.ExpressionList.Count != 2)
                        {
                            return "module/2 command: Syntax error in export list (1).";
                        }

                        var exName_fe = ConvertToFunctorExpression(ex_fe.ExpressionList[0]);
                        var exArity = ex_fe.ExpressionList[1] as PrologIntegerLiteral;

#if DEAD_CODE
                        if (exName_fe == null || exName_fe.ExpressionList.Count != 0 || exArity == null || exArity.Value < 0)
                        {
                            return "module/2 command: Syntax error in export list (2).";
                        }
#else
                        if (exName_fe == null)
                        {
                            return "exName_fe is null.";
                        }
                        else if (exName_fe.ExpressionList.Count != 0)
                        {
                            return string.Format("exName_fe expression count is {0}, not zero.", exName_fe.ExpressionList.Count);
                        }
                        else if (exArity == null)
                        {
                            return string.Format("exArity ({0}) is null.", fe.ExpressionList[1]);
                        }
                        else if (exArity.Value < 0)
                        {
                            return string.Format("exArity is {0} < 0.", exArity.Value);
                        }
#endif
                        processedExportList.Add(new StringIntKey(exName_fe.Name.Name, exArity.Value));
                    }

                    var newModule = new PrologModule(processedExportList);

                    dictModules[name_fe.Name.Name] = newModule;

                    var currentModule = FindModule(currentModuleName);

                    foreach (var key in processedExportList)
                    {
                        currentModule.ImportList.Add(new KeyValuePair<StringIntKey, PrologModule>(key, newModule));
                    }

                    currentModuleName = name_fe.Name.Name;
                    return string.Format("The module '{0}' has been created.", name_fe.Name.Name);
                }
            }

            return InvalidCommand;
        }
#endif

        public string ProcessInput(object parseResult, ref string currentModuleName)
        {
#if SUPPORT_USER_DEFINED_OPERATORS
#if DEAD_CODE
            var inputAsFunctorExpression = parseResult as PrologNameExpression<PrologFunctor>;
            var inputAsVariable = parseResult as PrologVariable;

            if (inputAsVariable != null)
            {
                // Clause without RHS.
                parseResult = new PrologClause(
                    new PrologGoal(gs, new PrologPredicate(inputAsVariable.Name), new List<IPrologExpression>()),
                    new List<PrologGoal>());
            }
            else if (inputAsFunctorExpression != null)
            {

                if (inputAsFunctorExpression.Name.Name == "-->" && inputAsFunctorExpression.ExpressionList.Count == 2)
                {
                    // Definite Clause Grammar production rule.
                    var lhs = ConvertToFunctorExpression(inputAsFunctorExpression.ExpressionList[0]);

                    inputAsFunctorExpression = PrologGrammar2_LL1.GenerateDCGClause(lhs,
                        CommaSeparatedListToCSharpList(inputAsFunctorExpression.ExpressionList[1]));
                    // The result is a clause in the form of a functor expression.
                }

                parseResult = CreateClause(inputAsFunctorExpression);

                if (parseResult != null)
                {
                }
                else if (inputAsFunctorExpression.Name.Name == "?-" && inputAsFunctorExpression.ExpressionList.Count == 1)
                {
                    // Query.
                    parseResult = CommaSeparatedListToCSharpList(inputAsFunctorExpression.ExpressionList[0])
                        .Select(expr => ConvertToFunctorExpression(expr).ToGoal())
                        .ToList();
                }
                else if (inputAsFunctorExpression.Name.Name == ":-" && inputAsFunctorExpression.ExpressionList.Count == 1)
                {
                    return ProcessCommand(inputAsFunctorExpression.ExpressionList[0], ref currentModuleName);
                }
                else
                {
                    // Clause without RHS.
                    parseResult = new PrologClause(
                        new PrologGoal(gs, new PrologPredicate(inputAsFunctorExpression.Name.Name), inputAsFunctorExpression.ExpressionList),
                        new List<PrologGoal>());
                }
            }
#else
            var inputAsFunctorExpression = parseResult as PrologNameExpression<PrologFunctor>;

            if (inputAsFunctorExpression != null && inputAsFunctorExpression.Name.Name == ":-" && inputAsFunctorExpression.ExpressionList.Count == 1)
            {
                return ProcessCommand(inputAsFunctorExpression.ExpressionList[0], ref currentModuleName);
            }
#endif
#endif

            if (parseResult is PrologClause)
            {
                var clause = (PrologClause)parseResult;
                var currentModule = FindModule(currentModuleName);

                if (currentModule.ClauseList.Contains(clause))
                {
                    return ClauseAlreadyExists;
                }
                else if (ClauseIsIsomorphicToMemberOfClauseList(clause, currentModule))
                {
                    return IsomorphicClauseAlreadyExists;
                }
                else if (ClauseIsNoMoreGeneralThanMemberOfClauseList(clause, currentModule))
                {
                    return IsomorphicOrMoreGeneralClauseAlreadyExists;
                }

                //Console.WriteLine("Adding clause '{0}' to module '{1}'.", clause, currentModuleName);
                currentModule.ClauseList.Add(clause);
                return ClauseAdded;
            }
            else if (parseResult is List<PrologGoal>)
            {
                var goalList = new List<PrologGoal>((List<PrologGoal>)parseResult);
                var cutDetectorList = new List<CutDetector>();
                var listOfCurrentModules = new List<PrologModule>();
                var cutDetector = new CutDetector();
                PrologSubstitution substitution = null;

                sbOutput.Clear();
                goalList.ForEach(g => cutDetectorList.Add(cutDetector));
                goalList.ForEach(g => listOfCurrentModules.Add(DefaultModule));

                try
                {
                    substitution = ProveGoalList(goalList, cutDetectorList, 0, new PrologSubstitution(), GetVariablesFromGoalList(goalList),
                        GetListOfBindingVariablesFromGoalList(goalList), listOfCurrentModules);
                }
                catch (CutBacktrackException ex)
                {

                    if (!ex.Guid.Equals(cutDetector.Guid))
                    {
                        throw;
                    }
                }

                if (sbOutput.Length > 0)
                {
                    sbOutput.AppendLine();
                }

                sbOutput.Append(substitution != null ? Satisfied : NotSatisfied);
                return sbOutput.ToString();
            }
            else if (parseResult == null)
            {
                throw new Exception("PrologGlobalInfo.ProcessInput() : parseResult is null");
            }
            else
            {
                throw new Exception(string.Format("PrologGlobalInfo.ProcessInput() : parseResult is of unrecognized type {0}",
                    parseResult.GetType().FullName));
            }
        }

#if SUPPORT_USER_DEFINED_OPERATORS
        public static List<IPrologExpression> ExpressionToCSharpList(IPrologExpression expr)
        {
            var result = PrologListToCSharpList(expr);

            if (result != null)
            {
                return result;
            }

            return CommaSeparatedListToCSharpList(expr);
        }

        private IPrologExpression ParseList_Operators(List<Token> tokenList, int startIndex, TokenType openingDelimiter, TokenType closingDelimiter,
            out int nextIndex)
        {
            // Assert: tokenList[startIndex - 1].TokenType == openingDelimiter
            var delimiterCount = 1;
            var i = startIndex;

            for (; ; )
            {

                if (i >= tokenList.Count)
                {
                    throw new Exception("ParseList_Operators() : Unclosed list.");
                }

                if (tokenList[i].TokenType == openingDelimiter)
                {
                    ++delimiterCount;
                }
                else if (tokenList[i].TokenType == closingDelimiter)
                {
                    --delimiterCount;

                    if (delimiterCount == 0)
                    {
                        break;
                    }
                }

                ++i;
            }

            // The list to parse begins at startIndex and ends at i - 1 (inclusive).
            nextIndex = i + 1;
            return Parse_Operators(tokenList.Skip(startIndex).Take(i - startIndex).ToList());
        }

        private Stack<object> CloneOperatorSemanticStack(Stack<object> semanticStack)
        {
            var list = new List<object>();

            foreach (var obj in semanticStack)
            {
                var objAsOperatorUsage = obj as PrologOperatorUsage;

                if (objAsOperatorUsage != null)
                {
                    list.Add(objAsOperatorUsage.Clone());
                }
                else
                {
                    list.Add(obj);
                }
            }

            list.Reverse();

            return new Stack<object>(list);
        }

        public static List<IPrologExpression> CommaSeparatedListToCSharpList(IPrologExpression expr)
        {
            var exprAsFunctorExpression = expr as PrologNameExpression<PrologFunctor>;

            if (exprAsFunctorExpression == null || exprAsFunctorExpression.Name.Name != "," || exprAsFunctorExpression.ExpressionList.Count != 2)
            {
                return new List<IPrologExpression>() { expr };
            }
            else
            {
#if !DEAD_CODE
                var list = CommaSeparatedListToCSharpList(exprAsFunctorExpression.ExpressionList[1]);

                list.Insert(0, exprAsFunctorExpression.ExpressionList[0]);
#else
                var list = CommaSeparatedListToCSharpList(exprAsFunctorExpression.ExpressionList[0]);

                list.Add(exprAsFunctorExpression.ExpressionList[1]);
#endif
                return list;
            }
        }

        private IPrologExpression CSharpListToSequence(List<IPrologExpression> list, int index = 0)
        {

            if (index < 0 || index >= list.Count)
            {
                throw new Exception("CSharpListToSequence() : Invalid index.");
            }
            else if (index == list.Count - 1)
            {
                return list[index];
            }
            else
            {
                return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("consSeq"),
                    new List<IPrologExpression>() { list[index], CSharpListToSequence(list, index + 1)});
            }
        }

        private IPrologExpression AddAsPrologListTail(IPrologExpression expr, IPrologExpression tail)
        {
            var exprAsFunctorExpression = expr as PrologNameExpression<PrologFunctor>;

            if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "[]" && exprAsFunctorExpression.ExpressionList.Count == 0)
            {
                return tail;
            }
            else if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "." && exprAsFunctorExpression.ExpressionList.Count == 2)
            {
                return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("."),
                    new List<IPrologExpression>() { exprAsFunctorExpression.ExpressionList[0],
                        AddAsPrologListTail(exprAsFunctorExpression.ExpressionList[1], tail) });
            }
            else
            {
                throw new Exception("AddAsPrologListTail() : Failed.");
            }
        }

        private IPrologExpression CommaVBarExprToPrologList(IPrologExpression expr)
        {
            IPrologExpression firstExpr;
            IPrologExpression secondExpr;
            var exprAsFunctorExpression = expr as PrologNameExpression<PrologFunctor>;

            if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "," && exprAsFunctorExpression.ExpressionList.Count == 2)
            {
                firstExpr = exprAsFunctorExpression.ExpressionList[0];
                secondExpr = CommaVBarExprToPrologList(exprAsFunctorExpression.ExpressionList[1]);
            }
            else if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "|" && exprAsFunctorExpression.ExpressionList.Count == 2)
            {
                // Note that "|" binds more loosely than ",".
                firstExpr = exprAsFunctorExpression.ExpressionList[0];
                secondExpr = exprAsFunctorExpression.ExpressionList[1];
                return AddAsPrologListTail(CommaVBarExprToPrologList(firstExpr), secondExpr);
            }
            else
            {
                firstExpr = expr;
                secondExpr = CreateAtom("[]");
            }

            return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("."), new List<IPrologExpression>() { firstExpr, secondExpr });
        }

        private IPrologExpression ExpressionToSequence(IPrologExpression expr)
        {
            var fe = expr as PrologNameExpression<PrologFunctor>;

            if (fe != null && fe.Name.Name == "," && fe.ExpressionList.Count == 2)
            {
                return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("consSeq"),
                    new List<IPrologExpression>() { fe.ExpressionList[0], ExpressionToSequence(fe.ExpressionList[1]) });
            }

            return expr;
        }

        private IPrologExpression MarkCommaListAsNonDCG(IPrologExpression expr)
        {
            var exprAsFunctorExpression = expr as PrologNameExpression<PrologFunctor>;

            if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "," && exprAsFunctorExpression.ExpressionList.Count == 2)
            {
                var expr1AsFunctorExpression = ConvertToFunctorExpression(exprAsFunctorExpression.ExpressionList[0]);

                expr1AsFunctorExpression.DCGDoNotAddExtraArguments = true;
                return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor(","),
                    new List<IPrologExpression>() { expr1AsFunctorExpression, MarkCommaListAsNonDCG(exprAsFunctorExpression.ExpressionList[1]) });
            }
            else
            {
                exprAsFunctorExpression = ConvertToFunctorExpression(expr);

                exprAsFunctorExpression.DCGDoNotAddExtraArguments = true;
                return exprAsFunctorExpression;
            }
        }

        private bool BindValueToLeftOperator(PrologOperator leftOp, PrologOperator rightOp, out bool error)
        {
            error = false;

            if ((leftOp.IsPrefix || leftOp.IsInfix) && rightOp.IsPrefix)
            {
                return true;
            }
            else if (leftOp.IsPostfix && (rightOp.IsInfix || rightOp.IsPostfix))
            {
                return false;
            }
            else if (leftOp.IsPostfix && rightOp.IsPrefix)
            {
                error = true;
                return false;
            }
            else if (leftOp.Precedence < rightOp.Precedence)
            {
                return true;
            }
            else if (leftOp.Precedence > rightOp.Precedence)
            {
                return false;
            }
            else if ((leftOp.OpType == OperatorType.xfx || leftOp.OpType == OperatorType.yfx || leftOp.OpType == OperatorType.fx) &&
                (rightOp.OpType == OperatorType.yfx || rightOp.OpType == OperatorType.yf))
            {
                return true;
            }
            else if ((leftOp.OpType == OperatorType.xfy || leftOp.OpType == OperatorType.fy) &&
                (rightOp.OpType == OperatorType.xfx || rightOp.OpType == OperatorType.xfy || rightOp.OpType == OperatorType.xf))
            {
                return false;
            }
            else
            {
                error = true;
                return false;
            }
        }

        private IPrologExpression Parse_Operators_PushOp(PrologOperator op, List<Token> tokenList, int nextIndex, Stack<object> originalSemanticStack)
        {
            var semanticStack = CloneOperatorSemanticStack(originalSemanticStack);

            semanticStack.Push(new PrologOperatorUsage(op));

            // We have an operator.
            // Now see if the top three items on the stack are a value between two operators.
            // TODO: Also handle the case where a value is preceded by nothing and is followed by an operator (semanticStack.Count == 2).
            var done = false;

            while (!done)
            {
                done = true;

                if (semanticStack.Count == 0)
                {
                    return null;
                }
                else if (semanticStack.Count == 1)
                {
                    var onlyOp = semanticStack.Peek() as PrologOperatorUsage;

                    if (onlyOp != null && !onlyOp.Op.IsPrefix)
                    {
                        return null;
                    }
                }
                else
                //if (semanticStack.Count >= 2)
                {
                    var rightOp = (PrologOperatorUsage)semanticStack.Pop();
                    object rightOpAsObject = rightOp;

                    //Console.WriteLine("Parse_Operators_PushOp: rightOp is {0}.", rightOp.Op.Name);
                    //Console.WriteLine("Parse_Operators_PushOp: potential value has type {0}.", semanticStack.Peek().GetType().Name);

                    if (semanticStack.Peek() is IPrologExpression)
                    {
                        var inBetweenValue = (IPrologExpression)semanticStack.Pop();

                        if (semanticStack.Count == 0) // Not 2; we have just popped twice.
                        {
                            // Determine if rightOp is a postix operator.

                            //if (rightOp.Op.IsPostfix)
                            if (!rightOp.Op.IsPrefix)
                            {
                                //Console.WriteLine("Binding value {0} to rightOp {1}.", inBetweenValue, rightOp.Op.Name);
                                rightOp.Arguments.Add(inBetweenValue);
                                inBetweenValue = null;  // This value has been bound to an operator.

                                if (rightOp.IsComplete)
                                {
                                    rightOpAsObject = rightOp.ToFunctorExpression();
                                }
                            }
                        }
                        else if (semanticStack.Peek() is PrologOperatorUsage)
                        {
                            var leftOp = (PrologOperatorUsage)semanticStack.Pop();
                            object leftOpAsObject = leftOp;
                            bool error;
                            var bindToLeftOp = BindValueToLeftOperator(leftOp.Op, rightOp.Op, out error);

                            if (error)
                            {
                                //Console.WriteLine("BindValueToLeftOperator() : Cannot bind the value to either operator.");
                                return null;
                            }

                            if (bindToLeftOp)
                            {
                                //Console.WriteLine("Binding value {0} to leftOp {1}.", inBetweenValue, leftOp.Op.Name);
                                leftOp.Arguments.Add(inBetweenValue);

                                if (leftOp.IsComplete)
                                {
                                    //Console.WriteLine("leftOp {0} is now complete; will try to bind again.", leftOp.Op.Name);
                                    leftOpAsObject = leftOp.ToFunctorExpression();
                                    done = false;
                                }
                            }
                            else
                            {
                                //Console.WriteLine("Binding value {0} to rightOp {1}.", inBetweenValue, rightOp.Op.Name);
                                rightOp.Arguments.Add(inBetweenValue);

                                if (rightOp.IsComplete)
                                {
                                    rightOpAsObject = rightOp.ToFunctorExpression();
                                }
                            }

                            inBetweenValue = null;  // This value has been bound to an operator.
                            semanticStack.Push(leftOpAsObject);
                        }

                        if (inBetweenValue != null)
                        {
                            semanticStack.Push(inBetweenValue);
                        }
                    }
                    else if (!rightOp.Op.IsPrefix)
                    {
                        return null;
                    }

                    semanticStack.Push(rightOpAsObject);
                }
            }

            return Parse_Operators(tokenList, nextIndex, semanticStack);
        }

        private void FoldStackAtEnd(Stack<object> semanticStack)
        {

            while (semanticStack.Count > 1)
            {

                if (!(semanticStack.Peek() is IPrologExpression))
                {
                    //Console.WriteLine("FoldStackAtEnd: The object at the top of the stack is not an IPrologExpression.");
                    return;
                }

                var value = (IPrologExpression)semanticStack.Pop();

                //Console.WriteLine("FoldStackAtEnd: value is {0} ({1}).", value, value.GetType().Name);

                var leftOp = semanticStack.Peek() as PrologOperatorUsage;

                if (leftOp == null || leftOp.IsComplete || leftOp.Op.IsPostfix)
                {
#if DEAD_CODE
                    if (leftOp == null)
                    {
                        Console.WriteLine("FoldStackAtEnd: leftOp is null; top of stack is {0} ({1}).", semanticStack.Peek(), semanticStack.Peek().GetType().Name);
                    }
                    else if (leftOp.IsComplete)
                    {
                        Console.WriteLine("FoldStackAtEnd: leftOp is already complete.");
                    }
                    else if (leftOp.Op.IsPostfix)
                    {
                        Console.WriteLine("FoldStackAtEnd: leftOp is postfix.");
                    }
#endif
                    semanticStack.Push(value);
                    return;
                }

                leftOp.Arguments.Add(value);

                if (!leftOp.IsComplete) // This "if" block is probably unnecessary.
                {
                    //Console.WriteLine("FoldStackAtEnd: After adding an argument, leftOp {0} is not complete.", leftOp.Op.Name);
                    return;
                }

                semanticStack.Pop();
                semanticStack.Push(leftOp.ToFunctorExpression());
            }

            //Console.WriteLine("FoldStackAtEnd succeeded.");
        }

        private IPrologExpression Parse_Operators(List<Token> tokenList, int index = 0, Stack<object> originalSemanticStack = null)
        {
            Stack<object> semanticStack;

            if (originalSemanticStack == null)
            {
                semanticStack = new Stack<object>();
            }
            else
            {
                semanticStack = CloneOperatorSemanticStack(originalSemanticStack);
            }

            // The token list must end with a dot.

            if (index < 0 || index > tokenList.Count)
            {
                throw new Exception("Parse_Operators() : Index out of bounds.");
            }
            else if (index == tokenList.Count)
            {
                // Do the folding of objects (operators and their values) on the semantic stack so that no operator usage is incomplete.
                FoldStackAtEnd(semanticStack);
                // Then:

                if (semanticStack.Count == 1)
                {
                    // Is this object complete, and has it been converted to an IPrologExpression ?
                    var obj = semanticStack.Pop();

                    var objAsOperatorUsage = obj as PrologOperatorUsage;

                    if (objAsOperatorUsage != null)
                    {

                        if (!objAsOperatorUsage.IsComplete)
                        {
                            //Console.WriteLine("At end: Operator {0} is incomplete.", objAsOperatorUsage.Op.Name);
                            return null; // Syntax error.
                        }

                        return objAsOperatorUsage.ToFunctorExpression();
                    }
                    else
                    {
                        return (IPrologExpression)obj;
                    }
                }
                else
                {
                    //throw new Exception(string.Format("Parse_Operators() : Finishing up: There are {0} objects on the semantic stack.", semanticStack.Count));
                    //Console.WriteLine("At end: There are {0} objects on the semantic stack.", semanticStack.Count);
                    return null; // Syntax error.
                }
            }

            var token = tokenList[index];
            var value = token.TokenValue;
            var valueAsString = value.ToString();
            IPrologExpression currentExpr = null;
            IPrologExpression innerExpr;
            var nextIndex = index + 1;

            switch (token.TokenType)
            {
                case TokenType.T_IntLit:
                    currentExpr = new PrologIntegerLiteral((int)value);
                    break;

                case TokenType.T_FltLit:
                    currentExpr = new PrologFloatLiteral((double)value);
                    break;

                case TokenType.T_StrLit:
                    currentExpr = CSharpStringToPrologCodeList((string)value);
                    break;

                case TokenType.T_LeftBracket:
                    innerExpr = ParseList_Operators(tokenList, index + 1, TokenType.T_LeftBracket, TokenType.T_RightBracket, out nextIndex);

                    if (innerExpr == null)
                    {
                        return null;
                    }

                    //Console.WriteLine("Brackets: innerExpr is a {0}.", innerExpr.GetType().Name);

                    // If the object at the top of the stack is an atom or a variable, pop it and create a functor expression with arguments.

                    if (semanticStack.Count > 0)
                    {
                        //Console.WriteLine("Brackets: The top of the stack is a {0}.", semanticStack.Peek().GetType().Name);

                        var fe = semanticStack.Peek() as PrologNameExpression<PrologFunctor>;
                        var v = semanticStack.Peek() as PrologVariable;
                        var ou = semanticStack.Peek() as PrologOperatorUsage;
                        var inner_fe = innerExpr as PrologNameExpression<PrologFunctor>;

                        if ((fe != null && fe.ExpressionList.Count == 0) || v != null)
                        {
                            var obj = semanticStack.Pop();

                            currentExpr = new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor(obj.ToString()), CommaSeparatedListToCSharpList(innerExpr));
                        }
                        else if (ou != null && (ou.Arguments.Count != ou.ExpectedNumberOfArguments - 1 || (inner_fe != null && inner_fe.Name.Name == ","))
                            && ou.Op.Name != "," && ou.Op.Name != "|" && ou.Op.Name != "^") // The ^ check might be unnecessary.
                        {
                            return null;
                        }
                        else
                        {
                            currentExpr = ExpressionToSequence(innerExpr);
                        }
                    }
                    else
                    {
                        //Console.WriteLine("Brackets: The stack is empty.");
                        currentExpr = ExpressionToSequence(innerExpr);
                    }

                    break;

                case TokenType.T_LeftSquareBracket:

                    if (index + 1 < tokenList.Count && tokenList[index + 1].TokenType == TokenType.T_RightSquareBracket)
                    {
                        currentExpr = CreateAtom("[]");
                        nextIndex = index + 2;
                        break;
                    }

                    innerExpr = ParseList_Operators(tokenList, index + 1, TokenType.T_LeftSquareBracket, TokenType.T_RightSquareBracket, out nextIndex);

                    if (innerExpr == null)
                    {
                        return null;
                    }

                    currentExpr = CommaVBarExprToPrologList(innerExpr);
                    break;

                case TokenType.T_LeftCurlyBrace:
                    innerExpr = ParseList_Operators(tokenList, index + 1, TokenType.T_LeftCurlyBrace, TokenType.T_RightCurlyBrace, out nextIndex);

                    if (innerExpr == null)
                    {
                        return null;
                    }

                    currentExpr = MarkCommaListAsNonDCG(innerExpr);
                    break;

                case TokenType.T_StrLit2: // The contents of a single-quoted string.  It is never an operator.
                    currentExpr = CreateAtom(valueAsString);
                    break;

                default:
                    break;
            }

            if (currentExpr != null)
            {
                //Console.WriteLine("Pushing the value {0}.", currentExpr);
                semanticStack.Push(currentExpr);
                return Parse_Operators(tokenList, nextIndex, semanticStack);
            }

            // We will interpret the token (valueAsString) as an operator first, if possible, then as a functor.
            // Might an operator name begin with a capital letter?

            foreach (var op in Operators.Where(op => op.Name == valueAsString))
            {
                //Console.WriteLine("Attempting to match '{0}' to operator {1} ({2}, {3}).", valueAsString, op.Name, op.OpType, op.Precedence);

                var result = Parse_Operators_PushOp(op, tokenList, index + 1, semanticStack);

                if (result != null)
                {
                    return result;
                }
            }

            //if (tokenAsSymbol == Symbol.T_NameBeginningWithCapital)
            if (char.IsUpper(valueAsString, 0)
                // The following supports non-binding variables such as _ and _Foo .
                // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_3.html
                // TODO: Should we require the second character (if it exists) to be a capital letter if the first is an underscore?
                || valueAsString.StartsWith("_"))
            {
                currentExpr = new PrologVariable(valueAsString);
            }
            else
            {
                //Console.WriteLine("Creating the atom '{0}'.", valueAsString);
                currentExpr = CreateAtom(valueAsString);
                // TODO: Should we look for a left bracket immediately following, or should we wait until the left bracket is the current token?
            }

            semanticStack.Push(currentExpr);
            return Parse_Operators(tokenList, index + 1, semanticStack);
            // TODO: Do the folding of objects (operators and their values) on the semantic stack so that no operator usage is incomplete.  (?)
        }
#endif

        private object ParserDriver(List<Token> tokenList, bool parse)
        {

            if (tokenList.Count < 3 ||
                tokenList[tokenList.Count - 2].TokenType != TokenType.T_Dot ||
                tokenList[tokenList.Count - 1].TokenType != TokenType.T_EOF)
            {
                throw new Exception("The token list is too short, or it does not end with a dot and an EOF.");
            }

            object parseResult = Parse_Operators(tokenList.Take(tokenList.Count - 2).ToList());

            if (parseResult == null)
            {
                //throw new Exception("Syntax error: Parse_Operators() returned null.");
                throw new SyntaxException(string.Format("Syntax error: Parse_Operators() returned null; token list: {0}",
                    string.Join(" ", tokenList.Take(tokenList.Count - 1).Select(t => t.TokenValue)))); // Trim off the EOF.
            }

            var inputAsFunctorExpression = parseResult as PrologNameExpression<PrologFunctor>;
            var inputAsVariable = parseResult as PrologVariable;

            if (inputAsVariable != null)
            {
                // Clause without RHS.
                parseResult = new PrologClause(
                    new PrologGoal(gs, new PrologPredicate(inputAsVariable.Name), new List<IPrologExpression>()),
                    new List<PrologGoal>());
            }
            else if (inputAsFunctorExpression != null)
            {

                if (inputAsFunctorExpression.Name.Name == "-->" && inputAsFunctorExpression.ExpressionList.Count == 2)
                {
                    // Definite Clause Grammar production rule.
                    var lhs = ConvertToFunctorExpression(inputAsFunctorExpression.ExpressionList[0]);

                    inputAsFunctorExpression = PrologGrammar2_LL1.GenerateDCGClause(lhs,
                        CommaSeparatedListToCSharpList(inputAsFunctorExpression.ExpressionList[1]));
                    // The result is a clause in the form of a functor expression.
                }

                parseResult = CreateClause(inputAsFunctorExpression);

                if (parseResult != null)
                {
                }
                else if (inputAsFunctorExpression.Name.Name == "?-" && inputAsFunctorExpression.ExpressionList.Count == 1)
                {
                    // Query.
                    parseResult = CommaSeparatedListToCSharpList(inputAsFunctorExpression.ExpressionList[0])
                        .Select(expr => ConvertToFunctorExpression(expr).ToGoal())
                        .ToList();
                }
                else if (inputAsFunctorExpression.Name.Name == ":-" && inputAsFunctorExpression.ExpressionList.Count == 1)
                {
#if DEAD_CODE
                    return ProcessCommand(inputAsFunctorExpression.ExpressionList[0], ref currentModuleName);
#else
                    // Leave it as a functor expression; it will be detected and processed later.
                    parseResult = inputAsFunctorExpression;
#endif
                }
                else
                {
                    // Clause without RHS.
                    parseResult = new PrologClause(
                        new PrologGoal(gs, new PrologPredicate(inputAsFunctorExpression.Name.Name), inputAsFunctorExpression.ExpressionList),
                        new List<PrologGoal>());
                }
            }

            return parseResult;
        }

        public void Recognize(List<Token> tokenList)
        {
            ParserDriver(tokenList, false);
        }

        public object Parse(List<Token> tokenList)
        {
            return ParserDriver(tokenList, true);
        }

        public string ProcessTokenList(List<Token> tokenList, ref string currentModuleName)
        {
#if SUPPORT_USER_DEFINED_OPERATORS

            if (gs == GrammarSelector.Prolog2)
            {
#if DEAD_CODE
                if (tokenList.Count < 3 ||
                    tokenList[tokenList.Count - 2].TokenType != TokenType.T_Dot ||
                    tokenList[tokenList.Count - 1].TokenType != TokenType.T_EOF)
                {
                    throw new Exception("The token list is too short, or it does not end with a dot and an EOF.");
                }

                var parseResult = Parse_Operators(tokenList.Take(tokenList.Count - 2).ToList());

                if (parseResult == null)
                {
                    throw new Exception("Syntax error: Parse_Operators() returned null.");
                }

                return ProcessInput(parseResult, ref currentModuleName);
#else
                return ProcessInput(Parse(tokenList), ref currentModuleName);
#endif
            }
            else
            {
                return ProcessInput(parser.Parse(tokenList), ref currentModuleName);
            }
#else
            return ProcessInput(parser.Parse(tokenList));
#endif
        }

        public string ProcessInputString(string input)
        {
            string currentModuleName = string.Empty;

            return ProcessTokenList(tokenizer.Tokenize(input), ref currentModuleName);
        }

        public bool SetScoping(bool dynamicScoping)
        {
            return false;
        }

        public bool SetDebug(bool debug)
        {
            return false;
        }

        private PrologModule FindModule(string filePath)
        {

            if (dictModules.ContainsKey(filePath))
            {
                return dictModules[filePath];
            }
            else
            {
                return DefaultModule;
            }
        }
    }

    #endregion
}