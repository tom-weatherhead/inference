using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.Chapter1
{
    #region IntegerLiteral

    public class IntegerLiteral : IExpression<int>
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

        public int Evaluate(EnvironmentFrame<int> localEnvironment, IGlobalInfo<int> globalInfo)
        {
            return Value;
        }
    }

    #endregion

    #region GlobalInfo

    public class GlobalInfo : GlobalInfoBase<int>
    {
        public GlobalInfo(ITokenizer tokenizer, IParser parser)
            : base(tokenizer, parser)
        {
        }

        public override void LoadPresets()
        {
            Evaluate("(define > (x y) (< y x))");
            Evaluate("(define mod (m n) (- m (* n (/ m n))))");
            Evaluate("(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))");
        }

        public override int FalseValue
        {
            get
            {
                return 0;
            }
        }

        public override int TrueValue
        {
            get
            {
                return 1;
            }
        }

        public override bool ValueIsInteger(int value)
        {
            return true;
        }

        public override int ValueAsInteger(int value)
        {
            return value;
        }

        public override int IntegerAsValue(int value)
        {
            return value;
        }
    }

    #endregion
}
