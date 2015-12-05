using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Inference.Interpreter.JSON
{
    #region Interfaces

    public interface IValue
    {
        IValue EvaluateExpression(string expr);
    }

    #endregion

    #region JSONIntegerLiteral

    public class JSONIntegerLiteral : IValue
    {
        public readonly int Value;

        public JSONIntegerLiteral(int value)
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

            JSONIntegerLiteral otherIntLit = obj as JSONIntegerLiteral;

            return otherIntLit != null && Value == otherIntLit.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
        
        public IValue EvaluateExpression(string expr)
        {
            
            if (string.IsNullOrEmpty(expr))
            {
                return this;
            }

            throw new ArgumentException(string.Format("JSONIntegerLiteral.EvaluateExpression() : Expected null or empty string; received '{0}'", expr));
        }
    }

    #endregion

    #region JSONStringLiteral

    public class JSONStringLiteral : IValue
    {
        public readonly string Value;

        public JSONStringLiteral(string value)
        {
            Value = value;
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

            JSONStringLiteral otherStrLit = obj as JSONStringLiteral;

            return otherStrLit != null && Value == otherStrLit.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public IValue EvaluateExpression(string expr)
        {

            if (string.IsNullOrEmpty(expr))
            {
                return this;
            }

            throw new ArgumentException(string.Format("JSONStringLiteral.EvaluateExpression() : Expected null or empty string; received '{0}'", expr));
        }
    }

    #endregion

    #region JSONArray

    public class JSONArray : IValue
    {
        public readonly List<IValue> List = new List<IValue>();

        public JSONArray()
        {
        }

        public override string ToString()
        {
            return string.Format("[{0}]", string.Join(", ", List));
        }

        public IValue EvaluateExpression(string expr)
        {

            if (string.IsNullOrEmpty(expr))
            {
                return this;
            }

            if (expr[0] != '[')
            {
                throw new ArgumentException(string.Format("JSONObject.EvaluateExpression() : Expected string beginning with '.'; received '{0}'", expr));
            }

            var nextRightSquareBracket = expr.IndexOf(']', 1);

            if (nextRightSquareBracket < 0)
            {
                throw new Exception(string.Format("JSONObject.EvaluateExpression() : No ']' found in '{0}'", expr));
            }

            var indexAsString = expr.Substring(1, nextRightSquareBracket - 1);
            int indexAsInt;

            if (string.IsNullOrEmpty(indexAsString) || !int.TryParse(indexAsString, out indexAsInt))
            {
                throw new Exception(string.Format("JSONObject.EvaluateExpression() : The index '{0}' is not an integer", indexAsString));
            }
            else if (indexAsInt < 0 || indexAsInt >= List.Count)
            {
                throw new Exception(string.Format("JSONObject.EvaluateExpression() : Index {0} is out of bounds in a list of length {1}", indexAsInt, List.Count));
            }

            return List[indexAsInt].EvaluateExpression(expr.Substring(nextRightSquareBracket + 1));
        }
    }

    #endregion

    #region JSONObject

    public class JSONObject : IValue
    {
        public readonly Dictionary<string, IValue> Dict = new Dictionary<string,IValue>();

        public JSONObject()
        {
        }

        public override string ToString()
        {
            // In a format string, "{{" yields the character '{', and "}}" yields the character '}'.
            return string.Format("{{{0}}}", string.Join(", ", Dict.Keys.Select(k => string.Format("\"{0}\": {1}", k, Dict[k]))));
        }

        public IValue EvaluateExpression(string expr)
        {

            if (string.IsNullOrEmpty(expr))
            {
                return this;
            }

            if (expr[0] != '.')
            {
                throw new ArgumentException(string.Format("JSONObject.EvaluateExpression() : Expected string beginning with '.'; received '{0}'", expr));
            }

            var nextExpr = string.Empty;
            var key = expr.Substring(1);
            var nextDot = key.IndexOfAny(new char[] { '.', '[' });

            if (nextDot >= 0)
            {
                nextExpr = key.Substring(nextDot);
                key = key.Substring(0, nextDot);
            }

            if (!Dict.ContainsKey(key))
            {
                throw new Exception(string.Format("JSONObject.EvaluateExpression() : Key '{0}' not found in dictionary", key));
            }

            return Dict[key].EvaluateExpression(nextExpr);
        }
    }

    #endregion
}
