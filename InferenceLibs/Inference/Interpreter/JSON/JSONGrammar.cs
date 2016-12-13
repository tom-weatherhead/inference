using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.JSON
{
    public class JSONGrammar : GrammarBase
    {
        public JSONGrammar()
            : base(Symbol.N_Start)
        {
            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_IntegerLiteral, Symbol.T_StringLiteral, Symbol.T_LeftSquareBracket, Symbol.T_RightSquareBracket,
                Symbol.T_LeftCurlyBrace, Symbol.T_RightCurlyBrace, Symbol.T_Comma, Symbol.T_Colon,
                Symbol.T_EOF });

            NonTerminals.UnionWith(new HashSet<Symbol>() {
                Symbol.N_Start, Symbol.N_Value, Symbol.N_ValueListTail, Symbol.N_KeyValuePairListTail });

            // N_Value -> T_IntegerLiteral
            // N_Value -> T_DoubleQuotedStringLiteral
            // N_Value -> T_LeftSquareBracket N_Value N_ValueListTail T_RightSquareBracket
            // N_ValueListTail -> T_Comma N_Value N_ValueListTail
            // N_ValueListTail -> Lambda
            // N_Value -> T_LeftCurlyBrace T_DoubleQuotedStringLiteral T_Colon N_Value N_KeyValuePairListTail T_RightCurlyBrace
            // N_KeyValuePairListTail -> T_Comma T_DoubleQuotedStringLiteral T_Colon N_Value N_KeyValuePairListTail
            // N_KeyValuePairListTail -> Lambda
            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Value, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_IntegerLiteral }, 2));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 3));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftSquareBracket, Symbol.T_RightSquareBracket, "#emptyArray" }, 4));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftSquareBracket, Symbol.N_Value, Symbol.N_ValueListTail, Symbol.T_RightSquareBracket, "#array" }, 5));
            Productions.Add(new Production(Symbol.N_ValueListTail, new List<object>() { Symbol.T_Comma, Symbol.N_Value, Symbol.N_ValueListTail, "#array" }, 6));
            Productions.Add(new Production(Symbol.N_ValueListTail, new List<object>() { Symbol.Lambda, "#emptyArray" }, 7));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftCurlyBrace, Symbol.T_RightCurlyBrace, "#emptyObject" }, 8));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftCurlyBrace, Symbol.T_StringLiteral, Symbol.T_Colon, Symbol.N_Value, Symbol.N_KeyValuePairListTail, Symbol.T_RightCurlyBrace, "#object" }, 9));
            Productions.Add(new Production(Symbol.N_KeyValuePairListTail, new List<object>() { Symbol.T_Comma, Symbol.T_StringLiteral, Symbol.T_Colon, Symbol.N_Value, Symbol.N_KeyValuePairListTail, "#object" }, 10));
            Productions.Add(new Production(Symbol.N_KeyValuePairListTail, new List<object>() { Symbol.Lambda, "#emptyObject" }, 11));
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            IValue value;

            switch (action)
            {
                case "#array":
                    var array = (JSONArray)semanticStack.Pop();
                    value = (IValue)semanticStack.Pop();
                    array.List.Insert(0, value);
                    semanticStack.Push(array);
                    break;

                case "#emptyArray":
                    semanticStack.Push(new JSONArray());
                    break;

                case "#object":
                    var obj = (JSONObject)semanticStack.Pop();
                    value = (IValue)semanticStack.Pop();
                    var key = (JSONStringLiteral)semanticStack.Pop();

                    if (obj.Dict.ContainsKey(key.Value))
                    {
                        throw new SyntaxException(string.Format("JSON object: Duplicate key '{0}'", key.Value));
                    }

                    obj.Dict[key.Value] = value;
                    semanticStack.Push(obj);
                    break;

                case "#emptyObject":
                    semanticStack.Push(new JSONObject());
                    break;

                default:
                    throw new ArgumentException(string.Format("Unrecognized semantic action: {0}", action), "action");
            }
        }

        public override Symbol TokenToSymbol(Token token)
        {
            string tokenValueAsString = token.TokenValue.ToString();

            switch (token.TokenType)
            {
                case TokenType.T_StrLit: return Symbol.T_StringLiteral;
                case TokenType.T_LeftSquareBracket: return Symbol.T_LeftSquareBracket;
                case TokenType.T_RightSquareBracket: return Symbol.T_RightSquareBracket;
                case TokenType.T_LeftCurlyBrace: return Symbol.T_LeftCurlyBrace;
                case TokenType.T_RightCurlyBrace: return Symbol.T_RightCurlyBrace;
                case TokenType.T_Comma: return Symbol.T_Comma;
                case TokenType.T_Colon: return Symbol.T_Colon;
                default:
                    break;
            }

            return base.TokenToSymbol(token);
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            var value = token.TokenValue;

            switch (tokenAsSymbol)
            {
                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(new JSONIntegerLiteral((int)value));
                    break;

                case Symbol.T_StringLiteral:
                    semanticStack.Push(new JSONStringLiteral((string)value));
                    break;

                default:
                    break;
            }
        }
    }
}
