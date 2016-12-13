using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter
{
    public class InterpreterGrammarBase : GrammarBase
    {
        public InterpreterGrammarBase()
            : base(Symbol.N_Start)
        {
            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_LeftBracket, Symbol.T_RightBracket, Symbol.T_Define,
                Symbol.T_If, Symbol.T_While, Symbol.T_Set, Symbol.T_Begin,
                Symbol.T_Plus, Symbol.T_Minus, Symbol.T_Multiply, Symbol.T_Divide,
                Symbol.T_Equals, Symbol.T_LessThan, //Symbol.T_GreaterThan,
                Symbol.T_Print, Symbol.T_ID, Symbol.T_IntegerLiteral, Symbol.T_Cond,
                Symbol.T_Let, Symbol.T_LetStar, Symbol.T_EOF });

            NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
                Symbol.N_Input, Symbol.N_Expression, Symbol.N_FunDef, Symbol.N_Function,
                Symbol.N_ArgList, Symbol.N_VariableList, Symbol.N_Variable, Symbol.N_Value,
                Symbol.N_BracketedExpression, Symbol.N_ExpressionList, Symbol.N_Optr, Symbol.N_ValueOp,
                Symbol.N_ExprPairList, Symbol.N_LetKeyword, Symbol.N_VarExprList });

            // From Chapter 1 of Kamin:
            // Input -> Expression
            // Input -> FunDef
            // FunDef -> ( define Function ArgList Expression )
            // ArgList -> ( VariableList )
            // VariableList -> Variable VariableList
            // VariableList -> Lambda
            // Expression -> Value
            // Expression -> Variable
            // Expression -> ( BracketedExpression )
            // BracketedExpression -> if Expression Expression Expression
            // BracketedExpression -> while Expression Expression
            // BracketedExpression -> set Variable Expression
            // BracketedExpression -> begin Expression ExpressionList
            // BracketedExpression -> Optr ExpressionList
            // ExpressionList -> Expression ExpressionList
            // ExpressionList -> Lambda
            // Optr -> Function
            // Optr -> Value-Op
            // Value -> Integer
            // Value-Op -> +
            // Value-Op -> -
            // Value-Op -> *
            // Value-Op -> /
            // Value-Op -> =
            // Value-Op -> <
            // Value-Op -> >
            // Value-Op -> print
            // Function -> Name
            // Variable -> Name
            // Integer -> ...
            // Name -> ...

            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Expression }, 2));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_FunDef }, 3));
            Productions.Add(new Production(Symbol.N_FunDef, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Define, Symbol.N_Function, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#functionDefinition" }, 4));
            Productions.Add(new Production(Symbol.N_ArgList, new List<object>() { Symbol.T_LeftBracket, Symbol.N_VariableList, Symbol.T_RightBracket }, 5));
            Productions.Add(new Production(Symbol.N_VariableList, new List<object>() { Symbol.N_Variable, Symbol.N_VariableList, "#variableList" }, 6));
            Productions.Add(new Production(Symbol.N_VariableList, new List<object>() { Symbol.Lambda, "#emptyVariableList" }, 7));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Value }, 8));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Variable }, 9));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_BracketedExpression, Symbol.T_RightBracket }, 10));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_If, Symbol.N_Expression, Symbol.N_Expression, Symbol.N_Expression, "#if" }, 11));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_While, Symbol.N_Expression, Symbol.N_Expression, "#while" }, 12));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Set, Symbol.N_Variable, Symbol.N_Expression, "#set" }, 13));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Begin, Symbol.N_Expression, Symbol.N_ExpressionList, "#begin" }, 14));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Optr, Symbol.N_ExpressionList, "#operatorUsage" }, 15));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#expressionList" }, 16));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExpressionList" }, 17));
            Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_Function }, 18));
            Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_ValueOp }, 19));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_IntegerLiteral }, 20));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Plus }, 21));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Minus }, 22));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Multiply }, 23));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Divide }, 24));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Equals }, 25));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_LessThan }, 26));
            //Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_GreaterThan }, 27));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Print }, 28));
            Productions.Add(new Production(Symbol.N_Function, new List<object>() { Symbol.T_ID }, 29));
            Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_ID, "#variable" }, 30));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
                Symbol.T_Cond,
                Symbol.T_LeftBracket,
                Symbol.N_Expression,
                Symbol.N_Expression,
                Symbol.T_RightBracket,
                Symbol.N_ExprPairList, "#condUsage" }, 31));
            Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() {
                Symbol.T_LeftBracket,
                Symbol.N_Expression,
                Symbol.N_Expression,
                Symbol.T_RightBracket,
                Symbol.N_ExprPairList, "#exprPairList" }, 32));
            Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() { Symbol.Lambda, "#emptyExprPairList" }, 33));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
                Symbol.N_LetKeyword,
                Symbol.T_LeftBracket,
                Symbol.N_VarExprList,
                Symbol.T_RightBracket,
                Symbol.N_Expression, "#letUsage" }, 34));
            Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_Let }, 35));
            Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetStar }, 36));
            Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() {
                Symbol.T_LeftBracket,
                Symbol.N_Variable,
                Symbol.N_Expression,
                Symbol.T_RightBracket,
                Symbol.N_VarExprList, "#varExprList" }, 37));
            Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() { Symbol.Lambda, "#emptyVarExprList" }, 38));
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            throw new ArgumentException(string.Format("Unrecognized semantic action: {0}", action), "action");
        }

        public override Symbol TokenToSymbol(Token token)
        {
            string tokenValueAsString = token.TokenValue.ToString();

            switch (token.TokenType)
            {
                case TokenType.T_Ident:

                    switch (tokenValueAsString)
                    {
                        case "define": return Symbol.T_Define;
                        case "if": return Symbol.T_If;
                        case "while": return Symbol.T_While;
                        case "set": return Symbol.T_Set;
                        case "begin": return Symbol.T_Begin;
                        case "print": return Symbol.T_Print;
                        case "+": return Symbol.T_Plus;
                        case "-": return Symbol.T_Minus;
                        case "*": return Symbol.T_Multiply;
                        case "/": return Symbol.T_Divide;
                        case "=": return Symbol.T_Equals;
                        case "<": return Symbol.T_LessThan;
                        //case ">": return Symbol.T_GreaterThan;
                        case "cond": return Symbol.T_Cond;
                        case "let": return Symbol.T_Let;
                        case "let*": return Symbol.T_LetStar;
                        default: break;
                    }

                    break;

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
                case Symbol.T_ID:
                case Symbol.T_Print:
                case Symbol.T_Plus:
                case Symbol.T_Minus:
                case Symbol.T_Multiply:
                case Symbol.T_Divide:
                case Symbol.T_Equals:
                case Symbol.T_LessThan:
                //case Symbol.T_GreaterThan:
                case Symbol.T_Let:
                case Symbol.T_LetStar:
                    semanticStack.Push(new Name(value as string, token.Line, token.Column));
                    break;

                default:
                    break;
            }
        }
    }
}
