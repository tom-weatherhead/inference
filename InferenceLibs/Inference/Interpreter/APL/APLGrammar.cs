using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Parser;

namespace Inference.Interpreter.APL
{
    public class APLGrammar : InterpreterGrammarBase
    {
        public APLGrammar()
            : base()
        {
            Terminals.UnionWith(new HashSet<Symbol>() { 
                Symbol.T_Max, Symbol.T_Or, Symbol.T_And,
                Symbol.T_PlusSlash, Symbol.T_MinusSlash, Symbol.T_MultiplySlash, Symbol.T_DivideSlash,
                Symbol.T_MaxSlash, Symbol.T_OrSlash, Symbol.T_AndSlash,
                Symbol.T_Compress, Symbol.T_Shape, Symbol.T_Ravel, Symbol.T_Restruct,
                Symbol.T_Cat, Symbol.T_Indx, Symbol.T_Trans, Symbol.T_SquareBrackets,
                Symbol.T_Apostrophe, Symbol.T_Assign, Symbol.T_DoubleSubscripting, Symbol.T_FloatLiteral,
                Symbol.T_Random, Symbol.T_Pow, Symbol.T_Exp, Symbol.T_Ln,
                Symbol.T_Sin, Symbol.T_Cos, Symbol.T_Tan });

            NonTerminals.UnionWith(new HashSet<Symbol>() {
                Symbol.N_VectorConst, Symbol.N_IntegerLiteralList, Symbol.N_FloatLiteralList });

            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_VectorConst }, 39));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Max }, 40));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Or }, 41));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_And }, 42));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_PlusSlash }, 43));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_MinusSlash }, 44));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_MultiplySlash }, 45));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_DivideSlash }, 46));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_MaxSlash }, 47));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_OrSlash }, 48));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_AndSlash }, 49));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Compress }, 50));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Shape }, 51));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Ravel }, 52));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Restruct }, 53));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cat }, 54));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Indx }, 55));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Trans }, 56));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_SquareBrackets }, 57));
            // An empty vector is an int vector.
            Productions.Add(new Production(Symbol.N_VectorConst, new List<object>() { Symbol.T_Apostrophe, Symbol.T_LeftBracket, Symbol.N_IntegerLiteralList, Symbol.T_RightBracket, "#makeIntVector" }, 58));
            Productions.Add(new Production(Symbol.N_VectorConst, new List<object>() { Symbol.T_Apostrophe, Symbol.T_LeftBracket, Symbol.T_FloatLiteral, Symbol.N_FloatLiteralList, Symbol.T_RightBracket, "#makeFloatVector" }, 59));
            Productions.Add(new Production(Symbol.N_IntegerLiteralList, new List<object>() { Symbol.T_IntegerLiteral, Symbol.N_IntegerLiteralList, "#intList" }, 60));
            Productions.Add(new Production(Symbol.N_IntegerLiteralList, new List<object>() { Symbol.Lambda, "#emptyIntList" }, 61));
            Productions.Add(new Production(Symbol.N_FloatLiteralList, new List<object>() { Symbol.T_FloatLiteral, Symbol.N_FloatLiteralList, "#floatList" }, 62));
            Productions.Add(new Production(Symbol.N_FloatLiteralList, new List<object>() { Symbol.Lambda, "#emptyFloatList" }, 63));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Assign, Symbol.N_Variable, Symbol.N_Expression, Symbol.N_Expression, "#vecassign" }, 64));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_DoubleSubscripting }, 65));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 66));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Random }, 67));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Pow }, 68));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Exp }, 69));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Ln }, 70));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Sin }, 71));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cos }, 72));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Tan }, 73));
        }

        protected IExpression<IAPLValue> CreateLetUsage(string letKeyword,
            List<KeyValuePair<Variable<IAPLValue>, IExpression<IAPLValue>>> varExprList, IExpression<IAPLValue> expression)
        {

            switch (letKeyword)
            {
                case "let":
                    return new LetUsage<IAPLValue>(varExprList, expression);

                case "let*":
                    return new LetStarUsage<IAPLValue>(varExprList, expression);

                default:
                    throw new ArgumentException(string.Format("APLGrammar.CreateLetUsage() : Unknown 'let' keyword '{0}'.", letKeyword));
            }
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            Name name;
            Variable<IAPLValue> variable;
            VariableList<IAPLValue> variableList;
            IExpression<IAPLValue> expression;
            IExpression<IAPLValue> expression2;
            ExpressionList<IAPLValue> expressionList;
            List<int> intList;
            List<double> floatList;
            APLValue<int> intScalar;
            APLValue<double> floatScalar;
            List<KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>> exprPairList;
            List<KeyValuePair<Variable<IAPLValue>, IExpression<IAPLValue>>> varExprList;

            switch (action)
            {
                case "#functionDefinition":
                    var body = (IExpression<IAPLValue>)semanticStack.Pop();
                    var argList = (VariableList<IAPLValue>)semanticStack.Pop();
                    var functionName = (Name)semanticStack.Pop();

                    semanticStack.Push(new FunctionDefinition<IAPLValue>(functionName, argList, body));
                    break;

                case "#variableList":
                    variableList = (VariableList<IAPLValue>)semanticStack.Pop();
                    variable = (Variable<IAPLValue>)semanticStack.Pop();
                    variableList.Value.Insert(0, variable);
                    semanticStack.Push(variableList);
                    break;

                case "#emptyVariableList":
                    semanticStack.Push(new VariableList<IAPLValue>());
                    break;

                case "#if":
                    var expression3 = (IExpression<IAPLValue>)semanticStack.Pop();

                    expression2 = (IExpression<IAPLValue>)semanticStack.Pop();
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    semanticStack.Push(new APLIfUsage(expression, expression2, expression3));
                    break;

                case "#while":
                    expression2 = (IExpression<IAPLValue>)semanticStack.Pop();
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    semanticStack.Push(new APLWhileUsage(expression, expression2));
                    break;

                case "#set":
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    variable = (Variable<IAPLValue>)semanticStack.Pop();
                    semanticStack.Push(new SetUsage<IAPLValue>(variable, expression));
                    break;

                case "#begin":
                    expressionList = (ExpressionList<IAPLValue>)semanticStack.Pop();
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    semanticStack.Push(new BeginUsage<IAPLValue>(expression, expressionList));
                    break;

                case "#operatorUsage":
                    expressionList = (ExpressionList<IAPLValue>)semanticStack.Pop();

                    var operatorName = (Name)semanticStack.Pop();

                    semanticStack.Push(new APLOperatorUsage(operatorName, expressionList));
                    break;

                case "#expressionList":
                    expressionList = (ExpressionList<IAPLValue>)semanticStack.Pop();
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    expressionList.Value.Insert(0, expression);
                    semanticStack.Push(expressionList);
                    break;

                case "#emptyExpressionList":
                    semanticStack.Push(new ExpressionList<IAPLValue>());
                    break;

                case "#variable":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new Variable<IAPLValue>(name.Value, name.Line, name.Column));
                    break;

                case "#makeIntVector":
                    intList = (List<int>)semanticStack.Pop();
                    semanticStack.Push(APLValue<int>.CreateVector(intList));
                    break;

                case "#intList":
                    intList = (List<int>)semanticStack.Pop();
                    intScalar = (APLValue<int>)semanticStack.Pop();
                    intList.Insert(0, intScalar.GetFirstScalar());
                    semanticStack.Push(intList);
                    break;

                case "#emptyIntList":
                    semanticStack.Push(new List<int>());
                    break;

                case "#makeFloatVector":
                    floatList = (List<double>)semanticStack.Pop();
                    floatScalar = (APLValue<double>)semanticStack.Pop();
                    floatList.Insert(0, floatScalar.GetFirstScalar());
                    semanticStack.Push(APLValue<double>.CreateVector(floatList));
                    break;

                case "#floatList":
                    floatList = (List<double>)semanticStack.Pop();
                    floatScalar = (APLValue<double>)semanticStack.Pop();
                    floatList.Insert(0, floatScalar.GetFirstScalar());
                    semanticStack.Push(floatList);
                    break;

                case "#emptyFloatList":
                    semanticStack.Push(new List<double>());
                    break;

                case "#vecassign":
                    expression2 = (IExpression<IAPLValue>)semanticStack.Pop();
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    variable = (Variable<IAPLValue>)semanticStack.Pop();
                    semanticStack.Push(new VectorAssignmentUsage(variable, expression, expression2));
                    break;

                case "#condUsage":
                    exprPairList = (List<KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>>)semanticStack.Pop();
                    expression2 = (IExpression<IAPLValue>)semanticStack.Pop();
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>(expression, expression2));
                    semanticStack.Push(new APLCondUsage(exprPairList));
                    break;

                case "#exprPairList":
                    exprPairList = (List<KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>>)semanticStack.Pop();
                    expression2 = (IExpression<IAPLValue>)semanticStack.Pop();
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>(expression, expression2));
                    semanticStack.Push(exprPairList);
                    break;

                case "#emptyExprPairList":
                    semanticStack.Push(new List<KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>>());
                    break;

                case "#letUsage":
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    varExprList = (List<KeyValuePair<Variable<IAPLValue>, IExpression<IAPLValue>>>)semanticStack.Pop();

                    var letName = (Name)semanticStack.Pop();

                    semanticStack.Push(CreateLetUsage(letName.Value, varExprList, expression));
                    break;

                case "#varExprList":
                    varExprList = (List<KeyValuePair<Variable<IAPLValue>, IExpression<IAPLValue>>>)semanticStack.Pop();
                    expression = (IExpression<IAPLValue>)semanticStack.Pop();
                    variable = (Variable<IAPLValue>)semanticStack.Pop();
                    varExprList.Insert(0, new KeyValuePair<Variable<IAPLValue>, IExpression<IAPLValue>>(variable, expression));
                    semanticStack.Push(varExprList);
                    break;

                case "#emptyVarExprList":
                    semanticStack.Push(new List<KeyValuePair<Variable<IAPLValue>, IExpression<IAPLValue>>>());
                    break;

                default:
                    base.ExecuteSemanticAction(semanticStack, action);
                    break;
            }
        }

        public override Symbol TokenToSymbol(Token token)
        {
            string tokenValueAsString = token.TokenValue.ToString();

            switch (token.TokenType)
            {
                case TokenType.T_Ident:

                    switch (tokenValueAsString)
                    {
                        case "max": return Symbol.T_Max;
                        case "or": return Symbol.T_Or;
                        case "and": return Symbol.T_And;
                        case "+/": return Symbol.T_PlusSlash;
                        case "-/": return Symbol.T_MinusSlash;
                        case "*/": return Symbol.T_MultiplySlash;
                        case "//": return Symbol.T_DivideSlash;
                        case "max/": return Symbol.T_MaxSlash;
                        case "or/": return Symbol.T_OrSlash;
                        case "and/": return Symbol.T_AndSlash;
                        case "compress": return Symbol.T_Compress;
                        case "shape": return Symbol.T_Shape;
                        case "ravel": return Symbol.T_Ravel;
                        case "restruct": return Symbol.T_Restruct;
                        case "cat": return Symbol.T_Cat;
                        case "indx": return Symbol.T_Indx;
                        case "trans": return Symbol.T_Trans;
                        case "[]": return Symbol.T_SquareBrackets;      // For (single) subscripting.
                        case ":=": return Symbol.T_Assign;
                        case "[;]": return Symbol.T_DoubleSubscripting; // See page 86.
                        case "random": return Symbol.T_Random;
                        case "pow": return Symbol.T_Pow;
                        case "exp": return Symbol.T_Exp;
                        case "ln": return Symbol.T_Ln;
                        case "sin": return Symbol.T_Sin;
                        case "cos": return Symbol.T_Cos;
                        case "tan": return Symbol.T_Tan;
                        default: break;
                    }

                    break;

                case TokenType.T_Apostrophe: return Symbol.T_Apostrophe;

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
                case Symbol.T_Max:
                case Symbol.T_Or:
                case Symbol.T_And:
                case Symbol.T_PlusSlash:
                case Symbol.T_MinusSlash:
                case Symbol.T_MultiplySlash:
                case Symbol.T_DivideSlash:
                case Symbol.T_MaxSlash:
                case Symbol.T_OrSlash:
                case Symbol.T_AndSlash:
                case Symbol.T_Compress:
                case Symbol.T_Shape:
                case Symbol.T_Ravel:
                case Symbol.T_Restruct:
                case Symbol.T_Cat:
                case Symbol.T_Indx:
                case Symbol.T_Trans:
                case Symbol.T_SquareBrackets:
                case Symbol.T_DoubleSubscripting:
                case Symbol.T_Random:
                case Symbol.T_Pow:
                case Symbol.T_Exp:
                case Symbol.T_Ln:
                case Symbol.T_Sin:
                case Symbol.T_Cos:
                case Symbol.T_Tan:
                    semanticStack.Push(new Name((string)value, token.Line, token.Column));
                    break;

                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(APLValue<int>.CreateScalar((int)value));
                    break;

                case Symbol.T_FloatLiteral:
                    semanticStack.Push(APLValue<double>.CreateScalar((double)value));
                    break;

                default:
                    base.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
                    break;
            }
        }
    }
}
