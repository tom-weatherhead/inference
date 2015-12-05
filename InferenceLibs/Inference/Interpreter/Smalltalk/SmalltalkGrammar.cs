using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Parser;

namespace Inference.Interpreter.Smalltalk
{
    public class SmalltalkGrammar : InterpreterGrammarBase
    {
        public SmalltalkGrammar()
            : base()
        {
            Terminals.Remove(Symbol.T_If);
            RemoveProductionsContainingSymbol(Symbol.T_If);

            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_Class, Symbol.T_Octothorpe, Symbol.T_FloatLiteral, Symbol.T_StringLiteral,
                Symbol.T_NumberPred, Symbol.T_SymbolPred, Symbol.T_StringPred, Symbol.T_ObjectPred,
                Symbol.T_Random, Symbol.T_ToString, Symbol.T_StringToSymbol, Symbol.T_Pow,
                Symbol.T_Exp, Symbol.T_Ln, Symbol.T_Sin, Symbol.T_Cos,
                Symbol.T_Tan, Symbol.T_Floor, Symbol.T_Atan2, Symbol.T_Throw,
                Symbol.T_StringLessThan, Symbol.T_Strlen, Symbol.T_Substr, Symbol.T_Typename,
                Symbol.T_Hash, Symbol.T_ReferenceEquals, Symbol.T_Strcat, Symbol.T_NewArray,
                Symbol.T_ArrayLength, Symbol.T_ArrayGet, Symbol.T_ArraySet, Symbol.T_Dollar,
                Symbol.T_CharPred, Symbol.T_StringIndex, Symbol.T_ArrayPred });

            NonTerminals.UnionWith(new HashSet<Symbol>() {
                Symbol.N_ClassDef, Symbol.N_Class, Symbol.N_InstVars,
                Symbol.N_MethodDef, Symbol.N_MethodDefList, Symbol.N_Symbol, Symbol.N_LiteralList });

            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_ClassDef }, 39));
            Productions.Add(new Production(Symbol.N_ClassDef, new List<object>() {
                Symbol.T_LeftBracket,
                Symbol.T_Class,
                Symbol.N_Class,
                Symbol.N_Class,
                Symbol.N_InstVars,  // Actually the class variables; see Exercise 10 on page 347.
                Symbol.N_InstVars,
                Symbol.N_MethodDef,
                Symbol.N_MethodDefList,
                Symbol.T_RightBracket, "#classDefinition" }, 40));
            Productions.Add(new Production(Symbol.N_MethodDefList, new List<object>() { Symbol.N_MethodDef, Symbol.N_MethodDefList, "#methodDefList" }, 41));
            Productions.Add(new Production(Symbol.N_MethodDefList, new List<object>() { Symbol.Lambda, "#emptyMethodDefList" }, 42));
            Productions.Add(new Production(Symbol.N_InstVars, new List<object>() { Symbol.T_LeftBracket, Symbol.N_VariableList, Symbol.T_RightBracket }, 43));
            Productions.Add(new Production(Symbol.N_MethodDef, new List<object>() { Symbol.N_FunDef }, 44));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_Symbol }, 45));
            Productions.Add(new Production(Symbol.N_Symbol, new List<object>() { Symbol.T_Octothorpe, Symbol.T_ID, "#symbol" }, 46));
            Productions.Add(new Production(Symbol.N_Class, new List<object>() { Symbol.T_ID }, 47));

            // This next production allows us to redefine built-in value ops (such as +) in classes.
            Productions.Add(new Production(Symbol.N_FunDef, new List<object>() {
                Symbol.T_LeftBracket,
                Symbol.T_Define,
                Symbol.N_ValueOp,
                Symbol.N_ArgList,
                Symbol.N_Expression,
                Symbol.T_RightBracket, "#functionDefinition" }, 48));

            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 49));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 50));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Random }, 51));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ToString }, 52));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringToSymbol }, 53));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Pow }, 54));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Exp }, 55));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Ln }, 56));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Sin }, 57));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cos }, 58));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Tan }, 59));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Floor }, 60));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Atan2 }, 61));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Throw }, 62));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringLessThan }, 63));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NumberPred }, 64));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_SymbolPred }, 65));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringPred }, 66));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ObjectPred }, 67));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Strlen }, 68));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Substr }, 69));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Typename }, 70));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Hash }, 71));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ReferenceEquals }, 72));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Strcat }, 73));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NewArray }, 74));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ArrayLength }, 75));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ArrayGet }, 76));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ArraySet }, 77));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() {
                Symbol.T_Octothorpe, Symbol.T_LeftBracket, Symbol.N_LiteralList, Symbol.T_RightBracket, "#arrayLiteral" }, 78));
            Productions.Add(new Production(Symbol.N_LiteralList, new List<object>() { Symbol.Lambda, "#emptyLiteralList" }, 79));
            Productions.Add(new Production(Symbol.N_LiteralList, new List<object>() { Symbol.N_Value, Symbol.N_LiteralList, "#literalList" }, 80));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_Dollar, Symbol.T_ID, "#characterLiteral" }, 81));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_CharPred }, 82));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringIndex }, 83));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ArrayPred }, 84));
        }

        private ISmalltalkExpression CreateLetUsage(string letKeyword,
            List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> varExprList, ISmalltalkExpression expression)
        {

            switch (letKeyword)
            {
                case "let":
                    return new SmalltalkLetUsage(varExprList, expression);

                case "let*":
                    return new SmalltalkLetStarUsage(varExprList, expression);

                default:
                    throw new ArgumentException(string.Format("SmalltalkGrammar.CreateLetUsage() : Unknown 'let' keyword '{0}'.", letKeyword));
            }
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            Name name;
            Name functionName;
            Name superClassName;
            SmalltalkVariable variable;
            List<SmalltalkVariable> variableList;
            List<SmalltalkVariable> classVariableList;
            ISmalltalkExpression expression;
            ISmalltalkExpression expression2;
            List<ISmalltalkExpression> expressionList;
            SmalltalkFunctionDefinition funDef;
            List<SmalltalkFunctionDefinition> funDefList;
            List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>> exprPairList;
            List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> varExprList;
            List<ISmalltalkValue> literalList;

            switch (action)
            {
                case "#functionDefinition":
                    var body = (ISmalltalkExpression)semanticStack.Pop();
                    var argList = (List<SmalltalkVariable>)semanticStack.Pop();

                    functionName = (Name)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkFunctionDefinition(functionName.Value, argList, body));
                    break;

                case "#variableList":
                    variableList = (List<SmalltalkVariable>)semanticStack.Pop();
                    variable = (SmalltalkVariable)semanticStack.Pop();
                    variableList.Insert(0, variable);
                    semanticStack.Push(variableList);
                    break;

                case "#emptyVariableList":
                    semanticStack.Push(new List<SmalltalkVariable>());
                    break;

#if DEAD_CODE
                case "#if":
                    var expression3 = (ISmalltalkExpression)semanticStack.Pop();

                    expression2 = (ISmalltalkExpression)semanticStack.Pop();
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkIfUsage(expression, expression2, expression3));
                    break;
#endif

                case "#while":
                    expression2 = (ISmalltalkExpression)semanticStack.Pop();
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkWhileUsage(expression, expression2));
                    break;

                case "#set":
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    variable = (SmalltalkVariable)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkSetUsage(variable, expression));
                    break;

                case "#begin":
                    expressionList = (List<ISmalltalkExpression>)semanticStack.Pop();
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkBeginUsage(expression, expressionList));
                    break;

                case "#operatorUsage":
                    expressionList = (List<ISmalltalkExpression>)semanticStack.Pop();
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkOperatorUsage(name /*.Value */, expressionList));
                    break;

                case "#expressionList":
                    expressionList = (List<ISmalltalkExpression>)semanticStack.Pop();
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    expressionList.Insert(0, expression);
                    semanticStack.Push(expressionList);
                    break;

                case "#emptyExpressionList":
                    semanticStack.Push(new List<ISmalltalkExpression>());
                    break;

                case "#variable":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkVariable(name.Value));
                    break;

                case "#methodDefList":
                    funDefList = (List<SmalltalkFunctionDefinition>)semanticStack.Pop();
                    funDef = (SmalltalkFunctionDefinition)semanticStack.Pop();
                    funDefList.Insert(0, funDef);
                    semanticStack.Push(funDefList);
                    break;

                case "#emptyMethodDefList":
                    semanticStack.Push(new List<SmalltalkFunctionDefinition>());
                    break;

                case "#classDefinition":
                    funDefList = (List<SmalltalkFunctionDefinition>)semanticStack.Pop();
                    funDef = (SmalltalkFunctionDefinition)semanticStack.Pop();
                    funDefList.Insert(0, funDef);
                    variableList = (List<SmalltalkVariable>)semanticStack.Pop();
                    classVariableList = (List<SmalltalkVariable>)semanticStack.Pop();
                    superClassName = (Name)semanticStack.Pop();
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkClass(name, superClassName.Value, classVariableList, variableList, funDefList));
                    break;

                case "#symbol":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkSymbolValue(name.Value));
                    break;

                case "#condUsage":
                    exprPairList = (List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>>)semanticStack.Pop();
                    expression2 = (ISmalltalkExpression)semanticStack.Pop();
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>(expression, expression2));
                    semanticStack.Push(new SmalltalkCondUsage(exprPairList));
                    break;

                case "#exprPairList":
                    exprPairList = (List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>>)semanticStack.Pop();
                    expression2 = (ISmalltalkExpression)semanticStack.Pop();
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>(expression, expression2));
                    semanticStack.Push(exprPairList);
                    break;

                case "#emptyExprPairList":
                    semanticStack.Push(new List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>>());
                    break;

                case "#letUsage":
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    varExprList = (List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>>)semanticStack.Pop();

                    var letName = (Name)semanticStack.Pop();

                    semanticStack.Push(CreateLetUsage(letName.Value, varExprList, expression));
                    break;

                case "#varExprList":
                    varExprList = (List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>>)semanticStack.Pop();
                    expression = (ISmalltalkExpression)semanticStack.Pop();
                    variable = (SmalltalkVariable)semanticStack.Pop();
                    varExprList.Insert(0, new KeyValuePair<SmalltalkVariable, ISmalltalkExpression>(variable, expression));
                    semanticStack.Push(varExprList);
                    break;

                case "#emptyVarExprList":
                    semanticStack.Push(new List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>>());
                    break;

                case "#emptyLiteralList":
                    semanticStack.Push(new List<ISmalltalkValue>());
                    break;

                case "#literalList":
                    literalList = (List<ISmalltalkValue>)semanticStack.Pop();

                    var literal = (ISmalltalkValue)semanticStack.Pop();

                    literalList.Insert(0, literal);
                    semanticStack.Push(literalList);
                    break;

                case "#arrayLiteral":
                    literalList = (List<ISmalltalkValue>)semanticStack.Pop();

                    var array = new SmalltalkArrayValue(literalList.Count);

                    for (var i = 0; i < literalList.Count; ++i)
                    {
                        array.Value[i] = literalList[i];
                    }

                    semanticStack.Push(array);
                    break;

                case "#characterLiteral":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new SmalltalkCharacterValue(name.Value[0]));
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
                        case "class": return Symbol.T_Class;
                        case "number?": return Symbol.T_NumberPred;
                        case "symbol?": return Symbol.T_SymbolPred;
                        case "string?": return Symbol.T_StringPred;
                        case "object?": return Symbol.T_ObjectPred;
                        case "random": return Symbol.T_Random;
                        case "tostring": return Symbol.T_ToString;
                        case "stringtosymbol": return Symbol.T_StringToSymbol;
                        case "pow": return Symbol.T_Pow;
                        case "exp": return Symbol.T_Exp;
                        case "ln": return Symbol.T_Ln;
                        case "sin": return Symbol.T_Sin;
                        case "cos": return Symbol.T_Cos;
                        case "tan": return Symbol.T_Tan;
                        case "atan2": return Symbol.T_Atan2;
                        case "floor": return Symbol.T_Floor;
                        case "throw": return Symbol.T_Throw;
                        case "string<": return Symbol.T_StringLessThan;
                        case "strlen": return Symbol.T_Strlen;
                        case "substr": return Symbol.T_Substr;
                        case "typename": return Symbol.T_Typename;
                        case "hash": return Symbol.T_Hash;
                        case "ref=": return Symbol.T_ReferenceEquals;
                        case "strcat": return Symbol.T_Strcat;
                        case "newarray": return Symbol.T_NewArray;
                        case "arraylength": return Symbol.T_ArrayLength;
                        case "arrayget": return Symbol.T_ArrayGet;
                        case "arrayset": return Symbol.T_ArraySet;
                        case "array?": return Symbol.T_ArrayPred;
                        case "char?": return Symbol.T_CharPred;
                        case "stridx": return Symbol.T_StringIndex;
                        case "if": return Symbol.T_ID;
                        default: break;
                    }

                    break;

                case TokenType.T_Octothorpe: return Symbol.T_Octothorpe;
                case TokenType.T_Dollar: return Symbol.T_Dollar;

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
                case Symbol.T_NumberPred:
                case Symbol.T_SymbolPred:
                case Symbol.T_StringPred:
                case Symbol.T_ObjectPred:
                case Symbol.T_Random:
                case Symbol.T_ToString:
                case Symbol.T_StringToSymbol:
                case Symbol.T_Pow:
                case Symbol.T_Exp:
                case Symbol.T_Ln:
                case Symbol.T_Sin:
                case Symbol.T_Cos:
                case Symbol.T_Tan:
                case Symbol.T_Atan2:
                case Symbol.T_Floor:
                case Symbol.T_Throw:
                case Symbol.T_StringLessThan:
                case Symbol.T_Strlen:
                case Symbol.T_Substr:
                case Symbol.T_Typename:
                case Symbol.T_Hash:
                case Symbol.T_ReferenceEquals:
                case Symbol.T_Strcat:
                case Symbol.T_NewArray:
                case Symbol.T_ArrayLength:
                case Symbol.T_ArrayGet:
                case Symbol.T_ArraySet:
                case Symbol.T_ArrayPred:
                case Symbol.T_CharPred:
                case Symbol.T_StringIndex:
                    semanticStack.Push(new Name(value as string, token.Line, token.Column));
                    break;

                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(new SmalltalkIntegerValue((int)value));
                    break;

                case Symbol.T_FloatLiteral:
                    semanticStack.Push(new SmalltalkFloatValue((double)value));
                    break;

                case Symbol.T_StringLiteral:
                    semanticStack.Push(new SmalltalkStringValue((string)value));
                    break;

                default:
                    base.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
                    break;
            }
        }
    }
}
