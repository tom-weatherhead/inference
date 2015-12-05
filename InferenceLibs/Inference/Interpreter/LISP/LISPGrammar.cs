using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.LISP
{
    public class LISPGrammar : InterpreterGrammarBase
    {
        // The LISP grammar from Kamin

        public LISPGrammar()
            : base()
        {
            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_Cons, Symbol.T_Car, Symbol.T_Cdr,
                Symbol.T_NumberPred, Symbol.T_SymbolPred, Symbol.T_ListPred, Symbol.T_NullPred,
                Symbol.T_Apostrophe, Symbol.T_Dot, Symbol.T_List,
                Symbol.T_Rplaca, Symbol.T_Rplacd, Symbol.T_DefineMacro, Symbol.T_QuoteKeyword,
                Symbol.T_Random, Symbol.T_StringLiteral, Symbol.T_StringPred,
                Symbol.T_ToString, Symbol.T_ListToString, Symbol.T_StringToList, Symbol.T_StringToSymbol,
                Symbol.T_FloatLiteral, Symbol.T_Pow, Symbol.T_Exp, Symbol.T_Ln, Symbol.T_Sin, Symbol.T_Cos, Symbol.T_Tan, Symbol.T_Atan2, Symbol.T_Floor,
                Symbol.T_Throw, Symbol.T_StringLessThan });

            NonTerminals.UnionWith(new HashSet<Symbol>() {
                Symbol.N_QuotedConst, Symbol.N_SExpression, Symbol.N_SExpressionList, Symbol.N_Symbol,
                Symbol.N_MacroDef });

            // From Chapter 2 of Kamin:
            // Value -> Quoted-Const
            // Value-Op -> cons
            // Value-Op -> car
            // Value-Op -> cdr
            // Value-Op -> number?
            // Value-Op -> symbol?
            // Value-Op -> list?
            // Value-Op -> null?
            // Quoted-Const -> ' S-Expression
            // S-Expression -> Integer
            // S-Expression -> Symbol
            // S-Expression -> ( S-Expression-List )
            // S-Expression-List -> S-Expression S-Expression-List
            // S-Expression-List -> S-Expression . S-Expression
            // S-Expression-List -> Lambda
            // Symbol -> Name
            // BracketedExpression -> cond ( Expression Expression ) ExprPairList
            // ExprPairList -> ( Expression Expression ) ExprPairList
            // ExprPairList -> Lambda
            // Value-Op -> list

            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_QuotedConst }, 39));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cons }, 40));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Car }, 41));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cdr }, 42));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NumberPred }, 43));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_SymbolPred }, 44));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ListPred }, 45));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NullPred }, 46));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringPred }, 73)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_QuotedConst, new List<object>() { Symbol.T_Apostrophe, Symbol.N_SExpression, "#quotedConstantWithApostrophe" }, 47));
            Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_IntegerLiteral }, 48));
            Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.N_Symbol }, 49));
            Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_StringLiteral }, 74)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_FloatLiteral }, 80)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_SExpressionList, Symbol.T_RightBracket }, 50));
            Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.N_SExpression, Symbol.N_SExpressionList, "#sExpressionList" }, 51));
            Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.N_SExpression, Symbol.T_Dot, Symbol.N_SExpression, "#sExpressionList" }, 52));
            Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.Lambda, "#emptySExpressionList" }, 53));
            Productions.Add(new Production(Symbol.N_Symbol, new List<object>() { Symbol.T_ID, "#symbol" }, 54));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_List }, 55));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Rplaca }, 56));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Rplacd }, 57));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_MacroDef }, 58));
            Productions.Add(new Production(Symbol.N_MacroDef, new List<object>() { Symbol.T_LeftBracket, Symbol.T_DefineMacro, Symbol.N_Function, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#macroDefinition" }, 59));
            Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_LeftBracket, Symbol.T_QuoteKeyword, Symbol.N_SExpression, Symbol.T_RightBracket, "#quotedConstantWithQuoteKeyword" }, 60));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Random }, 61));
            //Productions.Add(new Production(Symbol.N_QuotedConst, new List<object>() { Symbol.T_LeftBracket, Symbol.T_QuoteKeyword, Symbol.N_SExpression, Symbol.T_RightBracket, "#quotedConstantWithQuoteKeyword" }, 62)); // Needed for macros.
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ToString }, 76)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ListToString }, 77)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringToList }, 78)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringToSymbol }, 81)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Pow }, 88)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Exp }, 82)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Ln }, 83)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Sin }, 84)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cos }, 85)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Tan }, 86)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Floor }, 87)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Atan2 }, 89)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Throw }, 90)); // Note: Number is out of order
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringLessThan }, 91)); // Note: Number is out of order
            // Next number is 92.
        }

        protected virtual IExpression<ISExpression> CreateLetUsage(string letKeyword,
            List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>> varExprList, IExpression<ISExpression> expression)
        {

            switch (letKeyword)
            {
                case "let":
                    return new LetUsage<ISExpression>(varExprList, expression);

                case "let*":
                    return new LetStarUsage<ISExpression>(varExprList, expression);

                default:
                    throw new ArgumentException(string.Format("LISPGrammar.CreateLetUsage() : Unknown 'let' keyword '{0}'.", letKeyword));
            }
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            Name name;
            Variable<ISExpression> variable;
            IExpression<ISExpression> expression;
            IExpression<ISExpression> expression2;
            ExpressionList<ISExpression> expressionList;
            ISExpression sexpression;
            List<KeyValuePair<IExpression<ISExpression>, IExpression<ISExpression>>> exprPairList;
            List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>> varExprList;

            switch (action)
            {
                case "#functionDefinition":
                    var body = (IExpression<ISExpression>)semanticStack.Pop();
                    var argList = (VariableList<ISExpression>)semanticStack.Pop();
                    var functionName = (Name)semanticStack.Pop();

                    semanticStack.Push(new FunctionDefinition<ISExpression>(functionName, argList, body));
                    break;

                case "#variableList":
                    var variableList = (VariableList<ISExpression>)semanticStack.Pop();

                    variable = (Variable<ISExpression>)semanticStack.Pop();
                    variableList.Value.Insert(0, variable);
                    semanticStack.Push(variableList);
                    break;

                case "#emptyVariableList":
                    semanticStack.Push(new VariableList<ISExpression>());
                    break;

                case "#if":
                    var expression3 = (IExpression<ISExpression>)semanticStack.Pop();

                    expression2 = (IExpression<ISExpression>)semanticStack.Pop();
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    semanticStack.Push(new IfUsage<ISExpression>(expression, expression2, expression3));
                    break;

                case "#while":
                    expression2 = (IExpression<ISExpression>)semanticStack.Pop();
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    semanticStack.Push(new WhileUsage<ISExpression>(expression, expression2));
                    break;

                case "#set":
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    variable = (Variable<ISExpression>)semanticStack.Pop();
                    semanticStack.Push(new SetUsage<ISExpression>(variable, expression));
                    break;

                case "#begin":
                    expressionList = (ExpressionList<ISExpression>)semanticStack.Pop();
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    semanticStack.Push(new BeginUsage<ISExpression>(expression, expressionList));
                    break;

                case "#operatorUsage":
                    expressionList = (ExpressionList<ISExpression>)semanticStack.Pop();

                    var operatorName = (Name)semanticStack.Pop();

                    semanticStack.Push(new LISPOperatorUsage(operatorName, expressionList));
                    break;

                case "#expressionList":
                    expressionList = (ExpressionList<ISExpression>)semanticStack.Pop();
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    expressionList.Value.Insert(0, expression);
                    semanticStack.Push(expressionList);
                    break;

                case "#emptyExpressionList":
                    semanticStack.Push(new ExpressionList<ISExpression>());
                    break;

                case "#variable":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new Variable<ISExpression>(name.Value, name.Line, name.Column));
                    break;

                case "#quotedConstantWithApostrophe":
                    sexpression = (ISExpression)semanticStack.Pop();
                    semanticStack.Push(new QuotedConstantWithApostrophe(sexpression));
                    break;

                case "#quotedConstantWithQuoteKeyword":
                    sexpression = (ISExpression)semanticStack.Pop();
                    semanticStack.Push(new QuotedConstantWithQuoteKeyword(sexpression));
                    break;

                case "#sExpressionList":
                    var tail = (ISExpression)semanticStack.Pop();
                    var head = (ISExpression)semanticStack.Pop();

                    semanticStack.Push(new SExpressionList(head, tail));
                    break;

                case "#emptySExpressionList":
                    semanticStack.Push(new NullSExpression());
                    break;

                case "#symbol":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new LISPSymbol(name.Value));
                    break;

                case "#condUsage":
                    exprPairList = (List<KeyValuePair<IExpression<ISExpression>, IExpression<ISExpression>>>)semanticStack.Pop();
                    expression2 = (IExpression<ISExpression>)semanticStack.Pop();
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<IExpression<ISExpression>, IExpression<ISExpression>>(expression, expression2));
                    semanticStack.Push(new CondUsage<ISExpression>(exprPairList));
                    break;

                case "#exprPairList":
                    exprPairList = (List<KeyValuePair<IExpression<ISExpression>, IExpression<ISExpression>>>)semanticStack.Pop();
                    expression2 = (IExpression<ISExpression>)semanticStack.Pop();
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<IExpression<ISExpression>, IExpression<ISExpression>>(expression, expression2));
                    semanticStack.Push(exprPairList);
                    break;

                case "#emptyExprPairList":
                    semanticStack.Push(new List<KeyValuePair<IExpression<ISExpression>, IExpression<ISExpression>>>());
                    break;

                case "#letUsage":
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    varExprList = (List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>>)semanticStack.Pop();

                    var letName = (Name)semanticStack.Pop();

                    semanticStack.Push(CreateLetUsage(letName.Value, varExprList, expression));
                    break;

                case "#varExprList":
                    varExprList = (List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>>)semanticStack.Pop();
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    variable = (Variable<ISExpression>)semanticStack.Pop();
                    varExprList.Insert(0, new KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>(variable, expression));
                    semanticStack.Push(varExprList);
                    break;

                case "#emptyVarExprList":
                    semanticStack.Push(new List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>>());
                    break;

                case "#macroDefinition":
                    var macroBody = (IExpression<ISExpression>)semanticStack.Pop();
                    var macroArgList = (VariableList<ISExpression>)semanticStack.Pop();
                    var macroName = (Name)semanticStack.Pop();

                    semanticStack.Push(new MacroDefinition(macroName, macroArgList, macroBody));
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
                        // Quoting never changes tokens with these values into IDs.
                        case ".": return Symbol.T_Dot;  // We could modify the tokenizer to generate TokenType.T_Dot in this case, to obviate this line.
                        //case "quote": return Symbol.T_QuoteKeyword;
                        default: break;
                    }

                    if (token.IsQuoted)
                    {
                        return Symbol.T_ID;
                    }

                    switch (tokenValueAsString)
                    {
                        case "cons": return Symbol.T_Cons;
                        case "car": return Symbol.T_Car;
                        case "cdr": return Symbol.T_Cdr;
                        case "number?": return Symbol.T_NumberPred;
                        case "symbol?": return Symbol.T_SymbolPred;
                        case "list?": return Symbol.T_ListPred;
                        case "null?": return Symbol.T_NullPred;
                        case "string?": return Symbol.T_StringPred;
                        case "list": return Symbol.T_List;
                        case "rplaca": return Symbol.T_Rplaca;
                        case "rplacd": return Symbol.T_Rplacd;
                        case "define-macro": return Symbol.T_DefineMacro;
                        case "random": return Symbol.T_Random;
                        case "tostring": return Symbol.T_ToString;
                        case "listtostring": return Symbol.T_ListToString;
                        case "stringtolist": return Symbol.T_StringToList;
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
                        default: break;
                    }

                    break;

                case TokenType.T_Apostrophe: return Symbol.T_Apostrophe;
                case TokenType.T_QuoteKeyword: return Symbol.T_QuoteKeyword;

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
                case Symbol.T_Cons:
                case Symbol.T_Car:
                case Symbol.T_Cdr:
                case Symbol.T_NumberPred:
                case Symbol.T_SymbolPred:
                case Symbol.T_ListPred:
                case Symbol.T_NullPred:
                case Symbol.T_StringPred:
                case Symbol.T_List:
                case Symbol.T_Rplaca:
                case Symbol.T_Rplacd:
                case Symbol.T_Random:
                case Symbol.T_ToString:
                case Symbol.T_ListToString:
                case Symbol.T_StringToList:
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
                    semanticStack.Push(new Name(value as string, token.Line, token.Column));
                    break;

                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(new IntegerLiteral(value));
                    break;

                case Symbol.T_FloatLiteral:
                    semanticStack.Push(new FloatLiteral(value));
                    break;

                case Symbol.T_StringLiteral:
                    semanticStack.Push(new LISPString(value as string));
                    break;

                default:
                    base.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
                    break;
            }
        }
    }
}
