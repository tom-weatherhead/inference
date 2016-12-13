using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.LISP;
using Inference.Parser;

namespace Inference.Interpreter.Scheme
{
    public class SchemeGrammar : LISPGrammar
    {
        public SchemeGrammar()
             : base()
        {
            Terminals.Remove(Symbol.T_Define);
            NonTerminals.Remove(Symbol.N_FunDef);
            NonTerminals.Remove(Symbol.N_Optr);
            RemoveProductionsContainingSymbol(Symbol.N_FunDef);
            RemoveProductionsContainingSymbol(Symbol.N_Optr);
            //RemoveProductionsContainingSymbol(Symbol.N_Function); // The LISP macro production uses N_Function

            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_PrimOpPred, Symbol.T_ClosurePred, Symbol.T_LambdaKeyword, Symbol.T_LetRec,
                Symbol.T_CallCC });

            /*
            NonTerminals.UnionWith(new HashSet<Symbol>() {
                Symbol. });
             */

            // BracketedExpression -> Expression ExpressionList
            // Value-Op -> primop?
            // Value-Op -> closure?
            // Value -> ( lambda ArgList Expression )
            // Value -> Value-Op
            // BracketedExpression -> LetKeyword ( VarExprList ) Expression
            // LetKeyword -> let
            // LetKeyword -> let*
            // LetKeyword -> letrec
            // VarExprList -> ( Variable Expression ) VarExprList
            // VarExprList -> Lambda
            // BracketedExpression -> call/cc Expression

            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#evaluableExpression" }, 63));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_PrimOpPred }, 64));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ClosurePred }, 65));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftBracket, Symbol.T_LambdaKeyword, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#lambdaExpression" }, 66));
            Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_ValueOp, "#valueOp" }, 67));
            Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetRec }, 68));
            Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_CallCC, Symbol.N_Expression, "#call/cc" }, 69));
        }

        protected override IExpression<ISExpression> CreateLetUsage(string letKeyword,
            List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>> varExprList, IExpression<ISExpression> expression)
        {

            switch (letKeyword)
            {
                case "letrec":
                    return new LetRecUsage(varExprList, expression);

                default:
                    return base.CreateLetUsage(letKeyword, varExprList, expression);
            }
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            Name name;
            IExpression<ISExpression> expression;
            ExpressionList<ISExpression> expressionList;

            switch (action)
            {
                case "#evaluableExpression":
                    expressionList = (ExpressionList<ISExpression>)semanticStack.Pop();
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    semanticStack.Push(new EvaluableExpression(expression, expressionList));
                    break;

                case "#lambdaExpression":
                    var body = (IExpression<ISExpression>)semanticStack.Pop();
                    var argList = (VariableList<ISExpression>)semanticStack.Pop();

                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new LambdaExpression(argList, body, name.Line, name.Column));
                    break;

                case "#valueOp":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new PrimOp(name));
                    break;

                case "#call/cc":
                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    semanticStack.Push(new CallCCUsage(expression));
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
                        case "define": return Symbol.T_ID;  // This undoes the LISP grammar's definition of "define".
                        case "primop?": return Symbol.T_PrimOpPred;
                        case "closure?": return Symbol.T_ClosurePred;
                        case "lambda": return Symbol.T_LambdaKeyword;
                        case "letrec": return Symbol.T_LetRec;
                        case "call/cc": return Symbol.T_CallCC;
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
                case Symbol.T_PrimOpPred:
                case Symbol.T_ClosurePred:
                case Symbol.T_LetRec:
                case Symbol.T_LambdaKeyword: // Push a Name for lambda so that we can find the line and column numbers where the lambda begins.
                    semanticStack.Push(new Name(value as string, token.Line, token.Column));
                    break;

                default:
                    base.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
                    break;
            }
        }
    }
}
