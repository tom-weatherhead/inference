using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Interpreter.LISP;
using Inference.Interpreter.Scheme;
using Inference.Parser;

namespace Inference.Interpreter.SASL
{
    public class SASLGrammar : SchemeGrammar
    {
        public SASLGrammar()
            : base()
        {
            Terminals.Remove(Symbol.T_While);
            Terminals.Remove(Symbol.T_Begin);
            Terminals.Remove(Symbol.T_Print);
            Terminals.Remove(Symbol.T_Rplaca);
            Terminals.Remove(Symbol.T_Rplacd);
            Terminals.Remove(Symbol.T_DefineMacro);
            NonTerminals.Remove(Symbol.N_MacroDef);
            RemoveProductionsContainingSymbol(Symbol.T_While);
            RemoveProductionsContainingSymbol(Symbol.T_Begin);
            RemoveProductionsContainingSymbol(Symbol.T_Print);
            RemoveProductionsContainingSymbol(Symbol.T_Set);
            RemoveProductionsContainingSymbol(Symbol.T_Rplaca);
            RemoveProductionsContainingSymbol(Symbol.T_Rplacd);
            RemoveProductionsContainingSymbol(Symbol.T_If);
            RemoveProductionsContainingSymbol(Symbol.N_MacroDef);
            RemoveProductionsContainingSymbol(Symbol.N_ExprPairList);   // This removes the three productions related to cond.

            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Set, Symbol.N_Variable, Symbol.N_Expression, Symbol.T_RightBracket, "#set" }, 70));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_If }, 71));
            Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cond }, 72));
        }

#if DEAD_CODE
        protected override IExpression<ISExpression> CreateLetUsage(string letKeyword,
            List<KeyValuePair<Variable<ISExpression>, IExpression<ISExpression>>> varExprList, IExpression<ISExpression> expression)
        {

            switch (letKeyword)
            {
                case "letrec":
                    return new SASLLetRecUsage(varExprList, expression);

                default:
                    return base.CreateLetUsage(letKeyword, varExprList, expression);
            }
        }
#endif

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            Name name;
            IExpression<ISExpression> expression;

            switch (action)
            {
                case "#evaluableExpression":
                    var expressionList = (ExpressionList<ISExpression>)semanticStack.Pop();

                    expression = (IExpression<ISExpression>)semanticStack.Pop();
                    semanticStack.Push(new SASLEvaluableExpression(expression, expressionList));
                    break;

                case "#lambdaExpression":
                    var body = (IExpression<ISExpression>)semanticStack.Pop();
                    var argList = (VariableList<ISExpression>)semanticStack.Pop();

                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new SASLLambdaExpression(argList, body, name.Line, name.Column));
                    break;

                case "#valueOp":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new SASLPrimOp(name));
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
                        case "while":
                        case "begin":
                        case "print":
                        case "rplaca":
                        case "rplacd":
                        case "define-macro":
                            return Symbol.T_ID;

                        default:
                            break;
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
                case Symbol.T_If:
                case Symbol.T_Cond:
                    semanticStack.Push(new Name(value as string, token.Line, token.Column));
                    break;

                default:
                    base.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
                    break;
            }
        }
    }
}
