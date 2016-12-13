using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Parser;

namespace Inference.Interpreter.Chapter1
{
    public class Grammar : InterpreterGrammarBase
    {
        public Grammar()
            : base()
        {
        }

        private IExpression<int> CreateLetUsage(string letKeyword,
            List<KeyValuePair<Variable<int>, IExpression<int>>> varExprList, IExpression<int> expression)
        {

            switch (letKeyword)
            {
                case "let":
                    return new LetUsage<int>(varExprList, expression);

                case "let*":
                    return new LetStarUsage<int>(varExprList, expression);

                default:
                    throw new ArgumentException(string.Format("Grammar.CreateLetUsage() : Unknown 'let' keyword '{0}'.", letKeyword));
            }
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            Name name;
            Variable<int> variable;
            IExpression<int> expression;
            IExpression<int> expression2;
            ExpressionList<int> expressionList;
            List<KeyValuePair<IExpression<int>, IExpression<int>>> exprPairList;
            List<KeyValuePair<Variable<int>, IExpression<int>>> varExprList;

            switch (action)
            {
                case "#functionDefinition":
                    var body = (IExpression<int>)semanticStack.Pop();
                    var argList = (VariableList<int>)semanticStack.Pop();
                    var functionName = (Name)semanticStack.Pop();

                    semanticStack.Push(new FunctionDefinition<int>(functionName, argList, body));
                    break;

                case "#variableList":
                    var variableList = (VariableList<int>)semanticStack.Pop();

                    variable = (Variable<int>)semanticStack.Pop();
                    variableList.Value.Insert(0, variable);
                    semanticStack.Push(variableList);
                    break;

                case "#emptyVariableList":
                    semanticStack.Push(new VariableList<int>());
                    break;

                case "#if":
                    var expression3 = (IExpression<int>)semanticStack.Pop();

                    expression2 = (IExpression<int>)semanticStack.Pop();
                    expression = (IExpression<int>)semanticStack.Pop();
                    semanticStack.Push(new IfUsage<int>(expression, expression2, expression3));
                    break;

                case "#while":
                    expression2 = (IExpression<int>)semanticStack.Pop();
                    expression = (IExpression<int>)semanticStack.Pop();
                    semanticStack.Push(new WhileUsage<int>(expression, expression2));
                    break;

                case "#set":
                    expression = (IExpression<int>)semanticStack.Pop();
                    variable = (Variable<int>)semanticStack.Pop();
                    semanticStack.Push(new SetUsage<int>(variable, expression));
                    break;

                case "#begin":
                    expressionList = (ExpressionList<int>)semanticStack.Pop();
                    expression = (IExpression<int>)semanticStack.Pop();
                    semanticStack.Push(new BeginUsage<int>(expression, expressionList));
                    break;

                case "#operatorUsage":
                    expressionList = (ExpressionList<int>)semanticStack.Pop();

                    var operatorName = (Name)semanticStack.Pop();

                    semanticStack.Push(new OperatorUsage<int>(operatorName, expressionList));
                    break;

                case "#expressionList":
                    expressionList = (ExpressionList<int>)semanticStack.Pop();
                    expression = (IExpression<int>)semanticStack.Pop();
                    expressionList.Value.Insert(0, expression);
                    semanticStack.Push(expressionList);
                    break;

                case "#emptyExpressionList":
                    semanticStack.Push(new ExpressionList<int>());
                    break;

                case "#variable":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new Variable<int>(name.Value, name.Line, name.Column));
                    break;

                case "#condUsage":
                    exprPairList = (List<KeyValuePair<IExpression<int>, IExpression<int>>>)semanticStack.Pop();
                    expression2 = (IExpression<int>)semanticStack.Pop();
                    expression = (IExpression<int>)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<IExpression<int>, IExpression<int>>(expression, expression2));
                    semanticStack.Push(new CondUsage<int>(exprPairList));
                    break;

                case "#exprPairList":
                    exprPairList = (List<KeyValuePair<IExpression<int>, IExpression<int>>>)semanticStack.Pop();
                    expression2 = (IExpression<int>)semanticStack.Pop();
                    expression = (IExpression<int>)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<IExpression<int>, IExpression<int>>(expression, expression2));
                    semanticStack.Push(exprPairList);
                    break;

                case "#emptyExprPairList":
                    semanticStack.Push(new List<KeyValuePair<IExpression<int>, IExpression<int>>>());
                    break;

                case "#letUsage":
                    expression = (IExpression<int>)semanticStack.Pop();
                    varExprList = (List<KeyValuePair<Variable<int>, IExpression<int>>>)semanticStack.Pop();

                    var letName = (Name)semanticStack.Pop();

                    semanticStack.Push(CreateLetUsage(letName.Value, varExprList, expression));
                    break;

                case "#varExprList":
                    varExprList = (List<KeyValuePair<Variable<int>, IExpression<int>>>)semanticStack.Pop();
                    expression = (IExpression<int>)semanticStack.Pop();
                    variable = (Variable<int>)semanticStack.Pop();
                    varExprList.Insert(0, new KeyValuePair<Variable<int>, IExpression<int>>(variable, expression));
                    semanticStack.Push(varExprList);
                    break;

                case "#emptyVarExprList":
                    semanticStack.Push(new List<KeyValuePair<Variable<int>, IExpression<int>>>());
                    break;

                default:
                    base.ExecuteSemanticAction(semanticStack, action);
                    break;
            }
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            var value = token.TokenValue;

            switch (tokenAsSymbol)
            {
                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(new IntegerLiteral(value));
                    break;

                default:
                    base.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
                    break;
            }
        }
    }
}
