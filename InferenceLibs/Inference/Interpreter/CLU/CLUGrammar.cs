using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter;
using Inference.Parser;

namespace Inference.Interpreter.CLU
{
    public class CLUGrammar : InterpreterGrammarBase
    {
        public CLUGrammar()
            : base()
        {
            Productions.RemoveAt(18 - 1);   // Remove this production: Optr -> Function

            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_Cluster, Symbol.T_Rep, Symbol.T_Dollar, Symbol.T_Export });

            NonTerminals.UnionWith(new HashSet<Symbol>() {
                Symbol.N_ClusterDef, /* Symbol.N_Cluster, */ Symbol.N_Rep,
                Symbol.N_FunDefList, Symbol.N_OnePartName, Symbol.N_TwoPartName,
                Symbol.N_ExportList, Symbol.N_OnePartNameList });

            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_ClusterDef }, 39));
            Productions.Add(new Production(Symbol.N_ClusterDef, new List<object>() {
                Symbol.T_LeftBracket,
                Symbol.T_Cluster,
                Symbol.T_ID, // This T_ID is really a Symbol.N_Cluster
                Symbol.N_ExportList,
                Symbol.N_Rep,
                Symbol.N_FunDef,
                Symbol.N_FunDefList,
                Symbol.T_RightBracket, "#clusterDefinition" }, 40));
            Productions.Add(new Production(Symbol.N_Rep, new List<object>() {
                Symbol.T_LeftBracket,
                Symbol.T_Rep,
                Symbol.N_Variable,
                Symbol.N_VariableList,
                Symbol.T_RightBracket, "#variableList" }, 41));
            Productions.Add(new Production(Symbol.N_FunDefList, new List<object>() { Symbol.N_FunDef, Symbol.N_FunDefList, "#funDefList" }, 42));
            Productions.Add(new Production(Symbol.N_FunDefList, new List<object>() { Symbol.Lambda, "#emptyFunDefList" }, 43));
            Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_OnePartName }, 44));
            Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_TwoPartName }, 45));
            Productions.Add(new Production(Symbol.N_OnePartName, new List<object>() { Symbol.T_ID }, 46));
            Productions.Add(new Production(Symbol.N_TwoPartName, new List<object>() {
                Symbol.T_ID, // This T_ID is really a Symbol.N_Cluster
                Symbol.T_Dollar,
                Symbol.T_ID, "#makeTwoPartName" }, 47));
            // SLR(1): There was a reduce-reduce conflict between N_OnePartName -> T_ID and N_Cluster -> T_ID.
            //Productions.Add(new Production(Symbol.N_Cluster, new List<object>() { Symbol.T_ID }, 48));
            Productions.Add(new Production(Symbol.N_ExportList, new List<object>() {
                Symbol.T_LeftBracket,
                Symbol.T_Export,
                Symbol.N_OnePartName,
                Symbol.N_OnePartNameList,
                Symbol.T_RightBracket, "#exportList" }, 49));
            Productions.Add(new Production(Symbol.N_OnePartNameList, new List<object>() { Symbol.N_OnePartName, Symbol.N_OnePartNameList, "#exportList" }, 50));
            Productions.Add(new Production(Symbol.N_OnePartNameList, new List<object>() { Symbol.Lambda, "#emptyExportList" }, 51));
        }

        private ICLUExpression CreateLetUsage(string letKeyword,
            List<KeyValuePair<CLUVariable, ICLUExpression>> varExprList, ICLUExpression expression)
        {

            switch (letKeyword)
            {
                case "let":
                    return new CLULetUsage(varExprList, expression);

                case "let*":
                    return new CLULetStarUsage(varExprList, expression);

                default:
                    throw new ArgumentException(string.Format("CLUGrammar.CreateLetUsage() : Unknown 'let' keyword '{0}'.", letKeyword));
            }
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            Name name;
            Name functionName;
            CLUVariable variable;
            List<CLUVariable> variableList;
            ICLUExpression expression;
            ICLUExpression expression2;
            List<ICLUExpression> expressionList;
            CLUFunctionDefinitionBase funDef;
            List<CLUFunctionDefinitionBase> funDefList;
            HashSet<string> exportSet;
            List<KeyValuePair<ICLUExpression, ICLUExpression>> exprPairList;
            List<KeyValuePair<CLUVariable, ICLUExpression>> varExprList;

            switch (action)
            {
                case "#functionDefinition":
                    var body = (ICLUExpression)semanticStack.Pop();
                    var argList = (List<CLUVariable>)semanticStack.Pop();

                    functionName = (Name)semanticStack.Pop();
                    semanticStack.Push(new CLUNormalFunctionDefinition(functionName.Value, argList, body));
                    break;

                case "#variableList":
                    variableList = (List<CLUVariable>)semanticStack.Pop();
                    variable = (CLUVariable)semanticStack.Pop();
                    variableList.Insert(0, variable);
                    semanticStack.Push(variableList);
                    break;

                case "#emptyVariableList":
                    semanticStack.Push(new List<CLUVariable>());
                    break;

                case "#if":
                    var expression3 = (ICLUExpression)semanticStack.Pop();

                    expression2 = (ICLUExpression)semanticStack.Pop();
                    expression = (ICLUExpression)semanticStack.Pop();
                    semanticStack.Push(new CLUIfUsage(expression, expression2, expression3));
                    break;

                case "#while":
                    expression2 = (ICLUExpression)semanticStack.Pop();
                    expression = (ICLUExpression)semanticStack.Pop();
                    semanticStack.Push(new CLUWhileUsage(expression, expression2));
                    break;

                case "#set":
                    expression = (ICLUExpression)semanticStack.Pop();
                    variable = (CLUVariable)semanticStack.Pop();
                    semanticStack.Push(new CLUSetUsage(variable, expression));
                    break;

                case "#begin":
                    expressionList = (List<ICLUExpression>)semanticStack.Pop();
                    expression = (ICLUExpression)semanticStack.Pop();
                    semanticStack.Push(new CLUBeginUsage(expression, expressionList));
                    break;

                case "#operatorUsage":
                    expressionList = (List<ICLUExpression>)semanticStack.Pop();

                    var operatorNameAsObject = semanticStack.Pop();   // TODO: This may be a one-part name or a two-part name.
                    ICLUFunctionName operatorName;

                    if (operatorNameAsObject is TwoPartFunctionName)
                    {
                        operatorName = (TwoPartFunctionName)operatorNameAsObject;
                    }
                    else
                    {
                        name = (Name)operatorNameAsObject;
                        operatorName = new OnePartFunctionName(name.Value);
                    }

                    semanticStack.Push(new CLUOperatorUsage(operatorName, expressionList));
                    break;

                case "#expressionList":
                    expressionList = (List<ICLUExpression>)semanticStack.Pop();
                    expression = (ICLUExpression)semanticStack.Pop();
                    expressionList.Insert(0, expression);
                    semanticStack.Push(expressionList);
                    break;

                case "#emptyExpressionList":
                    semanticStack.Push(new List<ICLUExpression>());
                    break;

                case "#variable":
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new CLUVariable(name.Value));
                    break;

                case "#makeTwoPartName":
                    functionName = (Name)semanticStack.Pop();

                    var clusterName = (Name)semanticStack.Pop();

                    semanticStack.Push(new TwoPartFunctionName(clusterName.Value, functionName.Value));
                    break;

                case "#funDefList":
                    funDefList = (List<CLUFunctionDefinitionBase>)semanticStack.Pop();
                    funDef = (CLUFunctionDefinitionBase)semanticStack.Pop();
                    funDefList.Insert(0, funDef);
                    semanticStack.Push(funDefList);
                    break;

                case "#emptyFunDefList":
                    semanticStack.Push(new List<CLUFunctionDefinitionBase>());
                    break;

                case "#clusterDefinition":
                    funDefList = (List<CLUFunctionDefinitionBase>)semanticStack.Pop();
                    funDef = (CLUFunctionDefinitionBase)semanticStack.Pop();
                    funDefList.Insert(0, funDef);
                    variableList = (List<CLUVariable>)semanticStack.Pop();
                    exportSet = (HashSet<string>)semanticStack.Pop();
                    name = (Name)semanticStack.Pop();
                    semanticStack.Push(new Cluster(name.Value, exportSet, variableList, funDefList));
                    break;

                case "#exportList":
                    exportSet = (HashSet<string>)semanticStack.Pop();
                    name = (Name)semanticStack.Pop();
                    exportSet.Add(name.Value);
                    semanticStack.Push(exportSet);
                    break;

                case "#emptyExportList":
                    semanticStack.Push(new HashSet<string>());
                    break;

                case "#condUsage":
                    exprPairList = (List<KeyValuePair<ICLUExpression, ICLUExpression>>)semanticStack.Pop();
                    expression2 = (ICLUExpression)semanticStack.Pop();
                    expression = (ICLUExpression)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<ICLUExpression, ICLUExpression>(expression, expression2));
                    semanticStack.Push(new CLUCondUsage(exprPairList));
                    break;

                case "#exprPairList":
                    exprPairList = (List<KeyValuePair<ICLUExpression, ICLUExpression>>)semanticStack.Pop();
                    expression2 = (ICLUExpression)semanticStack.Pop();
                    expression = (ICLUExpression)semanticStack.Pop();
                    exprPairList.Insert(0, new KeyValuePair<ICLUExpression, ICLUExpression>(expression, expression2));
                    semanticStack.Push(exprPairList);
                    break;

                case "#emptyExprPairList":
                    semanticStack.Push(new List<KeyValuePair<ICLUExpression, ICLUExpression>>());
                    break;

                case "#letUsage":
                    expression = (ICLUExpression)semanticStack.Pop();
                    varExprList = (List<KeyValuePair<CLUVariable, ICLUExpression>>)semanticStack.Pop();

                    var letName = (Name)semanticStack.Pop();

                    semanticStack.Push(CreateLetUsage(letName.Value, varExprList, expression));
                    break;

                case "#varExprList":
                    varExprList = (List<KeyValuePair<CLUVariable, ICLUExpression>>)semanticStack.Pop();
                    expression = (ICLUExpression)semanticStack.Pop();
                    variable = (CLUVariable)semanticStack.Pop();
                    varExprList.Insert(0, new KeyValuePair<CLUVariable, ICLUExpression>(variable, expression));
                    semanticStack.Push(varExprList);
                    break;

                case "#emptyVarExprList":
                    semanticStack.Push(new List<KeyValuePair<CLUVariable, ICLUExpression>>());
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
                        case "cluster": return Symbol.T_Cluster;
                        case "rep": return Symbol.T_Rep;
                        case "export": return Symbol.T_Export;
                        default: break;
                    }

                    break;

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
                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(new CLUPrimitiveValue((int)value));
                    break;

                default:
                    base.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
                    break;
            }
        }
    }
}
