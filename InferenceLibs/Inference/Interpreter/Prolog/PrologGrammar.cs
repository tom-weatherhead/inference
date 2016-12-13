using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.Prolog
{
    public class PrologGrammar : PrologGrammarBase
    {
        // The Prolog grammar from Kamin

        public PrologGrammar()
            : base(GrammarSelector.Prolog)
        {
            Terminals.UnionWith(new HashSet<Symbol>() { Symbol.T_Infer });

            // Start -> Input EOF
            // Input -> Clause
            // Input -> Query
            // Clause -> ( infer Goal from Goal GoalList )
            // Clause -> ( infer Goal )
            // GoalList -> Goal GoalList
            // GoalList -> Lambda
            // Query -> ( infer? Goal GoalList )
            // Goal -> Predicate
            // Goal -> ( Predicate ExpressionList )
            // ExpressionList -> Expression ExpressionList
            // ExpressionList -> Lambda
            // Expression -> Integer
            // Expression -> Variable
            // Expression -> Functor
            // Expression -> ( Functor ExpressionList )
            // Predicate -> Name
            // Functor -> NameNotBeginningWithCapital
            // Variable -> NameBeginningWithCapital
            // Name -> NameNotBeginningWithCapital
            // Name -> NameBeginningWithCapital

            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_EOF }, 1));
            //Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Clause }, 2));
            //Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Query }, 3));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Infer, Symbol.N_Goal, Symbol.T_From, Symbol.N_Goal, Symbol.N_GoalList, Symbol.T_RightBracket, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Infer, Symbol.N_Goal, Symbol.T_RightBracket, "#clauseWithoutSubgoals" }, 5));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 6));
            //Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#emptyGoalList" }, 7));
            Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_LeftBracket, Symbol.T_InferPred, Symbol.N_Goal, Symbol.N_GoalList, Symbol.T_RightBracket, "#goalList" }, 8));
            //Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Predicate, "#goalNoArgs" }, 9));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Predicate, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#goal" }, 10));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#exprList" }, 11));
            //Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExprList" }, 12));
            //Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_NumberOrVariableExpression }, 22));
            //Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_IntegerLiteral }, 13));
            //Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.N_Variable }, 14));
            //Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Functor, "#functorExpressionNoArgs" }, 15));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Functor, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression" }, 16));
            //Productions.Add(new Production(Symbol.N_Predicate, new List<object>() { Symbol.N_Name, "#predicate" }, 17));
            //Productions.Add(new Production(Symbol.N_Functor, new List<object>() { Symbol.T_NameNotBeginningWithCapital, "#functor" }, 18));
            //Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_NameBeginningWithCapital, "#variable" }, 19));
            //Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameNotBeginningWithCapital }, 20));
            //Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameBeginningWithCapital }, 21));
            // Production 22 is between 12 and 13.
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            List<IPrologExpression> exprList;
            PrologPredicate pred;
            PrologFunctor functor;

            switch (action)
            {
                case "#goal":
                    exprList = (List<IPrologExpression>)semanticStack.Pop();
                    pred = (PrologPredicate)semanticStack.Pop();
                    semanticStack.Push(new PrologGoal(gs, pred, exprList));
                    break;

                case "#functorExpression":
                    exprList = (List<IPrologExpression>)semanticStack.Pop();
                    functor = (PrologFunctor)semanticStack.Pop();
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, exprList));
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
                        case "infer": return Symbol.T_Infer;
                        case "infer?": return Symbol.T_InferPred;
                        case "from": return Symbol.T_From;
                        default: break;
                    }

                    if (char.IsUpper(tokenValueAsString, 0))
                    {
                        return Symbol.T_NameBeginningWithCapital;
                    }
                    else
                    {
                        return Symbol.T_NameNotBeginningWithCapital;
                    }

                default:
                    break;
            }

            return base.TokenToSymbol(token);
        }
    }
}
