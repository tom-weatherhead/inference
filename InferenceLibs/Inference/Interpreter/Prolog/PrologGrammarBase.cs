using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.Prolog
{
    public class PrologGrammarBase : GrammarBase
    {
        protected readonly GrammarSelector gs;

        public PrologGrammarBase(GrammarSelector gsParam)
            : base(Symbol.N_Start)
        {
            gs = gsParam;

            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_LeftBracket, Symbol.T_RightBracket, Symbol.T_From,
                Symbol.T_InferPred, Symbol.T_IntegerLiteral, Symbol.T_NameBeginningWithCapital, Symbol.T_NameNotBeginningWithCapital,
                Symbol.T_EOF });

            NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
                Symbol.N_Input, Symbol.N_Clause, Symbol.N_Query, Symbol.N_Goal,
                Symbol.N_GoalList, Symbol.N_Predicate, Symbol.N_Expression, Symbol.N_ExpressionList,
                Symbol.N_Functor, Symbol.N_Variable, Symbol.N_Name, Symbol.N_NumberOrVariableExpression });

            //Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Clause }, 2));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Query }, 3));
            //Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Infer, Symbol.N_Goal, Symbol.T_From, Symbol.N_Goal, Symbol.N_GoalList, Symbol.T_RightBracket, "#clauseWithSubgoals" }, 4));
            //Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Infer, Symbol.N_Goal, Symbol.T_RightBracket, "#clauseWithoutSubgoals" }, 5));
            //Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 6));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#emptyGoalList" }, 7));
            //Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_LeftBracket, Symbol.T_InferPred, Symbol.N_Goal, Symbol.N_GoalList, Symbol.T_RightBracket, "#goalList" }, 8));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Predicate, "#goalNoArgs" }, 9));
            //Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Predicate, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#goal" }, 10));
            //Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#exprList" }, 11));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExprList" }, 12));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_NumberOrVariableExpression }, 22));
            Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_IntegerLiteral }, 13));
            Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.N_Variable }, 14));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Functor, "#functorExpressionNoArgs" }, 15));
            //Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Functor, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression" }, 16));
            Productions.Add(new Production(Symbol.N_Predicate, new List<object>() { Symbol.N_Name, "#predicate" }, 17));
            Productions.Add(new Production(Symbol.N_Functor, new List<object>() { Symbol.T_NameNotBeginningWithCapital, "#functor" }, 18));
            Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_NameBeginningWithCapital, "#variable" }, 19));
#if DEAD_CODE
            Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.N_Functor, "#functorToString" }, 20));
            Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.N_Variable, "#variableToString" }, 21));
#else
            Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameNotBeginningWithCapital }, 20));
            Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameBeginningWithCapital }, 21));
#endif
            // Production 22 is between 12 and 13.
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            string str;
            PrologGoal goal;
            PrologGoal subgoal;
            List<PrologGoal> goalList;
            List<PrologGoal> subgoalList;
            IPrologExpression expr;
            List<IPrologExpression> exprList;
            PrologPredicate pred;
            PrologFunctor functor;
#if DEAD_CODE
            PrologVariable variable;
#endif

            switch (action)
            {
                case "#clauseWithSubgoals":
                    subgoalList = (List<PrologGoal>)semanticStack.Pop();
                    subgoal = (PrologGoal)semanticStack.Pop();
                    goal = (PrologGoal)semanticStack.Pop();
                    subgoalList.Insert(0, subgoal);
                    semanticStack.Push(new PrologClause(goal, subgoalList));
                    break;

                case "#clauseWithoutSubgoals":
                    goal = (PrologGoal)semanticStack.Pop();
                    semanticStack.Push(new PrologClause(goal, new List<PrologGoal>()));
                    break;

                case "#goalList":
                    goalList = (List<PrologGoal>)semanticStack.Pop();
                    goal = (PrologGoal)semanticStack.Pop();
                    goalList.Insert(0, goal);
                    semanticStack.Push(goalList);
                    break;

                case "#emptyGoalList":
                    semanticStack.Push(new List<PrologGoal>());
                    break;

                case "#exprList":
                    exprList = (List<IPrologExpression>)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    exprList.Insert(0, expr);
                    semanticStack.Push(exprList);
                    break;

                case "#emptyExprList":
                    semanticStack.Push(new List<IPrologExpression>());
                    break;

                case "#predicate":
                    str = (string)semanticStack.Pop();
                    semanticStack.Push(new PrologPredicate(str));
                    break;

                case "#goalNoArgs":
                    pred = (PrologPredicate)semanticStack.Pop();
                    semanticStack.Push(new PrologGoal(gs, pred, new List<IPrologExpression>()));
                    break;

                case "#functor":
                    str = (string)semanticStack.Pop();
                    semanticStack.Push(new PrologFunctor(str));
                    break;

#if DEAD_CODE
                case "#functorToString":
                    functor = (PrologFunctor)semanticStack.Pop();
                    semanticStack.Push(functor.Name);
                    break;
#endif

                case "#functorExpressionNoArgs":
                    functor = (PrologFunctor)semanticStack.Pop();
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>()));
                    break;

                case "#variable":
                    str = (string)semanticStack.Pop();
                    semanticStack.Push(new PrologVariable(str));
                    break;

#if DEAD_CODE
                case "#variableToString":
                    variable = (PrologVariable)semanticStack.Pop();
                    semanticStack.Push(variable.Name);
                    break;
#endif

                default:
                    throw new ArgumentException(string.Format("Unrecognized semantic action: {0}", action), "action");
            }
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            var value = token.TokenValue;

            switch (tokenAsSymbol)
            {
                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(new PrologIntegerLiteral((int)value));
                    break;

                case Symbol.T_NameBeginningWithCapital:
                case Symbol.T_NameNotBeginningWithCapital:
                    semanticStack.Push(value);  // value is really a string; it must be converted to a Prolog domain model type later.
                    break;

                default:
                    break;
            }
        }
    }
}
