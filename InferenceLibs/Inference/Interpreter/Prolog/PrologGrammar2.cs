using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.Prolog
{
    public class PrologGrammar2 : PrologGrammarBase
    {
        // This should be as close as possible to the grammar of "real" Prolog.
        // For a Prolog tutorial, see http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/contents.html

        // TODO 2013/02/07: Test functors, if we haven't done so already.  Does functor unification work properly?
        // -> Yes; we have tested functors such as cons and nil.

        public PrologGrammar2()
            : base(GrammarSelector.Prolog2)
        {
            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_Comma, Symbol.T_Dot, Symbol.T_Is, Symbol.T_Plus,
                Symbol.T_Minus, Symbol.T_Multiply, Symbol.T_Divide, Symbol.T_LessThan,
                Symbol.T_GreaterThan, Symbol.T_LessEqual, Symbol.T_GreaterEqual, Symbol.T_StringLiteral,
                Symbol.T_Assert, Symbol.T_AssertA, Symbol.T_AssertZ, Symbol.T_Retract,
                Symbol.T_Not, Symbol.T_LeftSquareBracket, Symbol.T_RightSquareBracket, Symbol.T_OrBar,
                Symbol.T_NotSymbol, Symbol.T_IfThen, Symbol.T_Colon, Symbol.T_Assign,
                Symbol.T_Equals, Symbol.T_NotEqual, Symbol.T_NotUnifiable, Symbol.T_RetractAll,
                Symbol.T_Semicolon, Symbol.T_Mod, Symbol.T_ArithmeticEquals, Symbol.T_ArithmeticNotEquals });

            NonTerminals.UnionWith(new HashSet<Symbol>() {
                Symbol.N_ComparisonOperator, Symbol.N_MetaPredicateWithClause, Symbol.N_ListTail, Symbol.N_ListContents,
                Symbol.N_Sequence, Symbol.N_ArithmeticExpression1, Symbol.N_ArithmeticExpression2, Symbol.N_GoalDisjunctionTail });

            // Start -> Input EOF
            // Input -> Clause
            // Input -> Query
            // Clause -> Goal :- Goal GoalList .
            // Clause -> Goal .
            // GoalList -> , Goal GoalList
            // GoalList -> Lambda
            // Query -> ?- Goal GoalList .
            // Goal -> Predicate
            // Goal -> Predicate ( Expression ExpressionList )
            // ExpressionList -> , Expression ExpressionList
            // ExpressionList -> Lambda
            // Expression -> Integer
            // Expression -> Variable
            // Expression -> Functor
            // Expression -> Functor ( Expression ExpressionList )
            // Predicate -> Name
            // Functor -> NameNotBeginningWithCapital
            // Variable -> NameBeginningWithCapital
            // Name -> NameNotBeginningWithCapital
            // Name -> NameBeginningWithCapital

            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_Dot, Symbol.T_EOF }, 1));
            //Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Clause }, 2));
            //Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Query }, 3));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, Symbol.T_From, Symbol.N_Goal, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, "#clauseWithoutSubgoals" }, 5));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.T_Comma, Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 6));
            //Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#emptyGoalList" }, 7));
            Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_InferPred, Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 8));
            //Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Predicate, "#goalNoArgs" }, 9));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Predicate, Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#goal2" }, 10));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExpressionList, "#exprList" }, 11));
            //Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExprList" }, 12));
            //Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_NumberOrVariableExpression }, 22));
            //Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_IntegerLiteral }, 13));
            //Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.N_Variable }, 14));
            //Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Functor, "#functorExpressionNoArgs" }, 15));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Functor, Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression2" }, 16));
            //Productions.Add(new Production(Symbol.N_Predicate, new List<object>() { Symbol.N_Name, "#predicate" }, 17));
            //Productions.Add(new Production(Symbol.N_Functor, new List<object>() { Symbol.T_NameNotBeginningWithCapital, "#functor" }, 18));
            //Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_NameBeginningWithCapital, "#variable" }, 19));
            //Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameNotBeginningWithCapital }, 20));
            //Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameBeginningWithCapital }, 21));
            // Production 22 is between 12 and 13.

            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.N_NumberOrVariableExpression, Symbol.T_Is, Symbol.N_ArithmeticExpression1, "#is" }, 23));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1, new List<object>() { Symbol.N_ArithmeticExpression2 }, 24));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1, new List<object>() {
                Symbol.N_ArithmeticExpression1, Symbol.T_Plus, Symbol.N_ArithmeticExpression2, "#arithExpr" }, 25));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1, new List<object>() {
                Symbol.N_ArithmeticExpression1, Symbol.T_Minus, Symbol.N_ArithmeticExpression2, "#arithExpr" }, 26));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2, new List<object>() { Symbol.N_NumberOrVariableExpression }, 27));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2, new List<object>() {
                Symbol.N_ArithmeticExpression2, Symbol.T_Multiply, Symbol.N_NumberOrVariableExpression, "#arithExpr" }, 28));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2, new List<object>() {
                Symbol.N_ArithmeticExpression2, Symbol.T_Divide, Symbol.N_NumberOrVariableExpression, "#arithExpr" }, 29));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2, new List<object>() {
                Symbol.T_Mod, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket, "#mod" }, 61));
#if DEAD_CODE
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.N_NumberOrVariableExpression, Symbol.N_ComparisonOperator, Symbol.N_NumberOrVariableExpression, "#comparisonOperator" }, 30));
#else
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.N_ArithmeticExpression1, Symbol.N_ComparisonOperator, Symbol.N_ArithmeticExpression1, "#comparisonOperator" }, 30));
#endif
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_LessThan }, 31));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_GreaterThan }, 32));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_LessEqual }, 33));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_GreaterEqual }, 34));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_ArithmeticEquals }, 62));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_ArithmeticNotEquals }, 63));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_StringLiteral }, 35));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.T_Not, Symbol.T_LeftBracket, Symbol.N_Goal, Symbol.T_RightBracket, "#not" }, 36));   // #metaPredicateWithGoal
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.N_MetaPredicateWithClause, Symbol.T_LeftBracket, Symbol.N_Clause, Symbol.T_RightBracket, "#metaPredicateWithClause" }, 37));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_Assert }, 38));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_AssertA }, 39));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_AssertZ }, 40));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_Retract }, 41));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_RetractAll }, 42));
            // Lists
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() {
                Symbol.T_LeftSquareBracket, Symbol.N_ListContents, Symbol.T_RightSquareBracket }, 43));
            Productions.Add(new Production(Symbol.N_ListContents, new List<object>() { Symbol.Lambda, "#nil" }, 44));
            Productions.Add(new Production(Symbol.N_ListContents, new List<object>() { Symbol.N_Expression, Symbol.N_ListTail, "#cons" }, 45));
            Productions.Add(new Production(Symbol.N_ListTail, new List<object>() { Symbol.T_OrBar, Symbol.N_Expression }, 46));
            Productions.Add(new Production(Symbol.N_ListTail, new List<object>() { Symbol.Lambda, "#nil" }, 47));
            Productions.Add(new Production(Symbol.N_ListTail, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ListTail, "#cons" }, 48));
            // Sequences
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Sequence, Symbol.T_RightBracket }, 49));
            Productions.Add(new Production(Symbol.N_Sequence, new List<object>() { Symbol.N_Expression }, 50)); // (a) = a
            Productions.Add(new Production(Symbol.N_Sequence, new List<object>() { Symbol.N_Expression, Symbol.T_Comma, Symbol.N_Sequence, "#consSeq" }, 51));
            // The "not" symbol: \+
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.T_NotSymbol, Symbol.N_Goal, "#not" }, 52));
            // Goal disjunction: See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_9.html
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.N_Goal, Symbol.T_Semicolon, Symbol.N_Goal, Symbol.N_GoalDisjunctionTail, "#goalDisjunction2" }, 53));
            Productions.Add(new Production(Symbol.N_GoalDisjunctionTail, new List<object>() { Symbol.Lambda, "#null" }, 54));
            Productions.Add(new Production(Symbol.N_GoalDisjunctionTail, new List<object>() {
                Symbol.T_Semicolon, Symbol.N_Goal, Symbol.N_GoalDisjunctionTail, "#goalDisjunction" }, 55));
            // If-then-else
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Goal, Symbol.T_IfThen, Symbol.N_Goal, Symbol.T_Colon, Symbol.N_Goal, Symbol.T_RightBracket, "#ifThenElse" }, 56));
            // Unifiable: =
#if !DEAD_CODE
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_NumberOrVariableExpression, Symbol.T_Assign, Symbol.N_Expression, "#=" }, 57));
#else
            // LALR(1) ?  No: e.g. ?- mia(t) = mia(X), print(X).
            // Reduce-reduce conflict:
            // N_Functor -> T_NameNotBeginningWithCapital
            // N_Name -> T_NameNotBeginningWithCapital
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Expression, Symbol.T_Assign, Symbol.N_Expression, "#=" }, 57));
#endif
            // Equals: ==
            //Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Expression, Symbol.T_Equals, Symbol.N_Expression, "#==" }, 57));
            //Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.T_Equals, Symbol.N_Expression, Symbol.T_RightBracket, "#==" }, 57));
            // We use N_NumberOrVariableExpression instead of N_Expression in order to avoid a shift-reduce or reduce-reduce conflict.
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_NumberOrVariableExpression, Symbol.T_Equals, Symbol.N_Expression, "#==" }, 58));
            // Not equal: \==
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_NumberOrVariableExpression, Symbol.T_NotEqual, Symbol.N_Expression, "#notEqual" }, 59));
            // Not unifiable: \=
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_NumberOrVariableExpression, Symbol.T_NotUnifiable, Symbol.N_Expression, "#notUnifiable" }, 60));

            // Production 61 is between 29 and 30.
            // Productions 62 and 63 are between 34 and 35.
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            string str;
            string str2;
            IPrologExpression expr;
            IPrologExpression expr2;
            List<IPrologExpression> exprList;
            PrologPredicate pred;
            PrologFunctor functor;
            PrologGoal innerGoal;
            PrologClause innerClause;
            object obj;
            PrologGoal goal;
            PrologGoal goal2;
            PrologGoal goal3;
            //PrologVariable variable;

            switch (action)
            {
                case "#goal2":
                    exprList = (List<IPrologExpression>)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    pred = (PrologPredicate)semanticStack.Pop();
                    exprList.Insert(0, expr);
                    semanticStack.Push(new PrologGoal(gs, pred, exprList));
                    break;

                case "#functorExpression2":
                    exprList = (List<IPrologExpression>)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    functor = (PrologFunctor)semanticStack.Pop();
                    exprList.Insert(0, expr);
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, exprList));
                    break;

                case "#is":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    pred = new PrologPredicate("is");
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    semanticStack.Push(new PrologGoal(gs, pred, exprList));
                    break;

                case "#arithExpr":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    str = (string)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    functor = new PrologFunctor(str);
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, exprList));
                    break;

                case "#mod":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    functor = new PrologFunctor("mod");
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, exprList));
                    break;

                case "#comparisonOperator":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    str = (string)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();

                    switch (str)
                    {
                        case "<":
                            str2 = "less";
                            break;

                        case ">":
                            str2 = "greater";
                            break;

                        case "<=":
                            str2 = "less_equal";
                            break;

                        case ">=":
                            str2 = "greater_equal";
                            break;

                        case "=:=":
                            str2 = "arithmetic_equals";
                            break;

                        case @"=\=":
                            str2 = "arithmetic_not_equals";
                            break;

                        default:
                            throw new Exception(string.Format("Unknown comparison operator '{0}'.", str));
                    }

                    pred = new PrologPredicate(str2);
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    semanticStack.Push(new PrologGoal(gs, pred, exprList));
                    break;

                case "#not":    // #metaPredicateWithGoal
                    innerGoal = (PrologGoal)semanticStack.Pop();
                    //str = (string)semanticStack.Pop();
                    pred = new PrologPredicate("not");
                    semanticStack.Push(new PrologGoalWithInner<PrologGoal>(gs, pred, innerGoal));
                    break;

                case "#metaPredicateWithClause":
                    innerClause = (PrologClause)semanticStack.Pop();
                    str = (string)semanticStack.Pop();
                    pred = new PrologPredicate(str);
                    semanticStack.Push(new PrologGoalWithInner<PrologClause>(gs, pred, innerClause));
                    break;

                case "#nil":
                    functor = new PrologFunctor("nil");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>()));
                    break;

                case "#cons":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    functor = new PrologFunctor("cons");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>() { expr, expr2 }));
                    break;

                case "#consSeq":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    functor = new PrologFunctor("consSeq");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>() { expr, expr2 }));
                    break;

                case "#null":
                    semanticStack.Push(null);
                    break;

                case "#goalDisjunction":
                    obj = semanticStack.Pop();

                    if (obj == null)
                    {
                        break;
                    }

                    goal2 = (PrologGoal)obj;
                    goal = (PrologGoal)semanticStack.Pop();
                    semanticStack.Push(new PrologGoalDisjunction(gs, goal, goal2));
                    break;

                case "#goalDisjunction2":
                    ExecuteSemanticAction(semanticStack, "#goalDisjunction");
                    ExecuteSemanticAction(semanticStack, "#goalDisjunction");
                    break;

                case "#ifThenElse":
                    goal3 = (PrologGoal)semanticStack.Pop();
                    goal2 = (PrologGoal)semanticStack.Pop();
                    goal = (PrologGoal)semanticStack.Pop();
                    semanticStack.Push(new PrologGoalIfThenElse(gs, goal, goal2, goal3));
                    break;

                case "#=":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    pred = new PrologPredicate("unifiable");
                    semanticStack.Push(new PrologGoal(gs, pred, exprList));
                    break;

                case "#==":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    pred = new PrologPredicate("==");
                    semanticStack.Push(new PrologGoal(gs, pred, exprList));
                    break;

                case "#notEqual":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    pred = new PrologPredicate("not-equal");
                    semanticStack.Push(new PrologGoal(gs, pred, exprList));
                    break;

                case "#notUnifiable":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    /*
                    pred = new PrologPredicate("unifiable");
                    innerGoal = new PrologGoal(gs, pred, exprList);
                    pred = new PrologPredicate("not");
                    semanticStack.Push(new PrologGoalWithInner<PrologGoal>(gs, pred, innerGoal));
                     */
                    pred = new PrologPredicate("not-unifiable");
                    semanticStack.Push(new PrologGoal(gs, pred, exprList));
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
                        case "?-": return Symbol.T_InferPred;
                        case ":-": return Symbol.T_From;
                        case "is": return Symbol.T_Is;
                        case "+": return Symbol.T_Plus;
                        case "-": return Symbol.T_Minus;
                        case "*": return Symbol.T_Multiply;
                        case "/": return Symbol.T_Divide;
                        case "mod": return Symbol.T_Mod;
                        case "<": return Symbol.T_LessThan;
                        case ">": return Symbol.T_GreaterThan;
                        case "<=": return Symbol.T_LessEqual;
                        case ">=": return Symbol.T_GreaterEqual;
                        case "assert": return Symbol.T_Assert;
                        case "asserta": return Symbol.T_AssertA;
                        case "assertz": return Symbol.T_AssertZ;
                        case "retract": return Symbol.T_Retract;
                        case "retractall": return Symbol.T_RetractAll;
                        case "not": return Symbol.T_Not;
                        case @"\+": return Symbol.T_NotSymbol;
                        case "->": return Symbol.T_IfThen;
                        case ":": return Symbol.T_Colon;
                        case "=": return Symbol.T_Assign;
                        case "==": return Symbol.T_Equals;
                        case @"\==": return Symbol.T_NotEqual;
                        case @"\=": return Symbol.T_NotUnifiable;
                        case "=:=": return Symbol.T_ArithmeticEquals; // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse21
                        case @"=\=": return Symbol.T_ArithmeticNotEquals;
                        default: break;
                    }

                    if (char.IsUpper(tokenValueAsString, 0)
                        // The following supports non-binding variables such as _ and _Foo .
                        // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_3.html
                        // TODO: Should we require the second character (if it exists) to be a capital letter if the first is an underscore?
                        || tokenValueAsString.StartsWith("_"))
                    {
                        return Symbol.T_NameBeginningWithCapital;
                    }
                    else
                    {
                        return Symbol.T_NameNotBeginningWithCapital;
                    }

                case TokenType.T_StrLit: return Symbol.T_StringLiteral;
                case TokenType.T_Comma: return Symbol.T_Comma;
                case TokenType.T_Dot: return Symbol.T_Dot;
                case TokenType.T_LeftSquareBracket: return Symbol.T_LeftSquareBracket;
                case TokenType.T_RightSquareBracket: return Symbol.T_RightSquareBracket;
                case TokenType.T_OrBar: return Symbol.T_OrBar;
                case TokenType.T_Semicolon: return Symbol.T_Semicolon;
                default: break;
            }

            return base.TokenToSymbol(token);
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            var value = token.TokenValue;

            switch (tokenAsSymbol)
            {
                case Symbol.T_StringLiteral:
                    semanticStack.Push(new PrologStringLiteral((string)value));
                    break;

                case Symbol.T_Plus:
                case Symbol.T_Minus:
                case Symbol.T_Multiply:
                case Symbol.T_Divide:
                case Symbol.T_LessThan:
                case Symbol.T_GreaterThan:
                case Symbol.T_LessEqual:
                case Symbol.T_GreaterEqual:
                case Symbol.T_Assert:
                case Symbol.T_AssertA:
                case Symbol.T_AssertZ:
                case Symbol.T_Retract:
                case Symbol.T_RetractAll:
                case Symbol.T_ArithmeticEquals:
                case Symbol.T_ArithmeticNotEquals:
                    //case Symbol.T_Not:
                    semanticStack.Push(value);
                    break;

                default:
                    base.PushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
                    break;
            }
        }
    }
}
