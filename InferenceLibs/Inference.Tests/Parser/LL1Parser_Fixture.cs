//#define PROLOG_TEST_GRAMMAR
//#define SLR1_PROLOG
#define LL1_PROLOG_ATTEMPT2

using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Domain;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Parser
{
#if PROLOG_TEST_GRAMMAR
#region LL1PrologTestGrammar

    public class LL1PrologTestGrammar : PrologTestGrammarBase // PrologTestGrammarBase is defined in SLR1Parser_Fixture.cs
    {
        public LL1PrologTestGrammar()
        {
            Terminals.UnionWith(new HashSet<Symbol>() { // From PrologGrammarBase
                Symbol.T_LeftBracket, Symbol.T_RightBracket, Symbol.T_From,
                Symbol.T_InferPred, Symbol.T_IntegerLiteral, Symbol.T_NameBeginningWithCapital, Symbol.T_NameNotBeginningWithCapital,
                Symbol.T_EOF });

            Terminals.UnionWith(new HashSet<Symbol>() { // From PrologGrammar2
                Symbol.T_Comma, Symbol.T_Dot, Symbol.T_Is, Symbol.T_Plus,
                Symbol.T_Minus, Symbol.T_Multiply, Symbol.T_Divide, Symbol.T_LessThan,
                Symbol.T_GreaterThan, Symbol.T_LessEqual, Symbol.T_GreaterEqual, Symbol.T_StringLiteral,
                Symbol.T_Assert, Symbol.T_AssertA, Symbol.T_AssertZ, Symbol.T_Retract,
                Symbol.T_Not, Symbol.T_LeftSquareBracket, Symbol.T_RightSquareBracket, Symbol.T_OrBar,
                Symbol.T_NotSymbol, Symbol.T_IfThen, Symbol.T_Colon, Symbol.T_Assign,
                Symbol.T_Equals, Symbol.T_NotEqual, Symbol.T_NotUnifiable, Symbol.T_RetractAll,
                Symbol.T_Semicolon, Symbol.T_Mod, Symbol.T_ArithmeticEquals, Symbol.T_ArithmeticNotEquals });

#if LL1_PROLOG_ATTEMPT2
            NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
                Symbol.N_Input, Symbol.N_Clause, Symbol.N_Query, Symbol.N_Goal,
                Symbol.N_ClauseTail, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_PossibleDisjunctiveTail,
                Symbol.N_GoalList, Symbol.N_Predicate, Symbol.N_Expression, //Symbol.N_ExpressionList,
                //Symbol.N_Functor, Symbol.N_Variable, Symbol.N_Name, Symbol.N_NumberOrVariableExpression
                Symbol.N_IfThenElseTail, Symbol.N_ExpressionListTail, Symbol.N_ExpressionTail, Symbol.N_ComparisonTail,
                Symbol.N_ListContentsTail, Symbol.N_ExpressionPartFollowingAnInteger, Symbol.N_ExpressionPartFollowingAnUpperCaseID,
                Symbol.N_ExpressionPartFollowingALowerCaseID, Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams,
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_LHSGoal, Symbol.N_LHSGoalTail,
                // Arithmetic
                Symbol.N_ArithmeticExpression1, Symbol.N_ArithmeticExpression2, Symbol.N_ArithmeticExpression3,
                Symbol.N_ArithmeticExpression1Foo, Symbol.N_ArithmeticExpression2Foo, Symbol.N_OpType_Add, Symbol.N_OpType_Multiply,
                // Arithmetic comparisons
                Symbol.N_ComparisonOperator, Symbol.N_ArithmeticAndComparisonTail, //, Symbol.N_GoalBeginningWithArithmeticExpression, Symbol.N_ArithmeticExpressionTail1, Symbol.N_ArithmeticExpressionTail2
                // Lists
                Symbol.N_ListContents, Symbol.N_ListTail, Symbol.N_ListContentsTail,
                // Meta predicate with clause (e.g. assert, retract)
                Symbol.N_MetaPredicateWithClause
            });

            // **** Consider a grammar of this form: ****
            // Goal := Expression   // The semantic action here will ensure that the expression really is a goal.
            // Expression := ( Expression IfThenElseTail ) ExpressionTail
            // Expression := Integer ExpressionPartFollowingAnInteger
            // Expression := UpperCaseID ExpressionPartFollowingAnUpperCaseID   // The UpperCaseID will be a predicate or a variable.
            // Expression := LowerCaseID ExpressionPartFollowingALowerCaseID    // The LowerCaseID will be a predicate or a functor.
            // IfThenElseTail := Lambda
            // IfThenElseTail := -> Goal : Goal
            // IfThenElseTail := , Expression ExpressionListTail    // A sequence.
            // ExpressionListTail := Lambda
            // ExpressionListTail := , Expression ExpressionListTail
            // ExpressionTail := Lambda
            // ExpressionTail := ComparisonTail
            // ExpressionTail := ArithmeticOperator ArithmeticExpression ComparisonTail
            // ExpressionTail := OpType_EqualOrUnifiable Expression
            // ComparisonTail := Lambda
            // ComparisonTail := ComparisonOperator Expression  // The addition of this transforms an arithmetic expression into a goal.

            // Expression := [ ListContents ] ListTail
            // ListContents := Lambda
            // ListContents := Expression ListContentsTail
            // ListContentsTail := Lambda
            // ListContentsTail := , Expression ListContentsTail
            // ListContentsTail := | Expression
            // ListTail := Lambda
            // ListTail := OpType_EqualOrUnifiable Expression

            // ExpressionPartFollowingAnInteger := ComparisonTail   // This can derive Lambda.
            // ExpressionPartFollowingAnInteger := ArithmeticOperator ArithmeticExpression ComparisonTail
            // ExpressionPartFollowingAnInteger := is ArithmeticExpression
            // ExpressionPartFollowingAnInteger := OpType_EqualOrUnifiable Expression

            // ExpressionPartFollowingAnUpperCaseID := ComparisonTail   // This can derive Lambda, so the upper case ID could still be a predicate or a variable.
            // ExpressionPartFollowingAnUpperCaseID := ArithmeticOperator ArithmeticExpression ComparisonTail
            // ExpressionPartFollowingAnUpperCaseID := is ArithmeticExpression
            // ExpressionPartFollowingAnUpperCaseID := OpType_EqualOrUnifiable Expression
            // ExpressionPartFollowingAnUpperCaseID := ( Expression ExpressionListTail )    // The upper case ID is a predicate with parameters.

            // ExpressionPartFollowingALowerCaseID := ( Expression ExpressionListTail ) ExpressionPartFollowingALowerCaseIDWithParams
            // ExpressionPartFollowingALowerCaseID := ExpressionPartFollowingALowerCaseIDWithParams
            // ExpressionPartFollowingALowerCaseIDWithParams := Lambda
            // ExpressionPartFollowingALowerCaseIDWithParams := OpType_EqualOrUnifiable Expression

            // **** TODO: 2014/03/18 ****
            // 1) Define N_ArithmeticOperator and N_OpType_EqualOrUnifiable
            // 2) Write productions for N_ArithmeticExpression, implementing precedence of * / vs. + - () mod()

            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_Dot, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Clause }, 2));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Query }, 3));

            // TODO: The LHS of a Clause should be a Goal in the form of a predicate (N_GoalBeginningWithPredicateOrVariable)
            // or a goal that looks like a functor (N_FunctorExpression).
#if DEAD_CODE
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, Symbol.N_ClauseTail }, 0));
#else
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_LHSGoal, Symbol.N_ClauseTail }, 0));
            Productions.Add(new Production(Symbol.N_LHSGoal, new List<object>() { Symbol.T_NameBeginningWithCapital, Symbol.N_LHSGoalTail }, 0));
            Productions.Add(new Production(Symbol.N_LHSGoal, new List<object>() { Symbol.T_NameNotBeginningWithCapital, Symbol.N_LHSGoalTail }, 0));
            Productions.Add(new Production(Symbol.N_LHSGoalTail, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_LHSGoalTail, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionListTail, Symbol.T_RightBracket }, 0));
#endif

            Productions.Add(new Production(Symbol.N_ClauseTail, new List<object>() { Symbol.T_From, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_ClauseTail, new List<object>() { Symbol.Lambda, "#clauseWithoutSubgoals" }, 5));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.T_Comma, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#goalList" }, 6));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#emptyGoalList" }, 7));
            Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_InferPred, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#goalList" }, 8));
            Productions.Add(new Production(Symbol.N_GoalWithPossibleDisjunctiveTail, new List<object>() { Symbol.N_Goal, Symbol.N_PossibleDisjunctiveTail }, 0));
            Productions.Add(new Production(Symbol.N_PossibleDisjunctiveTail, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_PossibleDisjunctiveTail, new List<object>() { Symbol.T_Semicolon, Symbol.N_GoalWithPossibleDisjunctiveTail }, 0));

            // ThAW 2014/03/18 : This is where the fun begins.
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Expression, "#convertExpressionToGoal" }, 0));

            Productions.Add(new Production(Symbol.N_Expression , new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_IfThenElseTail, Symbol.T_RightBracket, Symbol.N_ExpressionTail }, 0));
            Productions.Add(new Production(Symbol.N_Expression , new List<object>() { Symbol.T_IntegerLiteral, Symbol.N_ExpressionPartFollowingAnInteger }, 0));
            Productions.Add(new Production(Symbol.N_Expression , new List<object>() { Symbol.T_NameBeginningWithCapital, Symbol.N_ExpressionPartFollowingAnUpperCaseID }, 0));
            Productions.Add(new Production(Symbol.N_Expression , new List<object>() { Symbol.T_NameNotBeginningWithCapital, Symbol.N_ExpressionPartFollowingALowerCaseID }, 0));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() {
                Symbol.T_Mod, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket, "#mod",
                Symbol.N_ArithmeticAndComparisonTail }, 0));
            Productions.Add(new Production(Symbol.N_IfThenElseTail, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_IfThenElseTail , new List<object>() { Symbol.T_IfThen, Symbol.N_Goal, Symbol.T_Colon, Symbol.N_Goal }, 0));
            Productions.Add(new Production(Symbol.N_IfThenElseTail , new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExpressionListTail }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionListTail , new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionListTail , new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExpressionListTail }, 0));
            //Productions.Add(new Production(Symbol.N_ExpressionTail , new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionTail , new List<object>() { Symbol.N_ArithmeticAndComparisonTail }, 0));
            //Productions.Add(new Production(Symbol.N_ExpressionTail, new List<object>() { Symbol.N_ArithmeticExpression2Foo, Symbol.N_ArithmeticExpression1Foo, Symbol.N_ComparisonTail }, 0)); // N_ArithmeticExpression2Foo and N_ArithmeticExpression1Foo can both derive Lambda.
            Productions.Add(new Production(Symbol.N_ExpressionTail , new List<object>() { Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression }, 0));

            Productions.Add(new Production(Symbol.N_ComparisonTail, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ComparisonTail, new List<object>() { Symbol.N_ComparisonOperator, Symbol.N_ArithmeticExpression1 }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticAndComparisonTail, new List<object>() { Symbol.N_ComparisonTail }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticAndComparisonTail, new List<object>() {
                Symbol.N_OpType_Add, Symbol.N_ArithmeticExpression2, "#arithExpr", Symbol.N_ArithmeticExpression1Foo, Symbol.N_ComparisonTail }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticAndComparisonTail, new List<object>() {
                Symbol.N_OpType_Multiply, Symbol.N_ArithmeticExpression3, "#arithExpr", Symbol.N_ArithmeticExpression2Foo, Symbol.N_ArithmeticExpression1Foo,
                Symbol.N_ComparisonTail }, 0));

            // Lists.
            Productions.Add(new Production(Symbol.N_Expression , new List<object>() { Symbol.T_LeftSquareBracket, Symbol.N_ListContents, Symbol.T_RightSquareBracket, Symbol.N_ListTail }, 0));
            Productions.Add(new Production(Symbol.N_ListContents , new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ListContents , new List<object>() { Symbol.N_Expression, Symbol.N_ListContentsTail }, 0));
            Productions.Add(new Production(Symbol.N_ListContentsTail , new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ListContentsTail , new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ListContentsTail }, 0));
            Productions.Add(new Production(Symbol.N_ListContentsTail , new List<object>() { Symbol.T_OrBar, Symbol.N_Expression }, 0));
            Productions.Add(new Production(Symbol.N_ListTail , new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ListTail , new List<object>() { Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression }, 0));

            //Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnInteger , new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnInteger, new List<object>() { Symbol.N_ArithmeticAndComparisonTail }, 0));
            //Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnInteger, new List<object>() { Symbol.N_ArithmeticExpression2Foo, Symbol.N_ArithmeticExpression1Foo, Symbol.N_ComparisonTail }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnInteger , new List<object>() { Symbol.T_Is, Symbol.N_ArithmeticExpression1 }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnInteger , new List<object>() { Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression }, 0));
#if !DEAD_CODE
            // Question: Could we save some productions with N_ExpressionPartFollowingAnUpperCaseID := N_ExpressionPartFollowingAnInteger ?
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnUpperCaseID, new List<object>() { Symbol.N_ExpressionPartFollowingAnInteger }, 0));
#else
            //Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnUpperCaseID, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnUpperCaseID, new List<object>() { Symbol.N_ArithmeticAndComparisonTail }, 0));
            //Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnUpperCaseID, new List<object>() { Symbol.N_ArithmeticExpression2Foo, Symbol.N_ArithmeticExpression1Foo, Symbol.N_ComparisonTail }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnUpperCaseID , new List<object>() { Symbol.T_Is, Symbol.N_ArithmeticExpression1 }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnUpperCaseID , new List<object>() { Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression }, 0));
#endif
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingAnUpperCaseID , new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionListTail, Symbol.T_RightBracket }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingALowerCaseID, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionListTail, Symbol.T_RightBracket, Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingALowerCaseID, new List<object>() { Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams, new List<object>() { Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression }, 0));

            // Handle + and -
            // Remember that for LL(1) grammars, semantic actions don't have to be placed at the end of the production (unlike with *LR(1) grammars).
            // See Fischer & LeBlanc page 239 for the Micro grammar with semantic action symbols.
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1, new List<object>() {
                Symbol.N_ArithmeticExpression2, Symbol.N_ArithmeticExpression1Foo }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1Foo, new List<object>() {
                Symbol.N_OpType_Add, Symbol.N_ArithmeticExpression2, "#arithExpr", Symbol.N_ArithmeticExpression1Foo }, 25));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1Foo, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_OpType_Add, new List<object>() { Symbol.T_Plus }, 0));
            Productions.Add(new Production(Symbol.N_OpType_Add, new List<object>() { Symbol.T_Minus }, 0));
            // Handle * and /
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2, new List<object>() {
                Symbol.N_ArithmeticExpression3, Symbol.N_ArithmeticExpression2Foo }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2Foo, new List<object>() {
                Symbol.N_OpType_Multiply, Symbol.N_ArithmeticExpression3, "#arithExpr", Symbol.N_ArithmeticExpression2Foo }, 25));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2Foo, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_OpType_Multiply, new List<object>() { Symbol.T_Multiply }, 0));
            Productions.Add(new Production(Symbol.N_OpType_Multiply, new List<object>() { Symbol.T_Divide }, 0));
            // Expressions represented by N_ArithmeticExpression3 do not rely on order of operations to be evaluated
            // (except for the arguments to mod).
            Productions.Add(new Production(Symbol.N_ArithmeticExpression3, new List<object>() {
                Symbol.T_Mod, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket, "#mod" }, 61));
            //Productions.Add(new Production(Symbol.N_ArithmeticExpression3, new List<object>() { Symbol.N_NumberOrVariableExpression }, 27));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression3, new List<object>() { Symbol.T_IntegerLiteral }, 27));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression3, new List<object>() { Symbol.T_NameBeginningWithCapital, "#variable" }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression3, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket }, 0));

            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_LessThan }, 31));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_GreaterThan }, 32));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_LessEqual }, 33));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_GreaterEqual }, 34));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_ArithmeticEquals }, 62));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_ArithmeticNotEquals }, 63));

            Productions.Add(new Production(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_Assign }, 0));       // "#="
            Productions.Add(new Production(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_Equals }, 0));       // "#=="
            Productions.Add(new Production(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_NotEqual }, 0));     // "#notEqual"
            Productions.Add(new Production(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_NotUnifiable }, 0)); // "#notUnifiable"

            // String literals.
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_StringLiteral }, 35));

            // Not keyword.
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() {
                Symbol.T_Not, Symbol.T_LeftBracket, Symbol.N_Goal, Symbol.T_RightBracket, "#not" }, 36));   // #metaPredicateWithGoal

            // Not symbol: \+
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() {
                Symbol.T_NotSymbol, Symbol.N_Goal, "#not" }, 0));   // #metaPredicateWithGoal

            // Assert and retract.
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() {
                Symbol.N_MetaPredicateWithClause, Symbol.T_LeftBracket, Symbol.N_Clause, Symbol.T_RightBracket, "#metaPredicateWithClause" }, 37));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_Assert }, 38));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_AssertA }, 39));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_AssertZ }, 40));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_Retract }, 41));
            Productions.Add(new Production(Symbol.N_MetaPredicateWithClause, new List<object>() { Symbol.T_RetractAll }, 42));

#if DEAD_CODE
            Productions.Add(new Production(Symbol. , new List<object>() { Symbol }, 0));
            Productions.Add(new Production(Symbol. , new List<object>() { Symbol }, 0));
            Productions.Add(new Production(Symbol. , new List<object>() { Symbol }, 0));
            Productions.Add(new Production(Symbol. , new List<object>() { Symbol }, 0));
            Productions.Add(new Production(Symbol. , new List<object>() { Symbol }, 0));
            //Productions.Add(new Production(Symbol. , new List<object>() { Symbol }, 0));
#endif
#else
            NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
                Symbol.N_Input, Symbol.N_Clause, Symbol.N_Query, Symbol.N_Goal,
                Symbol.N_GoalList, Symbol.N_Predicate, Symbol.N_Expression, Symbol.N_ExpressionList,
                Symbol.N_Functor, Symbol.N_Variable, Symbol.N_Name, Symbol.N_NumberOrVariableExpression });

            NonTerminals.UnionWith(new HashSet<Symbol>() {
                Symbol.N_ClauseTail, Symbol.N_GoalBeginningWithPredicateOrVariable, Symbol.N_FunctorExpression, Symbol.N_FunctorExpressionTail,
                Symbol.N_FunctorGoalTail, Symbol.N_ExpressionNotFunctorOrVariableOrInteger, Symbol.N_GoalBeginningWithInteger,
                Symbol.N_OpType_EqualOrUnifiable,
                // Arithmetic
                Symbol.N_ArithmeticExpression1, Symbol.N_ArithmeticExpression2, Symbol.N_ArithmeticExpression3,
                Symbol.N_ArithmeticExpression1Foo, Symbol.N_ArithmeticExpression2Foo, Symbol.N_OpType_Add, Symbol.N_OpType_Multiply,
                // Arithmetic comparisons
                Symbol.N_ComparisonOperator, Symbol.N_GoalBeginningWithArithmeticExpression, Symbol.N_ArithmeticExpressionTail1, Symbol.N_ArithmeticExpressionTail2,
                // Lists
                Symbol.N_ListContents, Symbol.N_ListTail,
                // Sequences
                Symbol.N_Sequence,
                // Meta predicate with clause (e.g. assert, retract)
                Symbol.N_MetaPredicateWithClause,
                // Goal disjunction
#if SLR1_PROLOG
                Symbol.N_GoalDisjunctionTail
#else
                Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_PossibleDisjunctiveTail
#endif
            });

            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_Dot, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Clause }, 2));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Query }, 3));

            // TODO: The LHS of a Clause should be a Goal in the form of a predicate (N_GoalBeginningWithPredicateOrVariable)
            // or a goal that looks like a functor (N_FunctorExpression).
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, Symbol.N_ClauseTail }, 0));

#if SLR1_PROLOG
            Productions.Add(new Production(Symbol.N_ClauseTail, new List<object>() { Symbol.T_From, Symbol.N_Goal, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_ClauseTail, new List<object>() { Symbol.Lambda, "#clauseWithoutSubgoals" }, 5));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.T_Comma, Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 6));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#emptyGoalList" }, 7));
            Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_InferPred, Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 8));
#else
            Productions.Add(new Production(Symbol.N_ClauseTail, new List<object>() { Symbol.T_From, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_ClauseTail, new List<object>() { Symbol.Lambda, "#clauseWithoutSubgoals" }, 5));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.T_Comma, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#goalList" }, 6));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#emptyGoalList" }, 7));
            Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_InferPred, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#goalList" }, 8));
            Productions.Add(new Production(Symbol.N_GoalWithPossibleDisjunctiveTail, new List<object>() { Symbol.N_Goal, Symbol.N_PossibleDisjunctiveTail }, 0));
            Productions.Add(new Production(Symbol.N_PossibleDisjunctiveTail, new List<object>() { Symbol.Lambda }, 0));
            // See below for: Productions.Add(new Production(Symbol.N_PossibleDisjunctiveTail, new List<object>() { Symbol.T_Semicolon, Symbol.N_GoalWithPossibleDisjunctiveTail }, 0));
#endif
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Variable, Symbol.N_GoalBeginningWithPredicateOrVariable }, 0)); // This "variable" is actually a predicate.
            Productions.Add(new Production(Symbol.N_GoalBeginningWithPredicateOrVariable, new List<object>() { Symbol.Lambda, "#goalNoArgs" }, 9));
            Productions.Add(new Production(Symbol.N_GoalBeginningWithPredicateOrVariable, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#goal2" }, 10));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExpressionList, "#exprList" }, 11));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExprList" }, 12));
            // TODO: Can we rename N_NumberOrVariableExpression to N_NonFunctorExpression (because of lists, sequences, strings, etc.) ?
            // Or perhaps rename it to N_NonFunctorOrVariableExpression (i.e. any expression except variables or functor expressions) ?
            // Or call it N_ExpressionNotFunctorOrVariable
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_ExpressionNotFunctorOrVariableOrInteger }, 22));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_IntegerLiteral }, 13));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Variable }, 14));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_FunctorExpression }, 0));
            Productions.Add(new Production(Symbol.N_FunctorExpression, new List<object>() { Symbol.N_Functor, Symbol.N_FunctorExpressionTail }, 0));
            Productions.Add(new Production(Symbol.N_FunctorExpressionTail, new List<object>() { Symbol.Lambda, "#functorExpressionNoArgs" }, 15));
            Productions.Add(new Production(Symbol.N_FunctorExpressionTail, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression2" }, 16));
            //Productions.Add(new Production(Symbol.N_Predicate, new List<object>() { Symbol.N_Name, "#predicate" }, 17));
            Productions.Add(new Production(Symbol.N_Functor, new List<object>() { Symbol.T_NameNotBeginningWithCapital, "#functor" }, 18));
            Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_NameBeginningWithCapital, "#variable" }, 19));
            //Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameNotBeginningWithCapital }, 20));
            //Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameBeginningWithCapital }, 21));
            // Production 22 is between 12 and 13.

            Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_IntegerLiteral }, 0));
            Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.N_Variable }, 0));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.T_IntegerLiteral, Symbol.N_GoalBeginningWithInteger }, 0));

            // Arithmetic via the "is" operator
#if SLR1_PROLOG
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
#else
            // TODO: Rename N_ArithmeticExpressionTail1 (defined below) to N_GoalBeginningWithPredicateOrVariableOrInteger.
            Productions.Add(new Production(Symbol.N_ArithmeticExpressionTail1, new List<object>() {
                Symbol.T_Is, Symbol.N_ArithmeticExpression1, "#is" }, 23));
            // Handle + and -
            // Remember that for LL(1) grammars, semantic actions don't have to be placed at the end of the production (unlike with *LR(1) grammars).
            // See Fischer & LeBlanc page 239 for the Micro grammar with semantic action symbols.
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1, new List<object>() {
                Symbol.N_ArithmeticExpression2, Symbol.N_ArithmeticExpression1Foo }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1Foo, new List<object>() {
                Symbol.N_OpType_Add, Symbol.N_ArithmeticExpression2, "#arithExpr", Symbol.N_ArithmeticExpression1Foo }, 25));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression1Foo, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_OpType_Add, new List<object>() { Symbol.T_Plus }, 0));
            Productions.Add(new Production(Symbol.N_OpType_Add, new List<object>() { Symbol.T_Minus }, 0));
            // Handle * and /
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2, new List<object>() {
                Symbol.N_ArithmeticExpression3, Symbol.N_ArithmeticExpression2Foo }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2Foo, new List<object>() {
                Symbol.N_OpType_Multiply, Symbol.N_ArithmeticExpression3, "#arithExpr", Symbol.N_ArithmeticExpression2Foo }, 25));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression2Foo, new List<object>() { Symbol.Lambda }, 0));
            Productions.Add(new Production(Symbol.N_OpType_Multiply, new List<object>() { Symbol.T_Multiply }, 0));
            Productions.Add(new Production(Symbol.N_OpType_Multiply, new List<object>() { Symbol.T_Divide }, 0));
            // Expressions represented by N_ArithmeticExpression3 do not rely on order of operations to be evaluated
            // (except for the arguments to mod).
            Productions.Add(new Production(Symbol.N_ArithmeticExpression3, new List<object>() {
                Symbol.T_Mod, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket, "#mod" }, 61));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression3, new List<object>() { Symbol.N_NumberOrVariableExpression }, 27));
            Productions.Add(new Production(Symbol.N_ArithmeticExpression3, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket }, 0));
#endif

            // Arithmetic comparisons: Beginning
#if SLR1_PROLOG
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.N_ArithmeticExpression1, Symbol.N_ComparisonOperator, Symbol.N_ArithmeticExpression1, "#comparisonOperator" }, 30));
#else
            Productions.Add(new Production(Symbol.N_GoalBeginningWithInteger, new List<object>() { Symbol.N_ArithmeticExpressionTail1 }, 0));
            Productions.Add(new Production(Symbol.N_GoalBeginningWithPredicateOrVariable, new List<object>() { Symbol.N_ArithmeticExpressionTail1 }, 0));

            Productions.Add(new Production(Symbol.N_ArithmeticExpressionTail1, new List<object>() {
                Symbol.N_ComparisonOperator, Symbol.N_ArithmeticExpression1, "#comparisonOperator" }, 30));
            Productions.Add(new Production(Symbol.N_ArithmeticExpressionTail1, new List<object>() {
                Symbol.N_OpType_Add, Symbol.N_ArithmeticExpression2, "#arithExpr", Symbol.N_ArithmeticExpression1Foo,
                Symbol.N_ComparisonOperator, Symbol.N_ArithmeticExpression1, "#comparisonOperator" }, 0));
            Productions.Add(new Production(Symbol.N_ArithmeticExpressionTail1, new List<object>() {
                Symbol.N_OpType_Multiply, Symbol.N_ArithmeticExpression3, "#arithExpr", Symbol.N_ArithmeticExpression2Foo,
                Symbol.N_ComparisonOperator, Symbol.N_ArithmeticExpression1, "#comparisonOperator" }, 0));

            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.T_Mod, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket,
                Symbol.N_ArithmeticExpressionTail1 }, 0));
#endif
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_LessThan }, 31));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_GreaterThan }, 32));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_LessEqual }, 33));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_GreaterEqual }, 34));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_ArithmeticEquals }, 62));
            Productions.Add(new Production(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_ArithmeticNotEquals }, 63));

            // 2014/03/15 : We have a bracket problem.  Brackets are used to delimit:
            // 1) Predicate and functor argument lists: e.g. member(2, [1, 2, 3])
            // 2) Arithmetic expressions (especially on the LHS of a comparison) : e.g. (1 + 2) * 3
            //    Goal := ( GoalBeginningWithABracket
            //    GoalBeginningWithABracket := SomeSortOfArithmeticExpression ) RestOfComparisonGoal
            // 3) Sequences: e.g. (1) or (1, 2, 3)
            //    Expression := ( Expression SequenceTail )
            //    SequenceTail := Lambda
            //    SequenceTail := , Expression SequenceTail
            // 4) If-then-else constructs: e.g. (bar(X) -> baz(X) : bat(X)) : i.e. Goal := ( Goal -> Goal : Goal )
            //    GoalBeginningWithABracket := Goal -> Goal : Goal )
            // How do we parse all of these with an LL(1) grammar?

            // SomethingBeginningWithABracket // Could be a Goal, an ArithmeticExpression, or a Sequence
            // SomethingNotBeginningWithABracket // Could be a PredicateOrVariable (upper case or _), a Functor (lower case), an Integer...
            // GoalBeginningWithABracket
            // GoalNotBeginningWithABracket
            // ArithmeticExpressionBeginningWithABracket
            // SequenceBeginningWithABracket
            // Goal := ( GoalBeginningWithABracket
            // GoalBeginningWithABracket := Goal -> Goal : Goal ) // If-then-else
            // GoalBeginningWithABracket := ArithmeticExpressionBeginningWithABracket_WithoutLeftBracket ComparisonOperator ArithmeticExpression
            // GoalBeginningWithABracket := SequenceWithoutLeftBracket OpType_UnifyOrEquals Expression

            // GoalBeginningWithABracket := ( SomethingBeginningWithABracket_WithExtraRightBracket MoreStuff_WithExtraRightBracket
            // TODO: Figure out the recursion involved here.

            //// GoalBeginningWithABracket := SomethingNotBeginningWithABracket MoreStuff
            // GoalBeginningWithABracket := PredicateOrVariable SomethingBeginningWithAPredicateOrVariable_WithExtraRightBracket MoreStuff
            // GoalBeginningWithABracket := Functor SomethingBeginningWithAFunctor_WithExtraRightBracket MoreStuff
            // GoalBeginningWithABracket := Integer SomethingBeginningWithAnInteger_WithExtraRightBracket MoreStuff

            // SomethingBeginningWithAPredicateOrVariable_WithExtraRightBracket := , Expression ExpressionTail ) // This is a sequence; the PredicateOrVariable is a variable.
            // SomethingBeginningWithAPredicateOrVariable_WithExtraRightBracket := ) // This is a single-element sequence; the PredicateOrVariable is a variable.
            // no: SomethingBeginningWithAPredicateOrVariable := ArithmeticOperator ArithmeticExpression ) // This is an arithmetic expression; the PredicateOrVariable is a variable.
            // instead: SomethingBeginningWithAPredicateOrVariable_WithExtraRightBracket := ArithmeticOperator ArithmeticExpression ) ArithmeticExpressionTail ComparisonOperator ArithmeticExpression // This is an arithmetic comparison; the PredicateOrVariable is a variable.
            // SomethingBeginningWithAPredicateOrVariable_WithExtraRightBracket := is ArithmeticExpression -> Goal : Goal ) // This is an arithmetic expression; the PredicateOrVariable is a variable.
            // SomethingBeginningWithAPredicateOrVariable_WithExtraRightBracket := -> Goal : Goal ) // The PredicateOrVariable is a predicate.
            // SomethingBeginningWithAPredicateOrVariable_WithExtraRightBracket := ( ExpressionList ) -> Goal : Goal ) // The PredicateOrVariable is a predicate.

            // SomethingBeginningWithAnInteger_WithExtraRightBracket := , Expression ExpressionTail ) // This is a sequence; the PredicateOrVariable is a variable.
            // SomethingBeginningWithAnInteger_WithExtraRightBracket := ) // This is a single-element sequence; the PredicateOrVariable is a variable.
            // SomethingBeginningWithAnInteger_WithExtraRightBracket := ArithmeticOperator ArithmeticExpression ) ArithmeticExpressionTail ComparisonOperator ArithmeticExpression // This is an arithmetic comparison; the PredicateOrVariable is a variable.
            // SomethingBeginningWithAnInteger_WithExtraRightBracket := is ArithmeticExpression -> Goal : Goal ) // This is an arithmetic expression; the PredicateOrVariable is a variable.

            // SomethingBeginningWithAFunctor_WithExtraRightBracket := , Expression ExpressionTail ) // This is a sequence.
            // SomethingBeginningWithAFunctor_WithExtraRightBracket := ) // This is a single-element sequence.
            // SomethingBeginningWithAFunctor_WithExtraRightBracket := -> Goal : Goal ) // The functor is really a predicate.
            // SomethingBeginningWithAFunctor_WithExtraRightBracket := OpType_UnifyOrEquals Expression -> Goal : Goal )
            // SomethingBeginningWithAFunctor_WithExtraRightBracket := ( ExpressionList ) SomethingBeginningWithAFunctorAndExpressionList_WithExtraRightBracket
            // SomethingBeginningWithAFunctorAndExpressionList_WithExtraRightBracket := -> Goal : Goal ) // The functor is really a predicate.
            // SomethingBeginningWithAFunctorAndExpressionList_WithExtraRightBracket := OpType_UnifyOrEquals Expression -> Goal : Goal )

            // Arithmetic comparisons: End

            Productions.Add(new Production(Symbol.N_ExpressionNotFunctorOrVariableOrInteger, new List<object>() { Symbol.T_StringLiteral }, 35));

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
            Productions.Add(new Production(Symbol.N_ExpressionNotFunctorOrVariableOrInteger, new List<object>() {
                Symbol.T_LeftSquareBracket, Symbol.N_ListContents, Symbol.T_RightSquareBracket }, 43));
            Productions.Add(new Production(Symbol.N_ListContents, new List<object>() { Symbol.Lambda, "#nil" }, 44));
            Productions.Add(new Production(Symbol.N_ListContents, new List<object>() { Symbol.N_Expression, Symbol.N_ListTail, "#cons" }, 45));
            Productions.Add(new Production(Symbol.N_ListTail, new List<object>() { Symbol.T_OrBar, Symbol.N_Expression }, 46));
            Productions.Add(new Production(Symbol.N_ListTail, new List<object>() { Symbol.Lambda, "#nil" }, 47));
            Productions.Add(new Production(Symbol.N_ListTail, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ListTail, "#cons" }, 48));

            Productions.Add(new Production(Symbol.N_ExpressionNotFunctorOrVariableOrInteger, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_Sequence, Symbol.T_RightBracket, "#consSeq" }, 49));
            Productions.Add(new Production(Symbol.N_Sequence, new List<object>() { Symbol.Lambda, "#nil" }, 50)); // Remember that (a) = a
            Productions.Add(new Production(Symbol.N_Sequence, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_Sequence, "#consSeq" }, 51));

            // The "not" symbol: \+
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.T_NotSymbol, Symbol.N_Goal, "#not" }, 52));

            // Goal disjunction: See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_9.html
#if SLR1_PROLOG
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.N_Goal, Symbol.T_Semicolon, Symbol.N_Goal, Symbol.N_GoalDisjunctionTail, "#goalDisjunction2" }, 53));
            Productions.Add(new Production(Symbol.N_GoalDisjunctionTail, new List<object>() { Symbol.Lambda, "#null" }, 54));
            Productions.Add(new Production(Symbol.N_GoalDisjunctionTail, new List<object>() {
                Symbol.T_Semicolon, Symbol.N_Goal, Symbol.N_GoalDisjunctionTail, "#goalDisjunction" }, 55));
#else
            Productions.Add(new Production(Symbol.N_PossibleDisjunctiveTail, new List<object>() {
                Symbol.T_Semicolon, Symbol.N_GoalWithPossibleDisjunctiveTail, "#goalDisjunction" }, 0));
#endif

#if DEAD_CODE // SLR1_PROLOG
            // If-then-else
            // See above for a description of our bracket problem.

            // This production prevents the grammar from being SLR(1);
            // a reduce-reduce conflict arises between N_Variable := T_NameBeginningWithCapital and N_Name := T_NameBeginningWithCapital.

            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Goal, Symbol.T_IfThen, Symbol.N_Goal, Symbol.T_Colon, Symbol.N_Goal, Symbol.T_RightBracket, "#ifThenElse" }, 56));
#endif

            // The infix unification operator: =
            Productions.Add(new Production(Symbol.N_GoalBeginningWithInteger, new List<object>() {
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression, "#=" }, 0));
            Productions.Add(new Production(Symbol.N_GoalBeginningWithPredicateOrVariable, new List<object>() {
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression, "#=varlhs" }, 0)); // The LHS of the = must be interpreted as a Variable, not a Goal.
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() {
                Symbol.N_ExpressionNotFunctorOrVariableOrInteger, Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression, "#=" }, 0));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_FunctorExpression, Symbol.N_FunctorGoalTail }, 0));
            Productions.Add(new Production(Symbol.N_FunctorGoalTail, new List<object>() { Symbol.Lambda, "#functortogoal" }, 0));
            Productions.Add(new Production(Symbol.N_FunctorGoalTail, new List<object>() {
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression, "#=funclhs" }, 0));
            Productions.Add(new Production(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_Assign }, 0));       // "#="
            Productions.Add(new Production(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_Equals }, 0));       // "#=="
            Productions.Add(new Production(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_NotEqual }, 0));     // "#notEqual"
            Productions.Add(new Production(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_NotUnifiable }, 0)); // "#notUnifiable"

            // Production 61 is between 29 and 30.
            // Productions 62 and 63 are between 34 and 35.
#endif
        }
    }

#endregion
#endif

    [TestFixture]
    public class LL1Parser_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public LL1Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Inference);
        }

        [Test]
        public void RecognizeManIsMortalTest()
        {
            parser.Recognize(tokenizer.Tokenize("@isMan(?x) -> @isMortal(?x)"));
        }

        [Test]
        public void ParseManIsMortalTest()
        {
            string strInput = "!@isMan(?x) || @isMortal(?x)";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            //Assert.AreEqual(expected, actual);
            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseSocratesIsAManTest()
        {
            string strInput = "@isMan(Socrates)";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseFatherFunctionTest()
        {
            string strInput = "@isFatherOf(father(?x), ?x)";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseSkolemFunctionTest1()
        {
            string strInput = "@isFoo($S1())";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseSkolemFunctionTest2()
        {
            string strInput = "@isFatherOf($father(?x), ?x)";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseIntLitTest()
        {
            string strInput = "@isIntLit(123)";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseStrLitTest()
        {
            string strInput = "@isStrLit(\"abc\")";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseTransitivityTest1()
        {
            string strInput = "!@equals(?a, ?b) || !@equals(?b, ?c) || @equals(?a, ?c)";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseTransitivityTest2()
        {
            string strInput = "(@equals(?a, ?b) && @equals(?b, ?c)) -> @equals(?a, ?c)";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();
            string strExpectedOutput = "!(@equals(?a, ?b) && @equals(?b, ?c)) || @equals(?a, ?c)";

            Assert.AreEqual(strExpectedOutput, strOutput);
        }

        [Test]
        public void ParseTransitivityTest3()
        {
            string strInput = "(@equals(?a, ?b) && @equals(?b, ?c)) -> @equals(?a, ?c)";
            IBooleanExpression boolExpr = parser.Parse(tokenizer.Tokenize(strInput)) as IBooleanExpression;

            Assert.IsNotNull(boolExpr);

            List<Clause> listOfClauses = Clause.ConvertBooleanExpressionToClausalForm(boolExpr);

            Assert.AreEqual(1, listOfClauses.Count);

            string strOutput = listOfClauses[0].ToString();
            string strExpectedOutput = "!@equals(?a, ?b) || !@equals(?b, ?c) || @equals(?a, ?c)";

            Assert.AreEqual(strExpectedOutput, strOutput);
        }

        [Test]
        public void ParseJunctionTest1()
        {
            string strInput = "@a() || (@b() && @c())";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseJunctionTest2()
        {
            string strInput = "@a() && (@b() || @c())";
            object output = parser.Parse(tokenizer.Tokenize(strInput));
            string strOutput = output.ToString();

            Assert.AreEqual(strInput, strOutput);
        }

        [Test]
        public void ParseErrorTest1()
        {
            Assert.Throws<SyntaxException>(() => parser.Parse(tokenizer.Tokenize("@f(?x")));
        }

        [Test]
        public void ParseErrorTest2()
        {
            Assert.Throws<SyntaxException>(() => parser.Parse(tokenizer.Tokenize("@f(?x))")));
        }

        [Test]
        public void ParseErrorTest3()
        {
            Assert.Throws<SyntaxException>(() => parser.Parse(tokenizer.Tokenize("@f(?x) @g(?y)")));
        }

#if PROLOG_TEST_GRAMMAR
        [Test]
        public void PrologGrammarTest() // 2014/03/14
        {
            var t = TokenizerFactory.Create(GrammarSelector.Prolog2);
#if SLR1_PROLOG
            // We can use the SLR(1) parser to parse the LL(1) grammar if all semantic actions are at the ends of their productions.
            var p = new SLR1Parser(new LL1PrologTestGrammar());
#elif DEAD_CODE
            var p = new LL1Parser(new LL1PrologTestGrammar());
#else
            var p = new LL1Parser(new Inference.Interpreter.Prolog.PrologGrammar2_LL1());
#endif

            p.Recognize(t.Tokenize("foo."));
            p.Recognize(t.Tokenize("?- foo."));
            p.Recognize(t.Tokenize("member(X, cons(X, _))."));
            p.Recognize(t.Tokenize("member(X, cons(_, T)) :- member(X, T)."));
            p.Recognize(t.Tokenize("?- member(2, cons(1, cons(2, cons(3, nil))))."));
            p.Recognize(t.Tokenize("?- member(X, cons(1, cons(2, cons(3, nil)))), print(X)."));

            p.Recognize(t.Tokenize("Foo."));
            p.Recognize(t.Tokenize("?- Foo."));
            p.Recognize(t.Tokenize("Member(X, cons(X, _))."));
            p.Recognize(t.Tokenize("Member(X, cons(_, T)) :- Member(X, T)."));
            p.Recognize(t.Tokenize("?- Member(2, cons(1, cons(2, cons(3, nil))))."));
            p.Recognize(t.Tokenize("?- Member(X, cons(1, cons(2, cons(3, nil)))), print(X)."));

            // Test the infix unification operator (=)
            p.Recognize(t.Tokenize("?- 1 = X."));
            p.Recognize(t.Tokenize("?- X = 1."));
            p.Recognize(t.Tokenize("?- mia = X."));
            p.Recognize(t.Tokenize("?- X = mia."));
            p.Recognize(t.Tokenize("?- mia(X) = mia(t)."));
            p.Recognize(t.Tokenize("?- cons(X, Y) = cons(1, cons(2, cons(3, nil)))."));

            // Test of list notation
            p.Recognize(t.Tokenize("print([])."));
            p.Recognize(t.Tokenize("print([1])."));
            p.Recognize(t.Tokenize("print([1, 2, 3])."));
            p.Recognize(t.Tokenize("print([1, 2, 3 | [4, 5, 6]])."));
            p.Recognize(t.Tokenize("member(X, [X | _])."));
            p.Recognize(t.Tokenize("member(X, [_, T]) :- member(X, T)."));
            p.Recognize(t.Tokenize("?- [X | Y] = [1, 2, 3], print(X, Y)."));
            p.Recognize(t.Tokenize("?- [1, 2, 3] = [X | Y], print(X, Y)."));

            // Test of sequence notation
            p.Recognize(t.Tokenize("foo((1))."));
            p.Recognize(t.Tokenize("foo((1, 2, 3))."));
            p.Recognize(t.Tokenize("?- (X, Y) = (1, 2, 3), print(X, Y)."));

            // Test of arithmetic via the "is" infix operator.
            p.Recognize(t.Tokenize("?- 2 is 1 + 1.")); // This causes a shift-reduce conflict when the comparison productions are enabled.
            p.Recognize(t.Tokenize("?- X is Y + 7."));
            p.Recognize(t.Tokenize("?- 2 is 3 - 1."));
            p.Recognize(t.Tokenize("?- X is 7 - Y."));
            p.Recognize(t.Tokenize("?- 12 is 3 * 4."));
            p.Recognize(t.Tokenize("?- X is Y * 2."));
            p.Recognize(t.Tokenize("?- 3 is 12 / 4."));
            p.Recognize(t.Tokenize("?- X is 256 / Y."));
            p.Recognize(t.Tokenize("?- 3 is mod(19, 4)."));
            p.Recognize(t.Tokenize("?- X is mod(Y, Z)."));
            p.Recognize(t.Tokenize("?- 3 is (2 + 3) * (4 - 1) / 5."));
            p.Recognize(t.Tokenize("?- W is (X + 3) * (4 - Y) / Z."));
            //p.Recognize(t.Tokenize("?-  is ."));

            // Test of infix arithmetic comparisons.
            p.Recognize(t.Tokenize("?- 1 < 2."));
            p.Recognize(t.Tokenize("?- X < 2."));
            p.Recognize(t.Tokenize("?- 1 < X."));
            p.Recognize(t.Tokenize("?- X < Y."));
            p.Recognize(t.Tokenize("?- 1 + 2 < 3 + 4."));
            p.Recognize(t.Tokenize("?- W + X < Y + Z."));
            p.Recognize(t.Tokenize("?- 1 * 2 < 3 * 4."));
            p.Recognize(t.Tokenize("?- W * X < Y * Z."));
            p.Recognize(t.Tokenize("?- mod(W, X) < mod(Y, Z)."));
            p.Recognize(t.Tokenize("?- mod(W, X) * 2 + 1 < mod(Y, Z) * 2 + 1.")); // LHS is a complex arithmetic expression that begins with mod/2.
            p.Recognize(t.Tokenize("?- (1 + 2) * 3 < (4 + 5) * 6.")); // The grammar must allow goals to start with a left bracket.
            p.Recognize(t.Tokenize("?- (((1 + 1) + 1) + 1) + 1 < 7."));
            p.Recognize(t.Tokenize("?- 1 + (2 + 3) * 4 + mod(55, 6) < 7 + (8 + 9) * 10 + mod(111, 12) + 13."));
            //p.Recognize(t.Tokenize("?-  < ."));

            // Test of goal disjunction.
            p.Recognize(t.Tokenize("connected(X, Y) :- edge(X, Y) ; edge(Y, X)."));
            p.Recognize(t.Tokenize("Connected(X, Y) :- Edge(X, Y) ; Edge(Y, X)."));

            // Test of if-then-else.
            p.Recognize(t.Tokenize("foo(X) :- (bar(X) -> baz(X) : bat(X))."));
            p.Recognize(t.Tokenize("Foo(X) :- (Bar(X) -> Baz(X) : Bat(X))."));

            // Test of not (predicate negation), both with the "not" keyword and the not symbol (\+).
            p.Recognize(t.Tokenize(@"not_foo(X) :- not(foo(X))."));
            p.Recognize(t.Tokenize(@"not_Foo(X) :- not(Foo(X))."));
            p.Recognize(t.Tokenize(@"not_foo(X) :- \+ foo(X)."));
            p.Recognize(t.Tokenize(@"not_Foo(X) :- \+ Foo(X)."));

            // Test of assert, retract, etc.
            p.Recognize(t.Tokenize("foo(X) :- assert(bar(X)), blah(X)."));
            p.Recognize(t.Tokenize("Foo(X) :- assert(Bar(X)), Blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- assert(bar(X) :- baz(X), bat(X)), blah(X)."));
            p.Recognize(t.Tokenize("Foo(X) :- assert(Bar(X) :- Baz(X), Bat(X)), Blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- asserta(bar(X)), blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- asserta(bar(X) :- baz(X), bat(X)), blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- assertz(bar(X)), blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- assertz(bar(X) :- baz(X), bat(X)), blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- retract(bar(X)), blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- retract(bar(X) :- baz(X), bat(X)), blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- stuff(X), retractall(bar(X)), blah(X)."));
            p.Recognize(t.Tokenize("foo(X) :- stuff(X), retractall(bar(X) :- baz(X), bat(X)), blah(X)."));

#if DEAD_CODE
            // TODO:
            p.Recognize(t.Tokenize("."));
#endif
        }
#endif
    }
}