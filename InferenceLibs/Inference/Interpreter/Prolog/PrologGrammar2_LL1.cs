﻿using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.Prolog
{
    public class PrologGrammar2_LL1 : GrammarBase
    {
        private readonly GrammarSelector gs = GrammarSelector.Prolog2;

        public PrologGrammar2_LL1()
            : base(Symbol.N_Start)
        {
            Terminals.UnionWith(new HashSet<Symbol>() { // From PrologGrammarBase
                Symbol.T_LeftBracket, Symbol.T_RightBracket, Symbol.T_From,
                Symbol.T_InferPred, Symbol.T_IntegerLiteral, Symbol.T_NameBeginningWithCapital, Symbol.T_NameNotBeginningWithCapital,
                Symbol.T_EOF,
                // From PrologGrammar2
                Symbol.T_Comma, Symbol.T_Dot, Symbol.T_Is, Symbol.T_Plus,
                Symbol.T_Minus, Symbol.T_Multiply, Symbol.T_Divide, Symbol.T_LessThan,
                Symbol.T_GreaterThan, Symbol.T_LessEqual, Symbol.T_GreaterEqual, Symbol.T_StringLiteral,
                Symbol.T_LeftSquareBracket, Symbol.T_RightSquareBracket, Symbol.T_OrBar,
                Symbol.T_NotSymbol, Symbol.T_IfThen, Symbol.T_Colon, Symbol.T_Assign,
                Symbol.T_Equals, Symbol.T_NotEqual, Symbol.T_NotUnifiable,
                Symbol.T_Semicolon, Symbol.T_Mod, Symbol.T_ArithmeticEquals, Symbol.T_ArithmeticNotEquals,
                Symbol.T_DCGArrow, Symbol.T_LeftCurlyBrace, Symbol.T_RightCurlyBrace, Symbol.T_Univ,
                Symbol.T_FloatLiteral, Symbol.T_Caret });

            NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
                Symbol.N_Input, Symbol.N_Clause, Symbol.N_Query, Symbol.N_Goal,
                Symbol.N_ClauseTail, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_PossibleDisjunctiveTail,
                Symbol.N_GoalList, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.N_NumberOrVariableExpression,
                Symbol.N_Functor, //Symbol.N_Variable, Symbol.N_Name, Symbol.N_Predicate, Symbol.N_ExpressionListTail, Symbol.N_ExpressionTail, 
                Symbol.N_IfThenElseTail, Symbol.N_InfixNonArithmeticPredicateTail, Symbol.N_InfixPredicateTail,
                Symbol.N_ListContentsTail, Symbol.N_ExpressionPartFollowingAnInteger, Symbol.N_ExpressionPartFollowingAnUpperCaseID,
                Symbol.N_ExpressionPartFollowingALowerCaseID, Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams,
                Symbol.N_ExpressionPartFollowingAMinus, Symbol.N_ExpressionPartFollowingMinusLBracketExpr,
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_LHSGoal, Symbol.N_LHSGoalTail, //Symbol.N_ComparisonTail,
                Symbol.N_PrefixArithmeticExpression, Symbol.N_ArithmeticExpression3Minus, Symbol.N_ArithmeticExpression3MinusLBrackArithExpr,
                // Arithmetic
                Symbol.N_ArithmeticExpression1, Symbol.N_ArithmeticExpression2, Symbol.N_ArithmeticExpression3, Symbol.N_ArithmeticExpression4,
                Symbol.N_ArithmeticExpression1Foo, Symbol.N_ArithmeticExpression2Foo, Symbol.N_OpType_Add, Symbol.N_OpType_Multiply,
                Symbol.N_ModExpression, Symbol.N_PrefixMinusExpression,
                // Arithmetic comparisons
                Symbol.N_ComparisonOperator, Symbol.N_ArithmeticAndComparisonTail,
                //, Symbol.N_GoalBeginningWithArithmeticExpression, Symbol.N_ArithmeticExpressionTail1, Symbol.N_ArithmeticExpressionTail2
                // Lists
                Symbol.N_List, Symbol.N_ListContents, Symbol.N_ListTail, Symbol.N_ListContentsTail,
                // Sequences
                Symbol.N_Sequence,
                // Definite Clause Grammar support
                Symbol.N_DCGRHSGoal, Symbol.N_DCGRHSGoalList,
                // Caret List (Var1 ^ Var2 ^ ... ^ functorExpression) support
                Symbol.N_CaretTail, Symbol.N_CaretTail2
            });

            AddProduction(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_Dot, Symbol.T_EOF });
            AddProduction(Symbol.N_Input, new List<object>() { Symbol.N_Clause, "#createClause" });
            AddProduction(Symbol.N_Input, new List<object>() { Symbol.N_Query, "#createCSharpGoalList" });

            AddProduction(Symbol.N_Clause, new List<object>() { Symbol.N_LHSGoal, Symbol.N_ClauseTail });
            AddProduction(Symbol.N_LHSGoal, new List<object>() { Symbol.T_NameBeginningWithCapital, Symbol.N_LHSGoalTail });
            AddProduction(Symbol.N_LHSGoal, new List<object>() { Symbol.N_Functor, Symbol.N_LHSGoalTail });
            AddProduction(Symbol.N_Functor, new List<object>() { Symbol.T_NameNotBeginningWithCapital });
            AddProduction(Symbol.N_Functor, new List<object>() { Symbol.T_Is });
            //AddProduction(Symbol.N_Functor, new List<object>() { Symbol.T_Not });
            AddProduction(Symbol.N_Functor, new List<object>() { Symbol.N_ComparisonOperator });
            AddProduction(Symbol.N_Functor, new List<object>() { Symbol.N_OpType_EqualOrUnifiable });
#if DEAD_CODE
            // We cannot add a production that says N_Functor := T_Minus, because of the unary minus operator.
            AddProduction(Symbol.N_Functor, new List<object>() { Symbol.T_Plus });
            AddProduction(Symbol.N_Functor, new List<object>() { Symbol.N_OpType_Multiply });
            //AddProduction(Symbol.N_Functor, new List<object>() { Symbol.T_Mod });
#endif
            AddProduction(Symbol.N_LHSGoalTail, new List<object>() { Symbol.Lambda, "#functorExpressionNoArgs" });
            AddProduction(Symbol.N_LHSGoalTail, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression2" });
            AddProduction(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExprList" });
            AddProduction(Symbol.N_ExpressionList, new List<object>() {
                Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExpressionList, "#exprList" });

            AddProduction(Symbol.N_ClauseTail, new List<object>() {
                Symbol.T_From, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#cons", "#clauseAsFunctor" });
            AddProduction(Symbol.N_ClauseTail, new List<object>() { Symbol.Lambda, "#nil", "#clauseAsFunctor" });
            AddProduction(Symbol.N_GoalList, new List<object>() {
                Symbol.T_Comma, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#cons" });
            AddProduction(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#nil" });
            AddProduction(Symbol.N_Query, new List<object>() {
                Symbol.T_InferPred, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#cons" });
            AddProduction(Symbol.N_GoalWithPossibleDisjunctiveTail, new List<object>() {
                Symbol.N_Goal, Symbol.N_PossibleDisjunctiveTail });
            AddProduction(Symbol.N_PossibleDisjunctiveTail, new List<object>() { Symbol.Lambda });
            AddProduction(Symbol.N_PossibleDisjunctiveTail, new List<object>() {
                Symbol.T_Semicolon, Symbol.N_Goal, Symbol.N_PossibleDisjunctiveTail, "#goalDisjunction" });

            // ThAW 2014/03/18 : This is where the fun begins.
            AddProduction(Symbol.N_Goal, new List<object>() { Symbol.N_Expression, "#convertExpressionToFunctorExpression" });

            AddProduction(Symbol.N_Expression, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_IfThenElseTail, Symbol.T_RightBracket, Symbol.N_ArithmeticAndComparisonTail });
            AddProduction(Symbol.N_Expression, new List<object>() { Symbol.T_IntegerLiteral, Symbol.N_ExpressionPartFollowingAnInteger });
            AddProduction(Symbol.N_Expression, new List<object>() { Symbol.T_FloatLiteral, Symbol.N_ExpressionPartFollowingAnInteger });
            AddProduction(Symbol.N_Expression, new List<object>() {
                Symbol.T_NameBeginningWithCapital, Symbol.N_ExpressionPartFollowingAnUpperCaseID });
            AddProduction(Symbol.N_Expression, new List<object>() {
                Symbol.N_Functor, Symbol.N_ExpressionPartFollowingALowerCaseID });
            AddProduction(Symbol.N_Expression, new List<object>() { Symbol.T_Minus, Symbol.N_ExpressionPartFollowingAMinus });
            AddProduction(Symbol.N_ModExpression, new List<object>() {
                Symbol.T_Mod, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket,
                "#arithExpr_Prefix" });
            AddProduction(Symbol.N_IfThenElseTail, new List<object>() { Symbol.N_Sequence }); // This can produce Lambda.
            AddProduction(Symbol.N_IfThenElseTail, new List<object>() {
                Symbol.T_IfThen, Symbol.N_Goal, Symbol.T_Colon, Symbol.N_Goal, "#ifThenElse" });
            AddProduction(Symbol.N_IfThenElseTail, new List<object>() {
                Symbol.T_From, Symbol.N_GoalWithPossibleDisjunctiveTail, Symbol.N_GoalList, "#cons", "#clauseAsFunctor" });

            // Sequences.
            AddProduction(Symbol.N_Sequence, new List<object>() { Symbol.Lambda });
            AddProduction(Symbol.N_Sequence, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_Sequence, "#consSeq" });

            AddProduction(Symbol.N_InfixNonArithmeticPredicateTail, new List<object>() { Symbol.Lambda });
            AddProduction(Symbol.N_InfixNonArithmeticPredicateTail, new List<object>() {
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression, "#infix" });

            AddProduction(Symbol.N_InfixPredicateTail, new List<object>() { Symbol.Lambda });
            AddProduction(Symbol.N_InfixPredicateTail, new List<object>() {
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression, "#infix" });
            AddProduction(Symbol.N_InfixPredicateTail, new List<object>() {
                Symbol.N_ComparisonOperator, Symbol.N_Expression, "#infix" }); // N_Expression here may be e.g. +(2, 2)

            AddProduction(Symbol.N_ArithmeticAndComparisonTail, new List<object>() { Symbol.N_InfixPredicateTail });
            AddProduction(Symbol.N_ArithmeticAndComparisonTail, new List<object>() {
                Symbol.N_OpType_Add, Symbol.N_ArithmeticExpression2, "#infix", Symbol.N_ArithmeticExpression1Foo, Symbol.N_InfixPredicateTail });
            AddProduction(Symbol.N_ArithmeticAndComparisonTail, new List<object>() {
                Symbol.N_OpType_Multiply, Symbol.N_ArithmeticExpression3, "#infix", Symbol.N_ArithmeticExpression2Foo, Symbol.N_ArithmeticExpression1Foo,
                Symbol.N_InfixPredicateTail });

            // Lists.
            AddProduction(Symbol.N_Expression, new List<object>() { Symbol.N_List, Symbol.N_ListTail });
            AddProduction(Symbol.N_List, new List<object>() {
                Symbol.T_LeftSquareBracket, Symbol.N_ListContents, Symbol.T_RightSquareBracket });
            AddProduction(Symbol.N_ListContents, new List<object>() { Symbol.Lambda, "#nil" });
            AddProduction(Symbol.N_ListContents, new List<object>() { Symbol.N_Expression, Symbol.N_ListContentsTail, "#cons" });
            AddProduction(Symbol.N_ListContentsTail, new List<object>() { Symbol.Lambda, "#nil" });
            AddProduction(Symbol.N_ListContentsTail, new List<object>() {
                Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ListContentsTail, "#cons" });
            AddProduction(Symbol.N_ListContentsTail, new List<object>() { Symbol.T_OrBar, Symbol.N_Expression });
            AddProduction(Symbol.N_ListTail, new List<object>() { Symbol.Lambda });
            AddProduction(Symbol.N_ListTail, new List<object>() {
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression, "#infix" });
            AddProduction(Symbol.N_Expression, new List<object>() { 
                Symbol.T_Dot, Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.T_Comma, Symbol.N_Expression, Symbol.T_RightBracket, "#cons",
                Symbol.N_InfixNonArithmeticPredicateTail });

            AddProduction(Symbol.N_ExpressionPartFollowingAnInteger, new List<object>() { Symbol.N_ArithmeticAndComparisonTail });
            AddProduction(Symbol.N_ExpressionPartFollowingAnInteger, new List<object>() {
                Symbol.T_Is, Symbol.N_Expression, "#infix" /* "#is" */ });
            AddProduction(Symbol.N_ExpressionPartFollowingAnUpperCaseID, new List<object>() {
                "#variable", Symbol.N_ExpressionPartFollowingAnInteger });
            AddProduction(Symbol.N_ExpressionPartFollowingAnUpperCaseID , new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression2" });
            AddProduction(Symbol.N_ExpressionPartFollowingALowerCaseID, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket,
                "#functorExpression2", // This functor may be converted to a goal later.
                Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams });
            AddProduction(Symbol.N_ExpressionPartFollowingALowerCaseID, new List<object>() {
                "#functorExpressionNoArgs", Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams });
            AddProduction(Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams, new List<object>() { Symbol.Lambda });
            AddProduction(Symbol.N_ExpressionPartFollowingALowerCaseIDWithParams, new List<object>() {
                Symbol.N_OpType_EqualOrUnifiable, Symbol.N_Expression, "#infix" });
            AddProduction(Symbol.N_ExpressionPartFollowingAMinus, new List<object>() {
                Symbol.T_IntegerLiteral, "#unaryMinus", Symbol.N_ExpressionPartFollowingAnInteger });
            AddProduction(Symbol.N_ExpressionPartFollowingAMinus, new List<object>() {
                Symbol.T_FloatLiteral, "#unaryMinus", Symbol.N_ExpressionPartFollowingAnInteger });
            AddProduction(Symbol.N_ExpressionPartFollowingAMinus, new List<object>() {
                Symbol.T_NameBeginningWithCapital, "#variable", "#unaryMinus", Symbol.N_ExpressionPartFollowingAnInteger });
            AddProduction(Symbol.N_ExpressionPartFollowingAMinus, new List<object>() {
                Symbol.N_PrefixArithmeticExpression, "#unaryMinus", Symbol.N_ExpressionPartFollowingAnInteger });
            AddProduction(Symbol.N_ExpressionPartFollowingAMinus, new List<object>() {
                Symbol.N_PrefixMinusExpression, "#unaryMinus", Symbol.N_ExpressionPartFollowingAnInteger });
            AddProduction(Symbol.N_ExpressionPartFollowingAMinus, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.N_ExpressionPartFollowingMinusLBracketExpr });
            AddProduction(Symbol.N_ExpressionPartFollowingMinusLBracketExpr, new List<object>() {
                Symbol.T_RightBracket, "#unaryMinus", Symbol.N_ExpressionPartFollowingAnInteger });

            // Handle + and -
            // Remember that for LL(1) grammars, semantic actions don't have to be placed at the end of the production (unlike with *LR(1) grammars).
            // See Fischer & LeBlanc page 239 for the Micro grammar with semantic action symbols.
            AddProduction(Symbol.N_ArithmeticExpression1, new List<object>() {
                Symbol.N_ArithmeticExpression2, Symbol.N_ArithmeticExpression1Foo });
            AddProduction(Symbol.N_ArithmeticExpression1Foo, new List<object>() {
                Symbol.N_OpType_Add, Symbol.N_ArithmeticExpression2, "#infix", Symbol.N_ArithmeticExpression1Foo });
            AddProduction(Symbol.N_ArithmeticExpression1Foo, new List<object>() { Symbol.Lambda });
            AddProduction(Symbol.N_OpType_Add, new List<object>() { Symbol.T_Plus });
            AddProduction(Symbol.N_OpType_Add, new List<object>() { Symbol.T_Minus });
            // Handle * and /
            AddProduction(Symbol.N_ArithmeticExpression2, new List<object>() {
                Symbol.N_ArithmeticExpression3, Symbol.N_ArithmeticExpression2Foo });
            AddProduction(Symbol.N_ArithmeticExpression2Foo, new List<object>() {
                Symbol.N_OpType_Multiply, Symbol.N_ArithmeticExpression3, "#infix", Symbol.N_ArithmeticExpression2Foo });
            AddProduction(Symbol.N_ArithmeticExpression2Foo, new List<object>() { Symbol.Lambda });
            AddProduction(Symbol.N_OpType_Multiply, new List<object>() { Symbol.T_Multiply });
            AddProduction(Symbol.N_OpType_Multiply, new List<object>() { Symbol.T_Divide });
            AddProduction(Symbol.N_ArithmeticExpression3, new List<object>() { Symbol.N_ArithmeticExpression4 });
            AddProduction(Symbol.N_ArithmeticExpression3, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket });
            AddProduction(Symbol.N_ArithmeticExpression3, new List<object>() {
                Symbol.T_Minus, Symbol.N_ArithmeticExpression3Minus });
            AddProduction(Symbol.N_PrefixMinusExpression, new List<object>() {
                Symbol.T_Minus, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket,
                "#arithExpr_Prefix" });
            AddProduction(Symbol.N_ArithmeticExpression3Minus, new List<object>() {
                Symbol.N_PrefixMinusExpression, "#unaryMinus" });
            AddProduction(Symbol.N_ArithmeticExpression3Minus, new List<object>() { Symbol.N_ArithmeticExpression4, "#unaryMinus" });
            AddProduction(Symbol.N_ArithmeticExpression3Minus, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.N_ArithmeticExpression3MinusLBrackArithExpr });
            AddProduction(Symbol.N_ArithmeticExpression3MinusLBrackArithExpr, new List<object>() {
                Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket, "#arithExpr_Prefix" });
            AddProduction(Symbol.N_ArithmeticExpression3MinusLBrackArithExpr, new List<object>() {
                Symbol.T_RightBracket, "#unaryMinus" });
            // Expressions represented by N_ArithmeticExpression4 do not rely on order of operations to be evaluated
            // (except possibly for the arguments to the prefix-operator expressions).
            AddProduction(Symbol.N_ArithmeticExpression4, new List<object>() { Symbol.N_PrefixArithmeticExpression });
            AddProduction(Symbol.N_ArithmeticExpression4, new List<object>() { Symbol.N_NumberOrVariableExpression });
            AddProduction(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_IntegerLiteral });
            AddProduction(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_FloatLiteral });
            AddProduction(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_NameBeginningWithCapital, "#variable" });

            // Prefix usages of operators that are usually infix: "is", arithmetic operators, and arithmetic comparison operators.
            // is :
            //AddProduction(Symbol.N_Expression, new List<object>() {
            //    Symbol.T_Is, Symbol.T_LeftBracket, Symbol.N_NumberOrVariableExpression, Symbol.T_Comma, Symbol.N_ArithmeticExpression1,
            //    Symbol.T_RightBracket, "#is" });
            // Arithmetic operators:
            AddProduction(Symbol.N_Expression, new List<object>() { Symbol.N_PrefixArithmeticExpression, Symbol.N_ArithmeticAndComparisonTail });
#if !DEAD_CODE
            AddProduction(Symbol.N_PrefixArithmeticExpression, new List<object>() {
                Symbol.T_Plus, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket,
                "#arithExpr_Prefix" });
            AddProduction(Symbol.N_PrefixArithmeticExpression, new List<object>() {
                Symbol.N_OpType_Multiply, Symbol.T_LeftBracket, Symbol.N_ArithmeticExpression1, Symbol.T_Comma, Symbol.N_ArithmeticExpression1,
                Symbol.T_RightBracket, "#arithExpr_Prefix" });
#endif
            AddProduction(Symbol.N_PrefixArithmeticExpression, new List<object>() { Symbol.N_ModExpression });
            AddProduction(Symbol.N_ExpressionPartFollowingMinusLBracketExpr, new List<object>() {
                Symbol.T_Comma, Symbol.N_ArithmeticExpression1, Symbol.T_RightBracket, "#arithExpr_Prefix", Symbol.N_ArithmeticAndComparisonTail });

            AddProduction(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_LessThan });
            AddProduction(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_GreaterThan });
            AddProduction(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_LessEqual });
            AddProduction(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_GreaterEqual });
            AddProduction(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_ArithmeticEquals });
            AddProduction(Symbol.N_ComparisonOperator, new List<object>() { Symbol.T_ArithmeticNotEquals });

            AddProduction(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_Assign });        // = (unification)
            AddProduction(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_NotUnifiable });  // \=
            AddProduction(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_Equals });        // ==
            AddProduction(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_NotEqual });      // \==
            AddProduction(Symbol.N_OpType_EqualOrUnifiable, new List<object>() { Symbol.T_Univ });          // =..

            // String literals.
            AddProduction(Symbol.N_Expression, new List<object>() { Symbol.T_StringLiteral });

            // Not symbol: \+
            AddProduction(Symbol.N_Expression, new List<object>() {
                Symbol.T_NotSymbol, Symbol.N_Goal, "#not" });   // #metaPredicateWithGoal

            // Definite Clause Grammar support.
            // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch7
            // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch8
            // The #DCGClause semantic action will add the extra arguments to the goals in order to support the difference list mechanism.
            // It also converts lists into "unifies-with" predicates.
            AddProduction(Symbol.N_ClauseTail, new List<object>() {
                Symbol.T_DCGArrow, Symbol.N_DCGRHSGoal, Symbol.N_DCGRHSGoalList, "#DCGObjectList", "#DCGClause" });
            AddProduction(Symbol.N_DCGRHSGoalList, new List<object>() { Symbol.Lambda, "#DCGEmptyObjectList" });
            AddProduction(Symbol.N_DCGRHSGoalList, new List<object>() {
                Symbol.T_Comma, Symbol.N_DCGRHSGoal, Symbol.N_DCGRHSGoalList, "#DCGObjectList" });
            AddProduction(Symbol.N_DCGRHSGoal, new List<object>() { Symbol.N_LHSGoal });
            AddProduction(Symbol.N_DCGRHSGoal, new List<object>() { Symbol.N_List });
            AddProduction(Symbol.N_DCGRHSGoal, new List<object>() {
                Symbol.T_LeftCurlyBrace, Symbol.N_Goal, Symbol.T_RightCurlyBrace, "#markGoalAsNonDCG" });

            // Caret list support for bagof/3 and setof/3:
            AddProduction(Symbol.N_ExpressionPartFollowingAnUpperCaseID, new List<object>() { Symbol.T_Caret, "#createVariableList", Symbol.N_CaretTail });
            AddProduction(Symbol.N_CaretTail, new List<object>() { Symbol.N_Functor,
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression2", "#createCaretList" });
            AddProduction(Symbol.N_CaretTail, new List<object>() { Symbol.T_NameBeginningWithCapital, Symbol.N_CaretTail2 });
            // Note: Functors aren't supposed to begin with a capital letter, but this functor will be converted to a goal.
            AddProduction(Symbol.N_CaretTail2, new List<object>() {
                Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression2", "#createCaretList" });
            AddProduction(Symbol.N_CaretTail2, new List<object>() { Symbol.T_Caret, "#appendToVariableList", Symbol.N_CaretTail });
        }

        private PrologNameExpression<PrologFunctor> PopAndConvertToFunctorExpression(Stack<object> semanticStack, string action)
        {
            var obj = semanticStack.Pop();
            var functorExpression = PrologGlobalInfo.ConvertToFunctorExpression(obj);

            if (functorExpression == null)
            {
                throw new Exception(string.Format(
                    "PopAndConvertToFunctorExpression() : Semantic action {0} : Cannot convert object of type {1} to a functor expression",
                    action, obj.GetType().Name));
            }

            return functorExpression;
        }

        private static PrologVariable GenerateNewVariable()
        {
            return new PrologVariable(string.Format("Var{0}", Guid.NewGuid()));
        }

        public static PrologNameExpression<PrologFunctor> GenerateDCGClause(PrologNameExpression<PrologFunctor> lhsGoal, List<IPrologExpression> rhsExprList)
        {
            var rhsGoalList = new List<IPrologExpression>();

            // We need a way to generate new variables that do not already occur in lhsGoal or objList.
            var variable1 = GenerateNewVariable();
            var variable2 = variable1;

            foreach (var rhsExpr in rhsExprList)
            {
                var variable3 = GenerateNewVariable();

                var rhsGoal = rhsExpr as PrologNameExpression<PrologFunctor>;

                if (rhsGoal == null)
                {
                    throw new Exception("GenerateDCGClause() : obj is not a functor expression.");
                }
                else if (rhsGoal.Name.Name == "." || rhsGoal.Name.Name == "[]")
                {
                    // Assume that obj is a list.
                    var objAsList = rhsGoal; // (PrologNameExpression<PrologFunctor>)obj;
                    var rhsGoalParam1 = variable2;
                    IPrologExpression rhsGoalParam2 = objAsList;
                    PrologNameExpression<PrologFunctor> parentConsCell = null;

                    for (; ; )
                    {

                        if (objAsList.Name.Name == "[]" && objAsList.ExpressionList.Count == 0)
                        {

                            if (parentConsCell != null)
                            {
                                parentConsCell.ExpressionList[1] = variable3;
                            }
                            else
                            {
                                rhsGoalParam2 = variable3;
                            }

                            break;
                        }
                        else if (objAsList.Name.Name == "." && objAsList.ExpressionList.Count == 2)
                        {
                            parentConsCell = objAsList;
                            objAsList = (PrologNameExpression<PrologFunctor>)objAsList.ExpressionList[1];
                        }
                        else
                        {
                            throw new Exception("GenerateDCGClause() : Non-list found where list expected.");
                        }
                    }

                    //rhsGoal = new PrologGoal(gs, new PrologPredicate("unifiable"), new List<IPrologExpression>() { rhsGoalParam1, rhsGoalParam2 });
                    rhsGoal = new PrologNameExpression<PrologFunctor>(GrammarSelector.Prolog2, new PrologFunctor("="),
                        new List<IPrologExpression>() { rhsGoalParam1, rhsGoalParam2 });
                    variable2 = variable3;
                }
                else if (!rhsGoal.DCGDoNotAddExtraArguments)
                {
                    rhsGoal.ExpressionList.Add(variable2);
                    rhsGoal.ExpressionList.Add(variable3);
                    variable2 = variable3;
                }

                rhsGoalList.Add(rhsGoal);
            }

            lhsGoal.ExpressionList.Add(variable1);
            lhsGoal.ExpressionList.Add(variable2);

            //return new PrologClause(lhsGoal, rhsGoalList);
            var rhsGoalListAsPrologList = PrologGlobalInfo.CSharpListToPrologList(rhsGoalList);

            //return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("clause"), new List<IPrologExpression>() { lhsGoal, rhsGoalListAsPrologList });
            return PrologGlobalInfo.CreateClauseAsFunctorExpression(lhsGoal, rhsGoalListAsPrologList);
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            string str;
            List<PrologGoal> goalList;
            IPrologExpression expr;
            IPrologExpression expr2;
            List<IPrologExpression> exprList;
            PrologFunctor functor;
            PrologVariable variable;
            List<PrologVariable> variableList;
            PrologNameExpression<PrologFunctor> functorExpr;
            PrologNameExpression<PrologFunctor> functorExpr2;
            PrologNameExpression<PrologFunctor> functorExpr3;
            PrologClause clause;

            switch (action)
            {
                case "#createClause":
                    clause = PrologGlobalInfo.CreateClause((IPrologExpression)semanticStack.Pop());

                    if (clause == null)
                    {
                        throw new Exception("Semantic action #createClause failed.");
                    }

                    semanticStack.Push(clause);
                    break;

                case "#createCSharpGoalList":
                    goalList = PrologGlobalInfo.PrologListToGoalList((IPrologExpression)semanticStack.Pop());

                    if (goalList == null)
                    {
                        throw new Exception("Semantic action #createCSharpGoalList failed.");
                    }

                    semanticStack.Push(goalList);
                    break;

                case "#convertExpressionToFunctorExpression":
                    semanticStack.Push(PopAndConvertToFunctorExpression(semanticStack, action));
                    break;

                case "#clauseAsFunctor":
                    functorExpr2 = (PrologNameExpression<PrologFunctor>)semanticStack.Pop();
                    //functorExpr = (PrologNameExpression<PrologFunctor>)semanticStack.Pop();
                    functorExpr = PopAndConvertToFunctorExpression(semanticStack, action);
                    //functor = new PrologFunctor("clause");
                    //semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>() { functorExpr, functorExpr2 }));
                    semanticStack.Push(PrologGlobalInfo.CreateClauseAsFunctorExpression(functorExpr, functorExpr2));
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

                case "#functorExpressionNoArgs":
                    str = (string)semanticStack.Pop();
                    functor = new PrologFunctor(str);
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>()));
                    break;

                case "#functorExpression2":
                    exprList = (List<IPrologExpression>)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    str = (string)semanticStack.Pop();
                    functor = new PrologFunctor(str);
                    exprList.Insert(0, expr);
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, exprList));
                    break;

                case "#variable":
                    str = (string)semanticStack.Pop();
                    semanticStack.Push(new PrologVariable(str));
                    break;

                case "#infix": // Infix binary (dyadic) operator.
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    str = (string)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    functor = new PrologFunctor(str);
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, exprList));
                    break;

                case "#arithExpr_Prefix":   // The same as #infix, except for the order of the items on the stack.
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    str = (string)semanticStack.Pop();
                    functor = new PrologFunctor(str);
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, exprList));
                    break;

                case "#unaryMinus":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    str = (string)semanticStack.Pop(); // Remove the - from the stack.
                    expr = new PrologIntegerLiteral(0);
                    exprList = new List<IPrologExpression>() { expr, expr2 };
                    functor = new PrologFunctor(str);
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, exprList));
                    break;

                case "#not":    // #metaPredicateWithGoal
                    functorExpr = PopAndConvertToFunctorExpression(semanticStack, action);
                    functor = new PrologFunctor("not");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>() { functorExpr }));
                    break;

                case "#nil":
                    functor = new PrologFunctor("[]");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>()));
                    break;

                case "#cons":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    functor = new PrologFunctor(".");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>() { expr, expr2 }));
                    break;

                case "#consSeq":
                    expr2 = (IPrologExpression)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    functor = new PrologFunctor("consSeq");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>() { expr, expr2 }));
                    break;

                case "#goalDisjunction":
                    functorExpr2 = (PrologNameExpression<PrologFunctor>)semanticStack.Pop();
                    functorExpr = (PrologNameExpression<PrologFunctor>)semanticStack.Pop();
                    functor = new PrologFunctor("goal_disjunction");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor, new List<IPrologExpression>() { functorExpr, functorExpr2 }));
                    break;

                case "#ifThenElse":
                    functorExpr3 = (PrologNameExpression<PrologFunctor>)semanticStack.Pop();
                    functorExpr2 = (PrologNameExpression<PrologFunctor>)semanticStack.Pop();
                    functorExpr = PopAndConvertToFunctorExpression(semanticStack, action);
                    functor = new PrologFunctor("if_then_else");
                    semanticStack.Push(new PrologNameExpression<PrologFunctor>(gs, functor,
                        new List<IPrologExpression>() { functorExpr, functorExpr2, functorExpr3 }));
                    break;

                case "#DCGClause":
                    // The #DCGClause semantic action will add the extra arguments to the goals in order to support the difference list mechanism.
                    // It also converts lists into "unifiable" predicates.
                    exprList = (List<IPrologExpression>)semanticStack.Pop();
                    functorExpr = (PrologNameExpression<PrologFunctor>)semanticStack.Pop(); // The LHS of the clause.
                    semanticStack.Push(GenerateDCGClause(functorExpr, exprList));
                    break;

                case "#DCGEmptyObjectList":
                    semanticStack.Push(new List<IPrologExpression>());
                    break;

                case "#DCGObjectList":
                    exprList = (List<IPrologExpression>)semanticStack.Pop();
                    expr = (IPrologExpression)semanticStack.Pop();
                    exprList.Insert(0, expr);
                    semanticStack.Push(exprList);
                    break;

                case "#markGoalAsNonDCG":
                    functorExpr = (PrologNameExpression<PrologFunctor>)semanticStack.Peek();
                    functorExpr.DCGDoNotAddExtraArguments = true;
                    break;

                case "#createVariableList":
                    str = (string)semanticStack.Pop();
                    variable = new PrologVariable(str);
                    semanticStack.Push(new List<PrologVariable>() { variable });
                    break;

                case "#appendToVariableList":
                    str = (string)semanticStack.Pop();
                    variableList = (List<PrologVariable>)semanticStack.Peek();
                    variable = new PrologVariable(str);
                    variableList.Add(variable);
                    break;

                case "#createCaretList":
                    functorExpr = (PrologNameExpression<PrologFunctor>)semanticStack.Pop();
                    variableList = (List<PrologVariable>)semanticStack.Pop();
                    semanticStack.Push(new CaretList(variableList, functorExpr));
                    break;

                default:
                    throw new ArgumentException(string.Format("Unrecognized semantic action: {0}", action), "action");
            }
        }

        public override Symbol TokenToSymbol(Token token)
        {
            string tokenValueAsString = token.TokenValue.ToString();

            switch (token.TokenType)
            {
                case TokenType.T_Ident:
                case TokenType.T_Exclamation:   // The cut.

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
                        case "=<": return Symbol.T_LessEqual; // Not <=.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse21
                        case ">=": return Symbol.T_GreaterEqual;
                        case @"\+": return Symbol.T_NotSymbol;
                        case "->": return Symbol.T_IfThen;
                        case ":": return Symbol.T_Colon;
                        case "=": return Symbol.T_Assign;   // Unifiable
                        case @"\=": return Symbol.T_NotUnifiable;
                        case "==": return Symbol.T_Equals;
                        case @"\==": return Symbol.T_NotEqual;
                        case "=:=": return Symbol.T_ArithmeticEquals; // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse21
                        case @"=\=": return Symbol.T_ArithmeticNotEquals;
                        case "-->": return Symbol.T_DCGArrow;
                        case "=..": return Symbol.T_Univ;
                        case "^": return Symbol.T_Caret;
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
                case TokenType.T_StrLit2: return Symbol.T_NameNotBeginningWithCapital; // The contents of a single-quoted string.
                case TokenType.T_Comma: return Symbol.T_Comma;
                case TokenType.T_Dot: return Symbol.T_Dot;
                case TokenType.T_LeftSquareBracket: return Symbol.T_LeftSquareBracket;
                case TokenType.T_RightSquareBracket: return Symbol.T_RightSquareBracket;
                case TokenType.T_OrBar: return Symbol.T_OrBar;
                case TokenType.T_Semicolon: return Symbol.T_Semicolon;
                case TokenType.T_LeftCurlyBrace: return Symbol.T_LeftCurlyBrace;
                case TokenType.T_RightCurlyBrace: return Symbol.T_RightCurlyBrace;
                case TokenType.T_Colon: return Symbol.T_Colon;
                case TokenType.T_QuestionMinus: return Symbol.T_InferPred;
                case TokenType.T_ColonMinus: return Symbol.T_From;
                case TokenType.T_Less: return Symbol.T_LessThan;
                case TokenType.T_EqualLessThan: return Symbol.T_LessEqual;
                case TokenType.T_Greater: return Symbol.T_GreaterThan;
                case TokenType.T_GreaterEqual: return Symbol.T_GreaterEqual;
                case TokenType.T_BackslashPlus: return Symbol.T_NotSymbol;
                case TokenType.T_BackslashEqual: return Symbol.T_NotUnifiable;
                case TokenType.T_EqualEqual: return Symbol.T_Equals;
                case TokenType.T_BackslashEqualEqual: return Symbol.T_NotEqual;
                case TokenType.T_EqualColonEqual: return Symbol.T_ArithmeticEquals;
                case TokenType.T_EqualBackslashEqual: return Symbol.T_ArithmeticNotEquals;
                case TokenType.T_MinusMinusGreaterThan: return Symbol.T_DCGArrow;
                case TokenType.T_EqualDotDot: return Symbol.T_Univ;
                case TokenType.T_Plus: return Symbol.T_Plus;
                case TokenType.T_Minus: return Symbol.T_Minus;
                case TokenType.T_Mult: return Symbol.T_Multiply;
                case TokenType.T_Div: return Symbol.T_Divide;
                case TokenType.T_Equal: return Symbol.T_Assign;   // Unifiable
                case TokenType.T_Arrow: return Symbol.T_IfThen;
                case TokenType.T_FltLit: return Symbol.T_FloatLiteral;
                case TokenType.T_Caret: return Symbol.T_Caret;
                default: break;
            }

            return base.TokenToSymbol(token);
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            var value = token.TokenValue;

            switch (tokenAsSymbol)
            {
                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(new PrologIntegerLiteral((int)value));
                    break;

                case Symbol.T_FloatLiteral:
                    semanticStack.Push(new PrologFloatLiteral((double)value));
                    break;

                case Symbol.T_NameBeginningWithCapital:
                case Symbol.T_NameNotBeginningWithCapital:
                    semanticStack.Push(value);  // value is really a string; it must be converted to a Prolog domain model type later.
                    break;

                case Symbol.T_StringLiteral:
                    semanticStack.Push(PrologGlobalInfo.CSharpStringToPrologCodeList((string)value));
                    break;

                case Symbol.T_Plus:
                case Symbol.T_Minus:
                case Symbol.T_Multiply:
                case Symbol.T_Divide:
                case Symbol.T_Mod:
                case Symbol.T_LessThan:
                case Symbol.T_GreaterThan:
                case Symbol.T_LessEqual:
                case Symbol.T_GreaterEqual:
                case Symbol.T_ArithmeticEquals:
                case Symbol.T_ArithmeticNotEquals:
                case Symbol.T_Assign:   // Unifiable
                case Symbol.T_Equals:
                case Symbol.T_NotEqual:
                case Symbol.T_NotUnifiable:
                case Symbol.T_Univ:
                case Symbol.T_Is:
                    semanticStack.Push(value);
                    break;

                default:
                    break;
            }
        }
    }
}