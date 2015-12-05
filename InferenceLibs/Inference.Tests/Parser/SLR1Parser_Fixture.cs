using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Domain;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Parser
{
#if DEAD_CODE
    #region PrologTestGrammarBase

    public class PrologTestGrammarBase : GrammarBase
    {
        public PrologTestGrammarBase()
            : base(Symbol.N_Start)
        {
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            throw new NotImplementedException();
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
            throw new NotImplementedException();
        }
    }
#endregion

#region BrokenPrologGrammar

    public class BrokenPrologGrammar : PrologTestGrammarBase
    {
        public BrokenPrologGrammar()
        {
            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_LeftBracket, Symbol.T_RightBracket, Symbol.T_From,
                Symbol.T_InferPred, Symbol.T_IntegerLiteral, Symbol.T_NameBeginningWithCapital, Symbol.T_NameNotBeginningWithCapital,
                Symbol.T_EOF });

            NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
                Symbol.N_Input, Symbol.N_Clause, Symbol.N_Query, Symbol.N_Goal,
                Symbol.N_GoalList, Symbol.N_Predicate, Symbol.N_Expression, Symbol.N_ExpressionList,
                Symbol.N_Functor, Symbol.N_Variable, Symbol.N_Name, Symbol.N_NumberOrVariableExpression });

#if !DEAD_CODE
            Terminals.UnionWith(new HashSet<Symbol>() { Symbol.T_Comma, Symbol.T_Dot, Symbol.T_Assign });
#else
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
#endif

            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_Dot, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Clause }, 2));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Query }, 3));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, Symbol.T_From, Symbol.N_Goal, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, "#clauseWithoutSubgoals" }, 5));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.T_Comma, Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 6));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#emptyGoalList" }, 7));
            Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_InferPred, Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 8));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Predicate, "#goalNoArgs" }, 9));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Predicate, Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#goal2" }, 10));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExpressionList, "#exprList" }, 11));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExprList" }, 12));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_NumberOrVariableExpression }, 22));
            Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_IntegerLiteral }, 13));
            Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.N_Variable }, 14));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Functor, "#functorExpressionNoArgs" }, 15));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Functor, Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression2" }, 16));
            Productions.Add(new Production(Symbol.N_Predicate, new List<object>() { Symbol.N_Name, "#predicate" }, 17));
            Productions.Add(new Production(Symbol.N_Functor, new List<object>() { Symbol.T_NameNotBeginningWithCapital, "#functor" }, 18));
            Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_NameBeginningWithCapital, "#variable" }, 19));
            Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameNotBeginningWithCapital }, 20));
            Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameBeginningWithCapital }, 21));
            // Production 22 is between 12 and 13.

#if !DEAD_CODE
            // LALR(1) ?  No: e.g. ?- mia(t) = mia(X), print(X).
            // Reduce-reduce conflict:
            // N_Functor -> T_NameNotBeginningWithCapital
            // N_Name -> T_NameNotBeginningWithCapital
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Expression, Symbol.T_Assign, Symbol.N_Expression, "#=" }, 57));
#else
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
#if DEAD_CODE
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
#endif
        }
    }
#endregion

#region FixedPrologGrammar

    public class FixedPrologGrammar : PrologTestGrammarBase
    {
        public FixedPrologGrammar()
        {
            Terminals.UnionWith(new HashSet<Symbol>() {
                Symbol.T_LeftBracket, Symbol.T_RightBracket, Symbol.T_From,
                Symbol.T_InferPred, Symbol.T_IntegerLiteral, Symbol.T_NameBeginningWithCapital, Symbol.T_NameNotBeginningWithCapital,
                Symbol.T_EOF });

            NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
                Symbol.N_Input, Symbol.N_Clause, Symbol.N_Query, Symbol.N_Goal,
                Symbol.N_GoalList, Symbol.N_Predicate, Symbol.N_Expression, Symbol.N_ExpressionList,
                Symbol.N_Functor, Symbol.N_Variable, Symbol.N_Name, Symbol.N_NumberOrVariableExpression });

#if !DEAD_CODE
            Terminals.UnionWith(new HashSet<Symbol>() { Symbol.T_Comma, Symbol.T_Dot, Symbol.T_Assign });

            NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_FunctorExpression });
#else
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
#endif

            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Clause, Symbol.T_Dot }, 2));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Query, Symbol.T_Dot }, 3));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, Symbol.T_From, Symbol.N_Goal, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, Symbol.T_From, Symbol.N_FunctorExpression, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_FunctorExpression, Symbol.T_From, Symbol.N_Goal, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_FunctorExpression, Symbol.T_From, Symbol.N_FunctorExpression, Symbol.N_GoalList, "#clauseWithSubgoals" }, 4));
            Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_Goal, "#clauseWithoutSubgoals" }, 5));
            //Productions.Add(new Production(Symbol.N_Clause, new List<object>() { Symbol.N_FunctorExpression, "#clauseWithoutSubgoals" }, 5));
            Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_FunctorExpression, Symbol.T_Dot, "#clauseWithoutSubgoals" }, 5));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.T_Comma, Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 6));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.T_Comma, Symbol.N_FunctorExpression, Symbol.N_GoalList, "#goalList" }, 6));
            Productions.Add(new Production(Symbol.N_GoalList, new List<object>() { Symbol.Lambda, "#emptyGoalList" }, 7));
            Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_InferPred, Symbol.N_Goal, Symbol.N_GoalList, "#goalList" }, 8));
            Productions.Add(new Production(Symbol.N_Query, new List<object>() { Symbol.T_InferPred, Symbol.N_FunctorExpression, Symbol.N_GoalList, "#goalList" }, 8));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Predicate, "#goalNoArgs" }, 9));
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Predicate, Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#goal2" }, 10));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExpressionList, "#exprList" }, 11));
            Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExprList" }, 12));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_NumberOrVariableExpression }, 22));
            Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.T_IntegerLiteral }, 13));
            Productions.Add(new Production(Symbol.N_NumberOrVariableExpression, new List<object>() { Symbol.N_Variable }, 14));
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_FunctorExpression }, 22));
            Productions.Add(new Production(Symbol.N_FunctorExpression, new List<object>() { Symbol.N_Functor, "#functorExpressionNoArgs" }, 15));
            Productions.Add(new Production(Symbol.N_FunctorExpression, new List<object>() { Symbol.N_Functor, Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.N_ExpressionList, Symbol.T_RightBracket, "#functorExpression2" }, 16));
            Productions.Add(new Production(Symbol.N_Predicate, new List<object>() { Symbol.N_Name, "#predicate" }, 17));
            Productions.Add(new Production(Symbol.N_Functor, new List<object>() { Symbol.T_NameNotBeginningWithCapital, "#functor" }, 18));
            Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_NameBeginningWithCapital, "#variable" }, 19));
            //Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameNotBeginningWithCapital }, 20));
            Productions.Add(new Production(Symbol.N_Name, new List<object>() { Symbol.T_NameBeginningWithCapital }, 21));
            // Production 22 is between 12 and 13.

            // LALR(1) ?  No: e.g. ?- mia(t) = mia(X), print(X).
            // Reduce-reduce conflict:
            // N_Functor -> T_NameNotBeginningWithCapital
            // N_Name -> T_NameNotBeginningWithCapital
            Productions.Add(new Production(Symbol.N_Goal, new List<object>() { Symbol.N_Expression, Symbol.T_Assign, Symbol.N_Expression, "#=" }, 57));
        }
    }
#endregion
#endif

    [TestFixture]
    public class SLR1Parser_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public SLR1Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.Inference);
        }

        [Test]
        public void RecognizeManIsMortalTest()
        {
            parser.Recognize(tokenizer.Tokenize("@isMan(?x) -> @isMortal(?x)"));
        }

        [Test]
        public void MicroRecognizeTest1()
        {
            IParser parserMicro = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.Micro);

            parserMicro.Recognize(tokenizer.Tokenize("begin abc := def + 123; i := i - 1; end"));
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

#if DEAD_CODE
        [Test]
        public void BrokenPrologGrammarTest()
        {
            var t = TokenizerFactory.Create(GrammarSelector.Prolog2);
            var p = new SLR1Parser(new BrokenPrologGrammar());

            Assert.Throws<ReduceReduceConflictException>(() => p.Recognize(t.Tokenize("?- mia(X) = mia(t).")));
        }

        [Test]
        public void FixedPrologGrammarTest()
        {
            var t = TokenizerFactory.Create(GrammarSelector.Prolog2);
            var p = new SLR1Parser(new FixedPrologGrammar());

            //Assert.Throws<ReduceReduceConflictException>(() => p.Recognize(t.Tokenize("?- mia(X) = mia(t).")));
            p.Recognize(t.Tokenize("?- mia(X) = mia(t)."));
#if DEAD_CODE
            p.Recognize(t.Tokenize("number(1).")); // This causes a shift-reduce conflict.
            p.Recognize(t.Tokenize("Number(1)."));
            p.Recognize(t.Tokenize("foo(X) :- bar(X)."));
            p.Recognize(t.Tokenize("foo(X) :- Bar(X)."));
            p.Recognize(t.Tokenize("Foo(X) :- bar(X)."));
            p.Recognize(t.Tokenize("Foo(X) :- Bar(X)."));
            p.Recognize(t.Tokenize("?- number(X), print(X)."));
            //p.Recognize(t.Tokenize("."));
#endif
        }
#endif
    }
}
