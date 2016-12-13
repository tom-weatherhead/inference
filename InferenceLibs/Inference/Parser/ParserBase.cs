using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Inference.Parser
{
    #region Symbol

    public enum Symbol
    {
        Lambda,         // The empty symbol.
        Dot,            // A sort of cursor.  For use in LR parsing.

        // Terminal symbols
        T_Begin,
        T_End,
        T_Assign,
        T_Semicolon,
        T_Read,
        T_Write,
        T_LeftBracket,
        T_RightBracket,
        T_Comma,
        T_ID,
        T_Variable,
        T_IntegerLiteral,
        T_FloatLiteral,     // We will use the "double" data type to represent values associated with this symbol.
        T_StringLiteral,
        T_Plus,
        T_Minus,
        // Inference terminals
        T_BoolID,
        T_SkolemID,
        T_Exclamation,
        T_2Ampersand,
        T_2OrBar,
        T_Arrow,
        T_ForAll,
        T_Exists,
        // LISP terminals
        T_Define,
        T_If,
        T_While,
        T_Set,
        T_Multiply,
        T_Divide,
        T_Equals,
        T_LessThan,
        T_GreaterThan,
        T_Print,
        T_Cons,
        T_Car,
        T_Cdr,
        T_NumberPred,   // The "number?" predicate
        T_SymbolPred,
        T_ListPred,
        T_NullPred,
        T_StringPred,
        T_Apostrophe,
        T_Dot,
        T_Rplaca,
        T_Rplacd,
        T_DefineMacro,
        T_QuoteKeyword, // The keyword "quote", for quoting something within a quoted S-expression.
        T_Random,
        T_ToString,
        T_ListToString,
        T_StringToList,
        T_StringToSymbol,
        T_Floor,
        T_Atan2,
        T_Throw,
        T_StringLessThan,
        // APL terminals
        T_Max,
        T_Or,
        T_And,
        T_PlusSlash,
        T_MinusSlash,
        T_MultiplySlash,
        T_DivideSlash,
        T_MaxSlash,
        T_OrSlash,
        T_AndSlash,
        T_Compress,
        T_Shape,
        T_Ravel,
        T_Restruct,
        T_Cat,
        T_Indx,
        T_Trans,
        T_SquareBrackets,   // Subscripting
        // T_Assign defined above; we will use it in APL for vector element assignment: (:= v i x)
        T_DoubleSubscripting,
        T_Pow,                  // x to the power of y
        T_Exp,                  // e to the power of x
        T_Ln,                   // Natural logarithm; log (base e) of x
        T_Sin,
        T_Cos,
        T_Tan,
        // Scheme terminals
        T_PrimOpPred,
        T_ClosurePred,
        T_LambdaKeyword,    // Not to be confused with the "Lambda" symbol, which represents nothing.
        T_Let,
        T_LetStar,
        T_LetRec,
        T_Cond,
        T_CallCC,
        T_List,
        // No new terminals for SASL
        // CLU terminals
        T_Cluster,
        T_Rep,
        T_Dollar,
        T_Export,
        // Smalltalk terminals
        T_Class,
        T_Octothorpe,
        T_ObjectPred,
        T_Strlen,
        T_Substr,
        T_Typename,
        T_Hash,
        T_ReferenceEquals,
        T_Strcat,
        T_NewArray,
        T_ArrayLength,
        T_ArrayGet,
        T_ArraySet,
        T_ArrayPred,
        T_CharPred,
        T_StringIndex,  // Used to get a character from a string; e.g. str[3]
        // Prolog terminals
        T_Infer,
        T_From,
        T_InferPred,
        T_NameBeginningWithCapital,
        T_NameNotBeginningWithCapital,
        // Prolog2 terminals
        T_Is,
        T_LessEqual,
        T_GreaterEqual,
        T_Assert,
        T_AssertA,
        T_AssertZ,
        T_Retract,
        T_RetractAll,
        T_Not,
        T_LeftSquareBracket,
        T_RightSquareBracket,
        T_OrBar,
        T_NotSymbol,    // The symbol for goal negation: \+
        T_IfThen,       // ->
        T_Colon,
        T_NotEqual,
        T_NotUnifiable,
        T_Mod,
        T_ArithmeticEquals,
        T_ArithmeticNotEquals,
        T_DCGArrow,     // The Definite Clause Grammar arrow: -->
        T_Univ,         // =..
        T_Caret,        // ^
        // JSON terminals
        T_LeftCurlyBrace,
        T_RightCurlyBrace,
        // End of file terminal
        T_EOF,

        // Non-terminal symbols
        N_Start,        // The start symbol (i.e. the system goal)
        N_Program,
        N_StatementList,
        N_StatementTail,
        N_Statement,
        N_IDList,
        N_IDTail,
        N_ExprList,
        N_ExprTail,
        N_Expression,
        N_PrimaryTail,
        N_Primary,
        N_AddOp,
        // Inference non-terminals
        N_BoolExpr,
        N_BoolExprTail,
        N_JunctionTail,     // I.e. the second half of either a conjunction or a disjunction.
        N_BoolAtom,
        N_ArgumentList,
        N_ArgumentTail,
        N_Argument,
        N_ArgIdentTail,
        // LISP non-terminals
        N_Input,
        N_FunDef,   // Function definition
        N_Function,
        N_ArgList,
        N_VariableList,
        N_Variable,
        N_Value,
        N_BracketedExpression, 
        N_ExpressionList,
        N_Optr,     // Operator
        N_ValueOp,
        N_QuotedConst,
        N_SExpression,
        N_SExpressionList,
        N_Symbol,
        N_MacroDef,
        // APL non-terminals
        N_VectorConst,
        N_IntegerLiteralList,
        N_FloatLiteralList,
        // Scheme non-terminals
        N_LetKeyword,
        N_VarExprList,
        N_ExprPairList,
        // No new non-terminals for SASL
        // CLU non-terminals
        N_ClusterDef,
        N_Cluster,
        N_Rep,
        N_FunDefList,
        N_OnePartName,
        N_TwoPartName,
        N_ExportList,
        N_OnePartNameList,
        // Smalltalk non-terminals
        N_ClassDef,
        N_Class,
        N_InstVars,
        N_MethodDef,
        N_MethodDefList,
        N_LiteralList,
        // Prolog non-terminals
        N_Clause,
        N_Query,
        N_Goal,
        N_GoalList,
        N_Predicate,
        N_Functor,
        N_Name,
        N_NumberOrVariableExpression,
        // Prolog2 non-terminals
        N_ComparisonOperator,
        //N_MetaPredicateWithGoal,  // E.g. not(goal)
        N_MetaPredicateWithClause,
        N_ListContents,
        N_ListTail,
        N_Sequence,
        N_ArithmeticExpression1,
        N_ArithmeticExpression2,
        N_GoalDisjunctionTail,
        N_FunctorExpression, // This is used in the SLR(1) and LL(1) tests.
        // LL(1) Prolog grammar test:
        N_ClauseTail,
        N_GoalTail,
        N_FunctorExpressionTail,
        N_FunctorGoalTail,
        N_ExpressionNotFunctorOrVariableOrInteger, // Does not replace N_NumberOrVariableExpression
        N_GoalBeginningWithPredicateOrVariable,
        N_GoalBeginningWithInteger,
        N_ArithmeticExpression3,
        //N_GoalBeginningWithArithmeticExpression,
        //N_ArithmeticExpressionTail1,
        //N_ArithmeticExpressionTail2,
        N_ArithmeticExpression1Foo,
        N_ArithmeticExpression2Foo,
        N_OpType_Add,
        N_OpType_Multiply,
        N_OpType_EqualOrUnifiable,
        N_GoalWithPossibleDisjunctiveTail,
        N_PossibleDisjunctiveTail,
        // Begin LL1_PROLOG_ATTEMPT2
        N_ArithmeticExpression4,
        N_ModExpression,
        N_IfThenElseTail,
        //N_ExpressionListTail,
        //N_ExpressionTail,
        N_InfixNonArithmeticPredicateTail,
        N_InfixPredicateTail,
        N_ComparisonTail,
        N_List,
        N_ListContentsTail,
        N_ExpressionPartFollowingAnInteger,
        N_ExpressionPartFollowingAnUpperCaseID,
        N_ExpressionPartFollowingALowerCaseID,
        N_ExpressionPartFollowingALowerCaseIDWithParams,
        N_ExpressionPartFollowingAMinus,
        N_ExpressionPartFollowingMinusLBracketExpr,
        N_LHSGoal,
        N_LHSGoalTail,
        N_ArithmeticAndComparisonTail,
        N_PrefixArithmeticExpression,
        N_ArithmeticExpression3Minus,
        N_ArithmeticExpression3MinusLBrackArithExpr,
        N_PrefixMinusExpression,
        N_DCGRHSGoal,
        N_DCGRHSGoalList,
        N_CaretTail,
        N_CaretTail2,
        // End LL1_PROLOG_ATTEMPT2
        // JSON terminals
        N_ValueListTail,
        N_KeyValuePairListTail
    }

    #endregion

    #region IParser

    public interface IParser
    {
        void Recognize(List<Token> tokenList);
        object Parse(List<Token> tokenList);
    }

    #endregion

    #region ParserBase

    public abstract class ParserBase : IParser
    {
        // The grammar:
        protected readonly IGrammar grammar = null;

        protected readonly HashSet<Symbol> derives_lambda = new HashSet<Symbol>();
        protected readonly Dictionary<Symbol, HashSet<Symbol>> first_set = new Dictionary<Symbol, HashSet<Symbol>>();
        protected readonly Dictionary<Symbol, HashSet<Symbol>> follow_set = new Dictionary<Symbol, HashSet<Symbol>>();

        protected ParserBase(IGrammar g)
        {
            grammar = g;

            MarkLambda();
            FillFirstSet();
            FillFollowSet();
        }

        // Adapted from Fischer and LeBlanc, page 103

        protected void MarkLambda()
        {
            bool changes;

            do
            {
                changes = false;

                foreach (Production p in grammar.Productions)
                {

                    if (!derives_lambda.Contains(p.lhs))
                    {
                        /*

                        if (p.rhs.Count == 0)
                        {
                            // Derives lambda directly
                            derives_lambda.Add(p.lhs);
                            changes = true;
                            continue;
                        }

                        // Does each part of RHS derive lambda?
                        bool rhs_derives_lambda = true;

                        foreach (Symbol rhsSymbol in p.rhs)
                        {

                            if (!derives_lambda.Contains(rhsSymbol))
                            {
                                rhs_derives_lambda = false;
                                break;
                            }
                        }
                         */
                        //bool rhs_derives_lambda = p.StripOutSemanticActions().rhs.All(rhsSymbol => derives_lambda.Contains((Symbol)rhsSymbol));
                        //bool rhs_derives_lambda = ExtractSymbols(p.rhs).All(rhsSymbol => derives_lambda.Contains((Symbol)rhsSymbol));
                        bool rhs_derives_lambda = p.RHSWithNoSemanticActions().All(rhsSymbol => derives_lambda.Contains(rhsSymbol));

                        if (rhs_derives_lambda)
                        {
                            derives_lambda.Add(p.lhs);
                            changes = true;
                        }
                    }
                }
            }
            while (changes);
        }

        protected HashSet<Symbol> withoutLambda(IEnumerable<Symbol> ie)
        {
            /*
            var rtn = new HashSet<Symbol>();

            foreach (Symbol symbol in ie)
            {

                if (symbol != Symbol.Lambda)
                {
                    rtn.Add(symbol);
                }
            }

            return rtn;
             */
            return new HashSet<Symbol>(ie.Where(symbol => symbol != Symbol.Lambda));
        }

        // Adapted from Fischer and LeBlanc, page 104

        protected HashSet<Symbol> ComputeFirst(List<Symbol> alpha)
        {
            int k = alpha.Count;
            var result = new HashSet<Symbol>();

            if (k == 0 || (k == 1 && alpha[0] == Symbol.Lambda))    // ThAW: Originally, this line was just: if (k == 0)
            {
                result.Add(Symbol.Lambda);
            }
            else
            {
                int i;

                if (!first_set.ContainsKey(alpha[0]))
                {
                    throw new Exception(string.Format("ComputeFirst() : first_set does not contain the key {0}", alpha[0]));
                }

                result.UnionWith(first_set[alpha[0]]);

                for (i = 1; i < k && first_set[alpha[i - 1]].Contains(Symbol.Lambda); ++i)
                {
                    // Test:

                    if (!first_set.ContainsKey(alpha[i]))
                    {
                        throw new Exception(string.Format("ComputeFirst() : first_set does not contain the key {0}", alpha[i]));
                    }

                    result.UnionWith(withoutLambda(first_set[alpha[i]]));
                }

                if (i == k && first_set[alpha[k - 1]].Contains(Symbol.Lambda) && !result.Contains(Symbol.Lambda))
                {
                    result.Add(Symbol.Lambda);
                }
            }

            return result;
        }

        protected bool ProductionExists(Symbol A, Symbol a)
        {
            return grammar.Productions.Any(p => p.lhs == A && p.rhs.Count > 0 && p.rhs[0] is Symbol && (Symbol)p.rhs[0] == a);
            /*
            foreach (Production p in grammar.Productions)
            {

                if (p.lhs == A && p.rhs.Count > 0 && p.rhs[0] is Symbol && (Symbol)p.rhs[0] == a)
                {
                    return true;
                }
            }

            return false;
             */
        }

        /*
        protected List<Symbol> ExtractSymbols(List<object> list)
        {
            var result = new List<Symbol>();

            list.Where(o => o is Symbol).ToList().ForEach(o => result.Add((Symbol)o));
            return result;
        }
         */

        // Adapted from Fischer and LeBlanc, page 105

        protected void FillFirstSet()
        {
            bool changes;

            foreach (Symbol A in grammar.NonTerminals)
            {
                var s = new HashSet<Symbol>();

                if (derives_lambda.Contains(A))
                {
                    s.Add(Symbol.Lambda);
                }

                first_set[A] = s;
            }

            foreach (Symbol a in grammar.Terminals)
            {
                var s = new HashSet<Symbol>();

                s.Add(a);
                first_set[a] = s;

                foreach (Symbol A in grammar.NonTerminals)
                {

                    // If there exists a production A -> a beta
                    if (ProductionExists(A, a))
                    {
                        first_set[A].UnionWith(s);
                    }
                }
            }

            do
            {
                changes = false;

                foreach (Production p in grammar.Productions)
                {
                    //HashSet<Symbol> s = ComputeFirst(ExtractSymbols(p.rhs));
                    HashSet<Symbol> s = ComputeFirst(p.RHSWithNoSemanticActions());

                    if (!first_set.ContainsKey(p.lhs))
                    {
                        throw new Exception(string.Format("FillFirstSet() : {0} is not a key in first_set", p.lhs));
                    }

                    if (!s.IsSubsetOf(first_set[p.lhs]))
                    {
                        first_set[p.lhs].UnionWith(s);
                        changes = true;
                    }
                }
            }
            while (changes);
        }

        /*
        protected List<object> Sublist(List<object> src, int i)
        {
            return src.Skip(i).ToList();
        }
         */

        // Adapted from Fischer and LeBlanc, page 106

        protected void FillFollowSet()
        {
            bool changes;

            foreach (Symbol A in grammar.NonTerminals)
            {
                follow_set[A] = new HashSet<Symbol>();
            }

            follow_set[grammar.StartSymbol].Add(Symbol.Lambda);

            do
            {
                changes = false;

                // For each production and each occurrence of a nonterminal in its right-hand side.

                foreach (Production p in grammar.Productions)
                {
                    var rhs = p.RHSWithNoSemanticActions();

                    //for (int i = 0; i < p.rhs.Count; ++i)
                    for (int i = 0; i < rhs.Count; ++i)
                    {
                        /*
                        object o = p.rhs[i];

                        if (!(o is Symbol))
                        {
                            continue;
                        }

                        Symbol B = (Symbol)o;
                         */
                        Symbol B = rhs[i];

                        if (!grammar.NonTerminals.Contains(B))
                        {
                            continue;
                        }

                        //List<object> beta = Sublist(p.rhs, i + 1);
                        List<Symbol> beta = rhs.Skip(i + 1).ToList();
                        //HashSet<Symbol> s = ComputeFirst(ExtractSymbols(beta));
                        HashSet<Symbol> s = ComputeFirst(beta);
                        HashSet<Symbol> sWithoutLambda = withoutLambda(s);

                        if (!sWithoutLambda.IsSubsetOf(follow_set[B]))
                        {
                            follow_set[B].UnionWith(sWithoutLambda);
                            changes = true;
                        }

                        if (s.Contains(Symbol.Lambda) && !follow_set[p.lhs].IsSubsetOf(follow_set[B]))
                        {
                            follow_set[B].UnionWith(follow_set[p.lhs]);
                            changes = true;
                        }
                    }
                }
            }
            while (changes);
        }

        public abstract void Recognize(List<Token> tokenList);

        public abstract object Parse(List<Token> tokenList);
    }

    #endregion

    #region ParserSelector

    public enum ParserSelector
    {
        LL1,
        LR0,
        LR1,
        SLR1,
        LALR1
    }

    #endregion

    #region ParserFactory

    static public class ParserFactory
    {
        static public IParser Create(ParserSelector ps, IGrammar g)
        {

            switch (ps)
            {
                case ParserSelector.LL1:
                    return new LL1Parser(g);

                case ParserSelector.LR0:
                    return new LR0Parser(g);

                case ParserSelector.LR1:
                    return new LR1Parser(g);

                case ParserSelector.SLR1:
                    return new SLR1Parser(g);

                case ParserSelector.LALR1:
                    return new LALR1Parser(g);

                default:
                    break;
            }

            throw new ArgumentException("ParserFactory.Create() : Unrecognized ParserSelector: " + ps.ToString(), "ps");
        }

        static public IParser Create(ParserSelector ps, GrammarSelector gs)
        {
            return Create(ps, GrammarFactory.Create(gs));
        }
    }

    #endregion
}
