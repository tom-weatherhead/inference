#define USE_PROLOG2_LL1 // This is also defined in Inference.Interpreter.InterpreterBase and Inference.Tests.Interpreter.Prolog.Prolog2Parser_Fixture

using System;
using System.Collections.Generic;
using System.Linq;
//using System.Text;
using Inference.Domain;

namespace Inference.Parser
{
    #region Production

    public class Production
    {
        public Symbol lhs { get; set; }
        public List<object> rhs { get; set; }
        private readonly int num;

        public Production(Symbol l, List<object> r, int n = 0)
        {
            lhs = l;
            rhs = r;
            num = n;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            Production that = obj as Production;

            //return that != null && lhs.Equals(that.lhs) && rhs.Equals(that.rhs);

            if (that == null || !lhs.Equals(that.lhs) || rhs.Count != that.rhs.Count)
            {
                return false;
            }

            for (int i = 0; i < rhs.Count; ++i)
            {

                if (!rhs[i].Equals(that.rhs[i]))
                {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode()
        {
            //return lhs.GetHashCode() + 101 * rhs.GetHashCode();
            /*
            int hashCode = lhs.GetHashCode();

            foreach (object o in rhs)
            {
                hashCode *= 101;
                hashCode += o.GetHashCode();
            }

            return hashCode;
             */
            return rhs
                .Select(o => o.GetHashCode())
                .Aggregate(lhs.GetHashCode(), (accumulator, hashCode) => accumulator * 101 + hashCode);
        }

        public override string ToString()
        {
            /*
            var sb = new StringBuilder();

            sb.Append(num.ToString() + ": " + lhs.ToString() + " ->");

            foreach (object o in rhs)
            {
                sb.Append(" " + o.ToString());
            }

            return sb.ToString();
             */
            return string.Format("{0}: {1} -> {2}", num, lhs, string.Join(" ", rhs));
        }

        public List<Symbol> RHSWithNoSemanticActions()
        {
            return rhs.Where(o => o is Symbol).Select(o => (Symbol)o).ToList();
        }

        public Production StripOutSemanticActions()
        {
            /*
            List<object> newRHS = new List<object>();

            foreach (object o in rhs)
            {

                if (o is Symbol)
                {
                    newRHS.Add(o);
                }
            }
             */
            var newRHS = rhs.Where(o => o is Symbol).ToList();

            return new Production(lhs, newRHS, num);
        }

        public bool ContainsSymbol(Symbol symbol)
        {
            return lhs == symbol || rhs.Where(o => o is Symbol).Select(o => (Symbol)o).Any(s => s == symbol);
        }
    }

    #endregion

    #region IGrammar

    public interface IGrammar
    {
        HashSet<Symbol> Terminals { get; }
        HashSet<Symbol> NonTerminals { get; }
        Symbol StartSymbol { get; }
        List<Production> Productions { get; }

        void ExecuteSemanticAction(Stack<object> semanticStack, string action);
        Symbol TokenToSymbol(Token token);
        void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token);
        Production FindStartingProduction();
        void RemoveProductionsContainingSymbol(Symbol symbol);
    }

    #endregion

    #region GrammarBase

    public abstract class GrammarBase : IGrammar
    {
        // These members cannot be readonly.
        public HashSet<Symbol> Terminals { get; private set; }
        public HashSet<Symbol> NonTerminals { get; private set; }
        public Symbol StartSymbol { get; private set; }
        public List<Production> Productions { get; private set; }

        protected GrammarBase(Symbol startSymbol)
        {
            Terminals = new HashSet<Symbol>();
            NonTerminals = new HashSet<Symbol>();
            StartSymbol = startSymbol;
            Productions = new List<Production>();
        }

        protected void AddProduction(Symbol lhs, List<object> rhs, int n = 0)
        {
            Productions.Add(new Production(lhs, rhs, n));
        }

        public abstract void ExecuteSemanticAction(Stack<object> semanticStack, string action);

        public virtual Symbol TokenToSymbol(Token token)
        {

            switch (token.TokenType)
            {
                case TokenType.T_EOF: return Symbol.T_EOF;
                case TokenType.T_IntLit: return Symbol.T_IntegerLiteral;
                case TokenType.T_FltLit: return Symbol.T_FloatLiteral;
                case TokenType.T_StrLit: return Symbol.T_StringLiteral;
                case TokenType.T_Ident: return Symbol.T_ID;
                //case TokenType.T_Assign: return Symbol.T_Assign;
                //case TokenType.T_Semicolon: return Symbol.T_Semicolon;
                case TokenType.T_LeftBracket: return Symbol.T_LeftBracket;
                case TokenType.T_RightBracket: return Symbol.T_RightBracket;
                case TokenType.T_Comma: return Symbol.T_Comma;
                case TokenType.T_Plus: return Symbol.T_Plus;
                //case TokenType.T_Minus: return Symbol.T_Minus;

                // Inference only.
                //case TokenType.T_Exclamation: return Symbol.T_Exclamation;
                //case TokenType.T_Variable: return Symbol.T_Variable;
                //case TokenType.T_2OrBar: return Symbol.T_2OrBar;

                default:
                    break;
            }

            throw new Exception(string.Format("No grammar symbol matches token {0} {1}", token.TokenType, token.TokenValue));
        }

        public abstract void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token);

        public Production FindStartingProduction()
        {
            List<Production> results = new List<Production>();

            foreach (Production p in Productions)
            {

                if (p.lhs == StartSymbol)
                {
                    Production p2 = p.StripOutSemanticActions();

                    if (p2.rhs.Count > 0)
                    {
                        object lastObject = p2.rhs[p2.rhs.Count - 1];

                        if (lastObject is Symbol && (Symbol)lastObject == Symbol.T_EOF)
                        {
                            results.Add(p2);
                        }
                    }
                }
            }

            if (results.Count != 1)
            {
                throw new GrammarException(string.Format("GrammarBase.FindStartingProduction() : Expected one starting production; found {0}.", results.Count));
            }

            return results[0];
        }

        public void RemoveProductionsContainingSymbol(Symbol symbol)
        {
            var productionsToRemove = new List<Production>();

            foreach (var production in Productions)
            {

                if (production.ContainsSymbol(symbol))
                {
                    productionsToRemove.Add(production);
                }
            }

            foreach (var production in productionsToRemove)
            {
                Productions.Remove(production);
            }
        }
    }

    #endregion

    #region MicroGrammar

    public class MicroGrammar : GrammarBase
    {
        // The "Micro" grammar from Fischer and LeBlanc

        public MicroGrammar()
            : base(Symbol.N_Start)
        {
            Terminals.Add(Symbol.T_Begin);
            Terminals.Add(Symbol.T_End);
            Terminals.Add(Symbol.T_Assign);
            Terminals.Add(Symbol.T_Semicolon);
            Terminals.Add(Symbol.T_Read);
            Terminals.Add(Symbol.T_Write);
            Terminals.Add(Symbol.T_LeftBracket);
            Terminals.Add(Symbol.T_RightBracket);
            Terminals.Add(Symbol.T_Comma);
            Terminals.Add(Symbol.T_ID);
            Terminals.Add(Symbol.T_IntegerLiteral);
            Terminals.Add(Symbol.T_Plus);
            Terminals.Add(Symbol.T_Minus);
            Terminals.Add(Symbol.T_EOF);

            NonTerminals.Add(Symbol.N_Start);
            NonTerminals.Add(Symbol.N_Program);
            NonTerminals.Add(Symbol.N_StatementList);
            NonTerminals.Add(Symbol.N_StatementTail);
            NonTerminals.Add(Symbol.N_Statement);
            NonTerminals.Add(Symbol.N_IDList);
            NonTerminals.Add(Symbol.N_IDTail);
            NonTerminals.Add(Symbol.N_ExprList);
            NonTerminals.Add(Symbol.N_ExprTail);
            NonTerminals.Add(Symbol.N_Expression);
            NonTerminals.Add(Symbol.N_PrimaryTail);
            NonTerminals.Add(Symbol.N_Primary);
            NonTerminals.Add(Symbol.N_AddOp);

            // See Fischer and LeBlanc, page 113
            Productions.Add(new Production(Symbol.N_Program, new List<object>() { Symbol.T_Begin, Symbol.N_StatementList, Symbol.T_End }, 1)); // 1
            Productions.Add(new Production(Symbol.N_StatementList, new List<object>() { Symbol.N_Statement, Symbol.N_StatementTail }, 2)); // 2
            Productions.Add(new Production(Symbol.N_StatementTail, new List<object>() { Symbol.N_Statement, Symbol.N_StatementTail }, 3)); // 3
            Productions.Add(new Production(Symbol.N_StatementTail, new List<object>() { Symbol.Lambda }, 4)); // 4
            Productions.Add(new Production(Symbol.N_Statement, new List<object>() { Symbol.T_ID, Symbol.T_Assign, Symbol.N_Expression, Symbol.T_Semicolon }, 5)); // 5
            Productions.Add(new Production(Symbol.N_Statement, new List<object>() { Symbol.T_Read, Symbol.T_LeftBracket, Symbol.N_IDList, Symbol.T_RightBracket, Symbol.T_Semicolon }, 6)); // 6
            Productions.Add(new Production(Symbol.N_Statement, new List<object>() { Symbol.T_Write, Symbol.T_LeftBracket, Symbol.N_ExprList, Symbol.T_RightBracket, Symbol.T_Semicolon }, 7)); // 7
            Productions.Add(new Production(Symbol.N_IDList, new List<object>() { Symbol.T_ID, Symbol.N_IDTail }, 8)); // 8
            Productions.Add(new Production(Symbol.N_IDTail, new List<object>() { Symbol.T_Comma, Symbol.T_ID, Symbol.N_IDTail }, 9)); // 9
            Productions.Add(new Production(Symbol.N_IDTail, new List<object>() { Symbol.Lambda }, 10)); // 10
            Productions.Add(new Production(Symbol.N_ExprList, new List<object>() { Symbol.N_Expression, Symbol.N_ExprTail }, 11)); // 11
            Productions.Add(new Production(Symbol.N_ExprTail, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExprTail }, 12)); // 12
            Productions.Add(new Production(Symbol.N_ExprTail, new List<object>() { Symbol.Lambda }, 13)); // 13
            Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Primary, Symbol.N_PrimaryTail }, 14)); // 14
            Productions.Add(new Production(Symbol.N_PrimaryTail, new List<object>() { Symbol.N_AddOp, Symbol.N_Primary, Symbol.N_PrimaryTail }, 15)); // 15
            Productions.Add(new Production(Symbol.N_PrimaryTail, new List<object>() { Symbol.Lambda }, 16)); // 16
            Productions.Add(new Production(Symbol.N_Primary, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.T_RightBracket }, 17)); // 17
            Productions.Add(new Production(Symbol.N_Primary, new List<object>() { Symbol.T_ID }, 18)); // 18
            Productions.Add(new Production(Symbol.N_Primary, new List<object>() { Symbol.T_IntegerLiteral }, 19)); // 19
            Productions.Add(new Production(Symbol.N_AddOp, new List<object>() { Symbol.T_Plus }, 20)); // 20
            Productions.Add(new Production(Symbol.N_AddOp, new List<object>() { Symbol.T_Minus }, 21)); // 21
            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Program, Symbol.T_EOF }, 22)); // 22
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            throw new NotImplementedException("MicroGrammar.ExecuteSemanticAction()");
        }

        public override Symbol TokenToSymbol(Token token)
        {
            string tokenValueAsString = token.TokenValue.ToString();

            switch (token.TokenType)
            {
                case TokenType.T_Ident:

                    switch (tokenValueAsString)
                    {
                        case "begin": return Symbol.T_Begin;
                        case "end": return Symbol.T_End;
                        case "read": return Symbol.T_Read;
                        case "write": return Symbol.T_Write;
                        default: break;
                    }

                    break;

                case TokenType.T_Assign: return Symbol.T_Assign;
                case TokenType.T_Semicolon: return Symbol.T_Semicolon;
                case TokenType.T_Minus: return Symbol.T_Minus;

                default:
                    break;
            }

            return base.TokenToSymbol(token);
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            throw new NotImplementedException("MicroGrammar.PushTokenOntoSemanticStack()");
        }
    }

    #endregion

    #region InferenceGrammar

    public class InferenceGrammar : GrammarBase
    {
        public InferenceGrammar()
            : base(Symbol.N_Start)
        {
            Terminals.Add(Symbol.T_LeftBracket);
            Terminals.Add(Symbol.T_RightBracket);
            Terminals.Add(Symbol.T_Comma);
            Terminals.Add(Symbol.T_ID);
            Terminals.Add(Symbol.T_BoolID);
            Terminals.Add(Symbol.T_SkolemID);
            Terminals.Add(Symbol.T_IntegerLiteral);
            Terminals.Add(Symbol.T_StringLiteral);
            Terminals.Add(Symbol.T_Variable);
            Terminals.Add(Symbol.T_Exclamation);
            Terminals.Add(Symbol.T_2Ampersand);
            Terminals.Add(Symbol.T_2OrBar);
            Terminals.Add(Symbol.T_Arrow);
            Terminals.Add(Symbol.T_ForAll);
            Terminals.Add(Symbol.T_Exists);
            Terminals.Add(Symbol.T_EOF);

            NonTerminals.Add(Symbol.N_Start);
            NonTerminals.Add(Symbol.N_BoolExpr);
            NonTerminals.Add(Symbol.N_BoolExprTail);
            NonTerminals.Add(Symbol.N_JunctionTail);
            NonTerminals.Add(Symbol.N_BoolAtom);
            NonTerminals.Add(Symbol.N_ArgumentList);
            NonTerminals.Add(Symbol.N_ArgumentTail);
            NonTerminals.Add(Symbol.N_Argument);
            NonTerminals.Add(Symbol.N_ArgIdentTail);

            Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_BoolExpr, Symbol.T_EOF }, 1));
            Productions.Add(new Production(Symbol.N_BoolExpr, new List<object>() { Symbol.N_BoolAtom, Symbol.N_BoolExprTail }, 2));
            Productions.Add(new Production(Symbol.N_BoolExprTail, new List<object>() { Symbol.N_JunctionTail, Symbol.N_BoolExprTail }, 3));
            Productions.Add(new Production(Symbol.N_JunctionTail, new List<object>() { Symbol.T_2Ampersand, Symbol.N_BoolAtom, "#makeConj" }, 4));
            Productions.Add(new Production(Symbol.N_JunctionTail, new List<object>() { Symbol.T_2OrBar, Symbol.N_BoolAtom, "#makeDisj" }, 5));
            Productions.Add(new Production(Symbol.N_BoolExprTail, new List<object>() { Symbol.T_Arrow, Symbol.N_BoolAtom, "#makeArrow" }, 6));
            Productions.Add(new Production(Symbol.N_BoolExprTail, new List<object>() { Symbol.Lambda }, 7));
            Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_LeftBracket, Symbol.N_BoolExpr, Symbol.T_RightBracket }, 8));
            Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_Exclamation, Symbol.N_BoolAtom, "#makeNeg" }, 9));
            Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_ForAll, Symbol.T_Variable, Symbol.T_LeftBracket, Symbol.N_BoolExpr, Symbol.T_RightBracket, "#makeForAll" }, 10));
            Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_Exists, Symbol.T_Variable, Symbol.T_LeftBracket, Symbol.N_BoolExpr, Symbol.T_RightBracket, "#makeExists" }, 11));
            Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_BoolID, Symbol.T_LeftBracket, Symbol.N_ArgumentList, Symbol.T_RightBracket, "#makePred" }, 12));
            Productions.Add(new Production(Symbol.N_ArgumentList, new List<object>() { Symbol.N_Argument, Symbol.N_ArgumentTail, "#makeArgList" }, 13));
            Productions.Add(new Production(Symbol.N_ArgumentList, new List<object>() { Symbol.Lambda, "#null" }, 14));
            Productions.Add(new Production(Symbol.N_ArgumentTail, new List<object>() { Symbol.T_Comma, Symbol.N_Argument, Symbol.N_ArgumentTail, "#makeArgList" }, 15));
            Productions.Add(new Production(Symbol.N_ArgumentTail, new List<object>() { Symbol.Lambda, "#null" }, 16));
            Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_IntegerLiteral }, 17));
            Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_StringLiteral }, 18));
            Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_Variable }, 19));
            Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_SkolemID, Symbol.T_LeftBracket, Symbol.N_ArgumentList, Symbol.T_RightBracket, "#makeSkolemFunc" }, 20));
            Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_ID, Symbol.N_ArgIdentTail }, 21));
            Productions.Add(new Production(Symbol.N_ArgIdentTail, new List<object>() { Symbol.T_LeftBracket, Symbol.N_ArgumentList, Symbol.T_RightBracket, "#makeFunc" }, 22));
            Productions.Add(new Production(Symbol.N_ArgIdentTail, new List<object>() { Symbol.Lambda }, 23));
        }

        public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
        {
            Variable v = null;
            ArgumentList argList = null;
            IBooleanExpression expr1 = null;
            IBooleanExpression expr2 = null;

            switch (action)
            {
                case "#null":
                    //Console.WriteLine("Semantic stack: pushing null...");
                    semanticStack.Push(null);
                    break;

                case "#makeArgList":
                    //Console.WriteLine("Semantic stack: making argument list...");

                    ArgumentList argList2 = semanticStack.Pop() as ArgumentList;
                    IArgument arg1 = semanticStack.Pop() as IArgument;

                    semanticStack.Push(new ArgumentList(arg1, argList2));
                    break;

                case "#makeFunc":
                    //Console.WriteLine("Semantic stack: making function...");
                    argList = semanticStack.Pop() as ArgumentList;

                    Constant c = semanticStack.Pop() as Constant;

                    semanticStack.Push(new Function(c, argList));
                    break;

                case "#makeSkolemFunc":
                    //Console.WriteLine("Semantic stack: making Skolem function...");
                    argList = semanticStack.Pop() as ArgumentList;

                    string name = semanticStack.Pop() as string;

                    semanticStack.Push(new SkolemFunction(name, argList));
                    break;

                case "#makePred":
                    //Console.WriteLine("Semantic stack: making predicate...");
                    argList = semanticStack.Pop() as ArgumentList;

                    BooleanConstant bc = semanticStack.Pop() as BooleanConstant;

                    semanticStack.Push(new Predicate(bc, argList));
                    break;

                case "#makeNeg":
                    //Console.WriteLine("Semantic stack: making negation...");
                    expr1 = semanticStack.Pop() as IBooleanExpression;
                    semanticStack.Push(new Negation(expr1));
                    break;

                case "#makeConj":
                    //Console.WriteLine("Semantic stack: making conjunction...");
                    expr2 = semanticStack.Pop() as IBooleanExpression;
                    expr1 = semanticStack.Pop() as IBooleanExpression;
                    semanticStack.Push(new Conjunction(expr1, expr2));
                    break;

                case "#makeDisj":
                    //Console.WriteLine("Semantic stack: making disjunction...");
                    expr2 = semanticStack.Pop() as IBooleanExpression;
                    expr1 = semanticStack.Pop() as IBooleanExpression;
                    semanticStack.Push(new Disjunction(expr1, expr2));
                    break;

                case "#makeArrow":
                    //Console.WriteLine("Semantic stack: making arrow...");
                    expr2 = semanticStack.Pop() as IBooleanExpression;
                    expr1 = semanticStack.Pop() as IBooleanExpression;
                    semanticStack.Push(new Disjunction(new Negation(expr1), expr2));
                    break;

                case "#makeForAll":
                    //Console.WriteLine("Semantic stack: making for all...");
                    expr1 = semanticStack.Pop() as IBooleanExpression;
                    v = semanticStack.Pop() as Variable;
                    semanticStack.Push(new ForAll(v, expr1));
                    break;

                case "#makeExists":
                    //Console.WriteLine("Semantic stack: making exists...");
                    expr1 = semanticStack.Pop() as IBooleanExpression;
                    v = semanticStack.Pop() as Variable;
                    semanticStack.Push(new Exists(v, expr1));
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

                    switch (tokenValueAsString)
                    {
                        case "A": return Symbol.T_ForAll;
                        case "E": return Symbol.T_Exists;
                        default: break;
                    }

                    break;

                case TokenType.T_BoolIdent: return Symbol.T_BoolID;
                case TokenType.T_SkolemIdent: return Symbol.T_SkolemID;
                case TokenType.T_Exclamation: return Symbol.T_Exclamation;
                case TokenType.T_Variable: return Symbol.T_Variable;
                case TokenType.T_2Ampersand: return Symbol.T_2Ampersand;
                case TokenType.T_2OrBar: return Symbol.T_2OrBar;
                case TokenType.T_Arrow: return Symbol.T_Arrow;
                case TokenType.T_StrLit: return Symbol.T_StringLiteral;
                default: break;
            }

            return base.TokenToSymbol(token);
        }

        public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
        {
            var value = token.TokenValue;

            switch (tokenAsSymbol)
            {
                case Symbol.T_ID:
                    semanticStack.Push(new Constant(value as string));
                    //Console.WriteLine("Pushed Constant {0} onto the semantic stack.", value);
                    break;

                case Symbol.T_IntegerLiteral:
                    semanticStack.Push(new IntegerLiteral(value));
                    //Console.WriteLine("Pushed IntegerLiteral {0} onto the semantic stack.", value);
                    break;

                case Symbol.T_StringLiteral:
                    semanticStack.Push(new StringLiteral(value));
                    //Console.WriteLine("Pushed StringLiteral {0} onto the semantic stack.", value);
                    break;

                case Symbol.T_BoolID:
                    semanticStack.Push(new BooleanConstant(value as string));
                    //Console.WriteLine("Pushed BooleanConstant {0} onto the semantic stack.", value);
                    break;

                case Symbol.T_SkolemID:
                    //semanticStack.Push(new SkolemConstant(value as string));
                    semanticStack.Push(value as string);
                    //Console.WriteLine("Pushed SkolemID {0} onto the semantic stack as a string.", value);
                    break;

                case Symbol.T_Variable:
                    semanticStack.Push(new Variable(value as string));
                    //Console.WriteLine("Pushed Variable {0} onto the semantic stack.", value);
                    break;

                default:
                    break;
            }
        }
    }

    #endregion

    #region GrammarSelector

    public enum GrammarSelector
    {
        Micro,
        Inference,
        InterpreterChapter1,
        LISP,
        APL,
        Scheme,
        SASL,
        CLU,
        Smalltalk, 
        Prolog,
        Prolog2,    // "Real" Prolog.
        JSON
    }

    #endregion

    #region GrammarFactory

    static public class GrammarFactory
    {
        static public IGrammar Create(GrammarSelector gs)
        {

            switch (gs)
            {
                case GrammarSelector.Micro:
                    return new MicroGrammar();

                case GrammarSelector.Inference:
                    return new InferenceGrammar();

                case GrammarSelector.InterpreterChapter1:
                    return new Inference.Interpreter.Chapter1.Grammar();

                case GrammarSelector.LISP:
                    return new Inference.Interpreter.LISP.LISPGrammar();

                case GrammarSelector.APL:
                    return new Inference.Interpreter.APL.APLGrammar();

                case GrammarSelector.Scheme:
                    return new Inference.Interpreter.Scheme.SchemeGrammar();

                case GrammarSelector.SASL:
                    return new Inference.Interpreter.SASL.SASLGrammar();

                case GrammarSelector.CLU:
                    return new Inference.Interpreter.CLU.CLUGrammar();

                case GrammarSelector.Smalltalk:
                    return new Inference.Interpreter.Smalltalk.SmalltalkGrammar();

                case GrammarSelector.Prolog:
                    return new Inference.Interpreter.Prolog.PrologGrammar();

                case GrammarSelector.Prolog2:
#if USE_PROLOG2_LL1
                    return new Inference.Interpreter.Prolog.PrologGrammar2_LL1();
#else
                    return new Inference.Interpreter.Prolog.PrologGrammar2();
#endif

                case GrammarSelector.JSON:
                    return new Inference.Interpreter.JSON.JSONGrammar();

                default:
                    throw new ArgumentException("GrammarFactory.Create() : Unrecognized GrammarSelector: " + gs.ToString(), "gs");
            }
        }
    }

    #endregion
}
