using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;

namespace Inference.Parser
{
    #region TokenType

    public enum TokenType
    {
        T_IntLit,		        // Integer literal
        T_FltLit,			    // Floating-point literal
        T_StrLit,			    // String literal
        T_StrLit2,			    // String literal, type 2 (i.e. a different string delimiter character is used)
        T_Ident,			    // Identifier
        T_BoolIdent,		    // Boolean identifier, beginning with @ ; e.g. @id
        T_SkolemIdent,          // Skolem identifier (non-Boolean), beginning with $ ; e.g. $S1
        T_Variable,             // E.g. ?var
        T_Mult,			        // *
        T_Div,			        // /
        T_Plus,			        // +
        T_Minus,			    // -
        T_Equal,			    // =
        T_NotEqual,		        // /=
        T_Less,			        // <
        T_LessEqual,		    // <=
        T_Greater,			    // >
        T_GreaterEqual,		    // >=
        T_Semicolon,		    // ;
        T_Comma,			    // ,
        T_2OrBar,			    // ||
        T_2Ampersand,		    // &&
        T_LeftBracket,		    // (
        T_RightBracket,		    // )
        T_Assign,			    // :=
        T_Arrow,			    // ->
        T_Exclamation,          // !
        T_Apostrophe,           // '
        T_Octothorpe,           // #
        T_Dollar,               // $
        T_Dot,			        // .
        T_LeftSquareBracket,	// [
        T_RightSquareBracket,	// ]
        T_OrBar,			    // |
        T_Colon,                // : (for JSON and Prolog)
        T_LeftCurlyBrace,       // { (for JSON and Prolog)
        T_RightCurlyBrace,      // } (for JSON and Prolog)
        T_QuoteKeyword,         // quote
        T_QuestionMinus,
        T_ColonMinus,
        T_EqualLessThan,
        T_BackslashPlus,
        T_BackslashEqual,
        T_EqualEqual,
        T_BackslashEqualEqual,
        T_EqualColonEqual,
        T_EqualBackslashEqual,
        T_MinusMinusGreaterThan,
        T_EqualDotDot,
        T_Caret,                // ^
        T_EOF,			        // End of file (no errors detected)
        //T_EOF_Error,            // End of file (one or more errors detected)
        //T_APOST,			    /* ' */
        //T_2DOT,			    /* .. */
        //T_POWER,			    /* ** */
        //EndOfT,

        // Non-accept (intermediate) states
        S_Start,			    // Start state
        S_Error,
        S_StrLitOpen,
        S_StrLit2Open,
        S_IntLitDot,
        S_Question,             // Question mark
        S_Ampersand, 		    // &
        S_At,                   // @
        S_Dollar,               // $
        S_Backslash,
        S_EqualColon,
        S_EqualBackslash,
        S_MinusMinus,
        S_EqualDot
    }

    #endregion

    #region TokenizerTableKey

    public class TokenizerTableKey
    {
        public readonly TokenType state;
        public readonly char c;

        public TokenizerTableKey(TokenType stateParam, char cParam)
        {
            state = stateParam;
            c = cParam;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            var otherKey = obj as TokenizerTableKey;

            return otherKey != null && state == otherKey.state && c == otherKey.c;
        }

        public override int GetHashCode()
        {
            return state.GetHashCode() * 257 + c.GetHashCode();
        }
    }

    #endregion

    #region Token

    public class Token
    {
        public readonly TokenType TokenType;
        public readonly object TokenValue;
        public readonly int Line;
        public readonly int Column;
        public readonly bool IsQuoted;

        public Token(TokenType tt, object tv, int line, int col, bool isQuoted = false)
        {
            TokenType = tt;
            TokenValue = tv;
            Line = line;
            Column = col;
            IsQuoted = isQuoted;
        }

        public Token CloneWithNewLineNumber(int line)
        {
            return new Token(TokenType, TokenValue, line, Column, IsQuoted);
        }
    }

    #endregion

    #region ITokenizer

    public interface ITokenizer
    {
        List<Token> Tokenize(string str);
    }

    #endregion

    #region TokenizerException

    public class TokenizerException : Exception
    {
        public readonly int Line;
        public readonly int Column;

        public TokenizerException(string message, int line, int col)
            : base(message)
        {
            Line = line;
            Column = col;
        }
    }

    #endregion

    #region TokenizerBase

    public abstract class TokenizerBase : ITokenizer
    {
        protected abstract void SetInputString(string str);

        protected abstract Token GetToken();

        public List<Token> Tokenize(string str)
        {
            var tokenList = new List<Token>();
            Token token = null;

            SetInputString(str);

            do
            {
                token = GetToken();
                tokenList.Add(token);
            }
            while (token.TokenType != TokenType.T_EOF);

            return tokenList;
        }
    }

    #endregion

    #region FSMTokenizer

    // A tokenizer based on a finite state machine.

    public class FSMTokenizer : TokenizerBase
    {
        protected readonly GrammarSelector gs;
        protected readonly HashSet<TokenType> acceptableTokens = new HashSet<TokenType>();
        private readonly Dictionary<TokenizerTableKey, TokenType> table = new Dictionary<TokenizerTableKey, TokenType>();
        private string m_str = null;   // The string to be tokenized.
        private readonly StringBuilder sbToken = new StringBuilder();
        //private bool was_err = false;   // Set to true if lexical error occurs
        private int line_num = 1;		// Current line number
	    private int col_num = 1;        // Current column number
        private int char_num = 0;       // Current index into m_str
        protected char cStringDelimiter = '"';  // This must be the delimiter for TokenType.T_StrLit
        protected readonly Dictionary<TokenType, char> dictInternalStringStateToDelimiter = new Dictionary<TokenType, char>();
        protected readonly Dictionary<TokenType, TokenType> dictInternalStringStateToCompletedState = new Dictionary<TokenType, TokenType>();
        protected bool removeComments = false;
        protected char cCommentDelimiter = '#';

        public FSMTokenizer(GrammarSelector gs)
        {
            this.gs = gs;

            dictInternalStringStateToDelimiter[TokenType.S_StrLitOpen] = cStringDelimiter;
            dictInternalStringStateToCompletedState[TokenType.S_StrLitOpen] = TokenType.T_StrLit;

            acceptableTokens.Add(TokenType.T_IntLit);
            acceptableTokens.Add(TokenType.T_FltLit);
            acceptableTokens.Add(TokenType.T_StrLit);
            acceptableTokens.Add(TokenType.T_Ident);
            acceptableTokens.Add(TokenType.T_Mult);
            acceptableTokens.Add(TokenType.T_Div);
            acceptableTokens.Add(TokenType.T_Plus);
            acceptableTokens.Add(TokenType.T_Minus);
            acceptableTokens.Add(TokenType.T_Equal);
            acceptableTokens.Add(TokenType.T_NotEqual);
            acceptableTokens.Add(TokenType.T_Less);
            //acceptableTokens.Add(TokenType.T_LessEqual); // In Prolog, it's =<, not <=
            acceptableTokens.Add(TokenType.T_Greater);
            acceptableTokens.Add(TokenType.T_GreaterEqual);
            acceptableTokens.Add(TokenType.T_Semicolon);
            acceptableTokens.Add(TokenType.T_Comma);
            acceptableTokens.Add(TokenType.T_LeftBracket);
            acceptableTokens.Add(TokenType.T_RightBracket);
            acceptableTokens.Add(TokenType.T_Arrow);
            acceptableTokens.Add(TokenType.T_EOF);

            AddTransition(TokenType.S_Start, 'A', TokenType.T_Ident);
            AddTransition(TokenType.S_Start, '0', TokenType.T_IntLit);
            AddTransition(TokenType.S_Start, cStringDelimiter, TokenType.S_StrLitOpen);
            AddTransition(TokenType.S_Start, '*', TokenType.T_Mult);
            AddTransition(TokenType.S_Start, '/', TokenType.T_Div);
            AddTransition(TokenType.S_Start, '+', TokenType.T_Plus);
            AddTransition(TokenType.S_Start, '-', TokenType.T_Minus);
            AddTransition(TokenType.S_Start, '=', TokenType.T_Equal);
            AddTransition(TokenType.S_Start, '<', TokenType.T_Less);
            AddTransition(TokenType.S_Start, '>', TokenType.T_Greater);
            AddTransition(TokenType.S_Start, ':', TokenType.T_Colon);
            AddTransition(TokenType.S_Start, ';', TokenType.T_Semicolon);
            AddTransition(TokenType.S_Start, ',', TokenType.T_Comma);
            AddTransition(TokenType.S_Start, '|', TokenType.T_OrBar);
            AddTransition(TokenType.S_Start, '(', TokenType.T_LeftBracket);
            AddTransition(TokenType.S_Start, ')', TokenType.T_RightBracket);
            AddTransition(TokenType.T_Ident, 'A', TokenType.T_Ident);
            AddTransition(TokenType.T_Ident, '0', TokenType.T_Ident);
            AddTransition(TokenType.T_Ident, '_', TokenType.T_Ident);
            AddTransition(TokenType.T_IntLit, '0', TokenType.T_IntLit);
            AddTransition(TokenType.T_IntLit, '.', TokenType.S_IntLitDot);
            AddTransition(TokenType.S_IntLitDot, '0', TokenType.T_FltLit);
            AddTransition(TokenType.T_FltLit, '0', TokenType.T_FltLit);
            AddTransition(TokenType.S_StrLitOpen, cStringDelimiter, TokenType.T_StrLit);
            AddTransition(TokenType.T_StrLit, cStringDelimiter, TokenType.S_StrLitOpen);
            AddTransition(TokenType.T_Minus, '0', TokenType.T_IntLit);
            AddTransition(TokenType.T_Minus, '>', TokenType.T_Arrow);
            AddTransition(TokenType.T_Greater, '=', TokenType.T_GreaterEqual);
        }

        protected override void SetInputString(string str)
        {
            m_str = str;
            line_num = 1;
            col_num = 1;
            char_num = 0;
        }

        protected void AddTransition(TokenType oldState, char c, TokenType newState)
        {
            var key = new TokenizerTableKey(oldState, c);

            if (table.ContainsKey(key))
            {
                throw new Exception(string.Format("Tokenizer.AddTransition() : The key [{0}, {1}] already exists in the transition table", oldState, c));
            }

            table[key] = newState;
        }

        private char GetChar()
        {

            if (m_str == null)
            {
                throw new Exception("Tokenizer.GetChar() : The input string is null.  SetInputString() must be called first.");
            }

            if (char_num >= 0 && char_num < m_str.Length)
            {
                return m_str[char_num];
            }

            return '\0';
        }

        private TokenType RecoverToken(List<TokenType> state_list)
        {

            for (var i = state_list.Count - 1; i > 0; i--)
            {

                if (acceptableTokens.Contains(state_list[i]))
                {
                    // Keep only the first i characters of token_str.
                    var newTokenStr = sbToken.ToString().Substring(0, i);

                    sbToken.Clear();
                    sbToken.Append(newTokenStr);
                    // We do not need to trim the end of the state_list.
	                return state_list[i];
	            }
            }

            return TokenType.S_Error;
        }

        protected string GetStrLitFromTokenStr(char cDelimiter)
        {
            var token_str = sbToken.ToString();

            if (string.IsNullOrEmpty(token_str) || token_str[0] != cDelimiter)
            {
                throw new Exception("Tokenizer.GetStrLitFromTokenStr() : Token string error");
            }

            var insideQuotes = true;
            var sb = new StringBuilder();

            for (var i = 1; i < token_str.Length; ++i)
            {
                var c = token_str[i];

                // This logic allows two consecutive string delimiters within the string to be interpreted as a single literal string delimiter.
                if (c != cDelimiter || !insideQuotes)
                {
                    sb.Append(c);
                    insideQuotes = true;
                }
                else
                {
                    insideQuotes = false;
                }
            }

            return sb.ToString();
        }

        protected virtual object ExtendedGetTokenValue(ref TokenType s, string token_str)
        {
            return null;
        }

        // Return the next valid token read from the specified file stream.
        // Read buffered characters before reading new characters from fp.

        protected override Token GetToken()
        {
            var state_list = new List<TokenType>();     // List of states corresponding to not-yet-accepted characters
            char c;                                     // A character read from get_char()
	        var start_col = col_num;		            // The column of the first char in a token
            var s = TokenType.S_Start;                  // Current state

            sbToken.Clear();
            state_list.Add(s);

            for ( ; ; ) 	// Loop until valid token read
            {
                char cSimplified;
                
                c = GetChar();

                if (char.IsLetter(c))
                {
                    cSimplified = 'A';
                }
                else if (char.IsDigit(c))
                {
                    cSimplified = '0';
                }
                else
                {
                    cSimplified = c;
                }

	            if (s != TokenType.S_Start  &&  ( c == '\0'  ||  c == '\n'  ||
                    (char.IsWhiteSpace(c) && !dictInternalStringStateToCompletedState.ContainsKey(s))))
                {

                    if (dictInternalStringStateToCompletedState.ContainsKey(s) && (c == '\0' || c == '\n'))   // Newline or EOF delimits string literal
                    {
                        s = dictInternalStringStateToCompletedState[s];
                    }

                    if (c == '\n')
                    {
                        ++char_num;
                    }

                    if (acceptableTokens.Contains(s))
                    {
		                // Valid token has been delimited by white space
		                break;
	                }
                    else
                    {		
                        // Error - try to recover
		                s = TokenType.S_Error;
	                }
	            }

                TokenType newState;

                if (s == TokenType.S_Start && c == '\0')
                {
                    newState = TokenType.T_EOF;
                }
                else if (s == TokenType.S_Start && c == '\n')
                {
                    ++line_num;
                    col_num = 1;
                    start_col = 1;    // Don't buffer white space
                    newState = s; // TokenType.S_Start;
                }
                else if (s == TokenType.S_Start && char.IsWhiteSpace(c))
                {
                    ++start_col;    // Don't buffer white space
                    newState = s; // TokenType.S_Start;
                }
                else if (s == TokenType.S_Start && removeComments && c == cCommentDelimiter)
                {

                    for (; ; )
                    {
                        ++char_num;
                        c = GetChar();

                        if (c == '\0')
                        {
                            newState = TokenType.T_EOF;
                            break;
                        }
                        else if (c == '\n')
                        {
                            ++line_num;
                            col_num = 1;
                            start_col = 1;    // Don't buffer white space
                            newState = s; // TokenType.S_Start;
                            break;
                        }
                    }
                }
#if DEAD_CODE
	            else if (s == TokenType.S_StrLitOpen && c != cStringDelimiter)
                {
                    newState = s;
		        }
#else
                else if (dictInternalStringStateToDelimiter.ContainsKey(s) && c != dictInternalStringStateToDelimiter[s])
                {
                    newState = s;
                }
#endif
                else
                {
                    var key = new TokenizerTableKey(s, cSimplified);

                    if (table.ContainsKey(key))
                    {
                        newState = table[key];
                    }
                    else
                    {
                        newState = TokenType.S_Error;
                    }
                }

                s = newState;

	            if (s == TokenType.T_EOF)
                {
	                break;
	            }
                else if (s == TokenType.S_Error)
                {
                    s = RecoverToken(state_list);

                    if (s != TokenType.S_Error)
                    {
                        var rewindAmount = col_num - (start_col + sbToken.Length);

                        char_num -= rewindAmount;
                        col_num = start_col + sbToken.Length;
                        break;	// Valid token recovered
                    }

                    throw new TokenizerException(string.Format("Lexical error at line {0}, column {1}", line_num, start_col), line_num, start_col);
                    /*
                    Console.WriteLine("Discarding unmatched '{0}' at column {1}", m_str[start_col], start_col);
                    was_err = true;
                    strlit.Clear();
                    ++start_col;
                    col_num = start_col;
                    token_str.Clear();
                    s = TokenType.S_Start;		// Start over at next unmatched char
                    state_list.Clear();
                    state_list.Add(s);
                     */
                }
                else
                {
                    ++char_num;

                    if (c != '\n')
                    {
                        ++col_num;
                    }

                    if (s != TokenType.S_Start)
                    {
                        sbToken.Append(c);
                        state_list.Add(s);
                    }
                }
            }

            if (!acceptableTokens.Contains(s))
            {
                throw new TokenizerException(
                    string.Format("Internal error at line {0}, column {1}: Non-token {2} accepted", line_num, start_col, s),
                    line_num, start_col);
            }

            //Console.WriteLine("Token: {0} ; Line {1}; Column: {2}", sbToken.ToString(), line_num, start_col);

            var token_str = sbToken.ToString();
            object tokenValue;

            switch (s)
            {
                case TokenType.T_IntLit:
                    tokenValue = int.Parse(token_str);
                    break;

                case TokenType.T_FltLit:
                    tokenValue = double.Parse(token_str);
                    break;

                case TokenType.T_StrLit:
                    tokenValue = GetStrLitFromTokenStr(cStringDelimiter);
                    break;

                case TokenType.T_EOF:
                    tokenValue = "EOF";
                    break;

                default:
                    tokenValue = ExtendedGetTokenValue(ref s, token_str); // TODO: Pass s by reference so that the Prolog interpreter can change it in the case of single-quoted strings -> identifiers.

                    if (tokenValue == null)
                    {
                        tokenValue = token_str.ToString();
                    }

                    break;
            }

            var token = new Token(s, tokenValue, line_num, start_col);

            if (c == '\n')
            {
                ++line_num;
                col_num = 1;
            }

            return token;
        }
    }

    #endregion

    #region Tokenizer

    // The tokenizer for the Micro and Inference languages.

    public class Tokenizer : FSMTokenizer
    {
        public Tokenizer(GrammarSelector gs)
            : base(gs)
        {
            acceptableTokens.Add(TokenType.T_Assign);   // := is a Micro token.

            AddTransition(TokenType.T_Colon, '=', TokenType.T_Assign);

            if (gs == GrammarSelector.Inference)
            {
                acceptableTokens.Add(TokenType.T_BoolIdent);
                acceptableTokens.Add(TokenType.T_SkolemIdent);
                acceptableTokens.Add(TokenType.T_2OrBar);
                acceptableTokens.Add(TokenType.T_2Ampersand);
                acceptableTokens.Add(TokenType.T_Exclamation);
                acceptableTokens.Add(TokenType.T_Variable);
                acceptableTokens.Add(TokenType.T_LessEqual);

                AddTransition(TokenType.S_Start, '&', TokenType.S_Ampersand);
                AddTransition(TokenType.S_Start, '?', TokenType.S_Question);
                AddTransition(TokenType.S_Start, '!', TokenType.T_Exclamation);
                AddTransition(TokenType.S_Start, '$', TokenType.S_Dollar);
                AddTransition(TokenType.S_Dollar, 'A', TokenType.T_SkolemIdent);
                AddTransition(TokenType.T_SkolemIdent, 'A', TokenType.T_SkolemIdent);
                AddTransition(TokenType.T_SkolemIdent, '0', TokenType.T_SkolemIdent);
                AddTransition(TokenType.T_SkolemIdent, '_', TokenType.T_SkolemIdent);
                AddTransition(TokenType.T_Less, '=', TokenType.T_LessEqual);
                AddTransition(TokenType.S_Question, 'A', TokenType.T_Variable);
                AddTransition(TokenType.T_Variable, 'A', TokenType.T_Variable);
                AddTransition(TokenType.T_Variable, '0', TokenType.T_Variable);
                AddTransition(TokenType.T_Variable, '_', TokenType.T_Variable);
                AddTransition(TokenType.T_OrBar, '|', TokenType.T_2OrBar);
                AddTransition(TokenType.S_Ampersand, '&', TokenType.T_2Ampersand);
                AddTransition(TokenType.S_Start, '@', TokenType.S_At);
                AddTransition(TokenType.S_At, 'A', TokenType.T_BoolIdent);
                AddTransition(TokenType.T_BoolIdent, 'A', TokenType.T_BoolIdent);
                AddTransition(TokenType.T_BoolIdent, '0', TokenType.T_BoolIdent);
                AddTransition(TokenType.T_BoolIdent, '_', TokenType.T_BoolIdent);
            }
        }

        protected override object ExtendedGetTokenValue(ref TokenType s, string token_str)
        {

            if (gs != GrammarSelector.Inference)
            {
                return null;
            }

            switch (s)
            {
                case TokenType.T_BoolIdent:
                case TokenType.T_SkolemIdent:
                case TokenType.T_Variable:
                    return token_str.Substring(1);     // Omit the @ or $ or ? at the beginning.

                default:
                    return null;
            }
        }
    }

    #endregion

    #region Prolog2Tokenizer

    // The tokenizer for standard Prolog.
    // We need a finite state machine tokenizer to tokenize strings such as "=.." vs. "=" and ".".

    public class Prolog2Tokenizer : FSMTokenizer
    {
        private const char cStringDelimiter2 = '\'';

        public Prolog2Tokenizer()
            : base(GrammarSelector.Prolog2)
        {
            dictInternalStringStateToDelimiter[TokenType.S_StrLit2Open] = cStringDelimiter2;
            dictInternalStringStateToCompletedState[TokenType.S_StrLit2Open] = TokenType.T_StrLit2;

            removeComments = true;
            cCommentDelimiter = '%';

            acceptableTokens.Add(TokenType.T_StrLit2);
            acceptableTokens.Add(TokenType.T_Colon);
            acceptableTokens.Add(TokenType.T_QuestionMinus);
            acceptableTokens.Add(TokenType.T_ColonMinus);
            acceptableTokens.Add(TokenType.T_EqualLessThan);
            acceptableTokens.Add(TokenType.T_BackslashPlus);
            acceptableTokens.Add(TokenType.T_BackslashEqual);
            acceptableTokens.Add(TokenType.T_EqualEqual);
            acceptableTokens.Add(TokenType.T_BackslashEqualEqual);
            acceptableTokens.Add(TokenType.T_EqualColonEqual);
            acceptableTokens.Add(TokenType.T_EqualBackslashEqual);
            acceptableTokens.Add(TokenType.T_MinusMinusGreaterThan);
            acceptableTokens.Add(TokenType.T_EqualDotDot);
            acceptableTokens.Add(TokenType.T_LeftSquareBracket);
            acceptableTokens.Add(TokenType.T_RightSquareBracket);
            acceptableTokens.Add(TokenType.T_LeftCurlyBrace);
            acceptableTokens.Add(TokenType.T_RightCurlyBrace);
            acceptableTokens.Add(TokenType.T_OrBar);
            acceptableTokens.Add(TokenType.T_Dot);
            acceptableTokens.Add(TokenType.T_Exclamation);
            acceptableTokens.Add(TokenType.T_Caret);

            AddTransition(TokenType.S_Start, cStringDelimiter2, TokenType.S_StrLit2Open);
            AddTransition(TokenType.S_StrLit2Open, cStringDelimiter2, TokenType.T_StrLit2);
            AddTransition(TokenType.T_StrLit2, cStringDelimiter2, TokenType.S_StrLit2Open);
            AddTransition(TokenType.S_Start, '?', TokenType.S_Question);
            AddTransition(TokenType.S_Question, '-', TokenType.T_QuestionMinus);
            AddTransition(TokenType.T_Colon, '-', TokenType.T_ColonMinus);
            AddTransition(TokenType.T_Equal, '<', TokenType.T_EqualLessThan);
            AddTransition(TokenType.S_Start, '\\', TokenType.S_Backslash);
            AddTransition(TokenType.S_Backslash, '+', TokenType.T_BackslashPlus);
            AddTransition(TokenType.S_Backslash, '=', TokenType.T_BackslashEqual);
            AddTransition(TokenType.T_Equal, '=', TokenType.T_EqualEqual);
            AddTransition(TokenType.T_BackslashEqual, '=', TokenType.T_BackslashEqualEqual);
            AddTransition(TokenType.T_Equal, ':', TokenType.S_EqualColon);
            AddTransition(TokenType.S_EqualColon, '=', TokenType.T_EqualColonEqual);
            AddTransition(TokenType.T_Equal, '\\', TokenType.S_EqualBackslash);
            AddTransition(TokenType.S_EqualBackslash, '=', TokenType.T_EqualBackslashEqual);
            AddTransition(TokenType.T_Minus, '-', TokenType.S_MinusMinus);
            AddTransition(TokenType.S_MinusMinus, '>', TokenType.T_MinusMinusGreaterThan);
            AddTransition(TokenType.T_Equal, '.', TokenType.S_EqualDot);
            AddTransition(TokenType.S_EqualDot, '.', TokenType.T_EqualDotDot);
            AddTransition(TokenType.S_Start, '[', TokenType.T_LeftSquareBracket);
            AddTransition(TokenType.S_Start, ']', TokenType.T_RightSquareBracket);
            AddTransition(TokenType.S_Start, '{', TokenType.T_LeftCurlyBrace);
            AddTransition(TokenType.S_Start, '}', TokenType.T_RightCurlyBrace);
            AddTransition(TokenType.S_Start, '.', TokenType.T_Dot);
            AddTransition(TokenType.S_Start, '!', TokenType.T_Exclamation);
            AddTransition(TokenType.S_Start, '_', TokenType.T_Ident);       // For non-binding variables.
            AddTransition(TokenType.S_Start, '^', TokenType.T_Caret);
            //AddTransition(TokenType., '', TokenType.);
        }

        protected override object ExtendedGetTokenValue(ref TokenType s, string token_str)
        {

            switch (s)
            {
                case TokenType.T_StrLit2:
                    //s = TokenType.T_Ident;  // In standard Prolog, single-quoted strings are tokenized as identifiers (except that e.g. 'V' is a functor, not a variable).
                    return GetStrLitFromTokenStr('\'');

                default:
                    return null;
            }
        }
    }

    #endregion

    #region TokenizerFactory

    static public class TokenizerFactory
    {
        static public ITokenizer Create(GrammarSelector gs)
        {

            switch (gs)
            {
                case GrammarSelector.Micro:
                case GrammarSelector.Inference:
                    return new Tokenizer(gs);

                case GrammarSelector.InterpreterChapter1:
                case GrammarSelector.LISP:
                case GrammarSelector.APL:
                case GrammarSelector.Scheme:
                case GrammarSelector.SASL:
                case GrammarSelector.CLU:
                case GrammarSelector.Smalltalk:
                case GrammarSelector.Prolog:
                case GrammarSelector.JSON:
                    return new Inference.Interpreter.InterpreterTokenizer(gs);

                case GrammarSelector.Prolog2:
#if DEAD_CODE
                    return new Inference.Interpreter.InterpreterTokenizer(gs);
#else
                    return new Prolog2Tokenizer();
#endif

                default:
                    break;
            }

            throw new ArgumentException("TokenizerFactory.Create() : Unrecognized GrammarSelector: " + gs.ToString(), "gs");
        }
    }

    #endregion
}
