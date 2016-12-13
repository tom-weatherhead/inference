using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
//using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter
{
    public class InterpreterTokenizer : TokenizerBase
    {
        private string m_str = null;   // The string to be tokenized.
        private readonly StringBuilder sbToken = new StringBuilder();
        private int line_num = 1;		// Current line number
        private int col_num = 1;        // Current column number
        private int char_num = 0;       // Current index into m_str
        private readonly Dictionary<char, TokenType> dictCharToTokenType = new Dictionary<char, TokenType>();
        private readonly bool markQuotedTokens = false; // For use in LISP and Scheme (and SASL?) macro support.
        private bool lastTokenWasASingleQuote = false;
        private int quotedBracketDepth = 0;
        private readonly Dictionary<char, TokenType> dictQuoteDelimiterToTokenType = new Dictionary<char, TokenType>();
        private readonly char commentDelimiter = ';';

        public InterpreterTokenizer(GrammarSelector gs)
        {
            // This dictionary is used to recognize single-character tokens.
            dictCharToTokenType['('] = TokenType.T_LeftBracket;
            dictCharToTokenType[')'] = TokenType.T_RightBracket;

            if (gs == GrammarSelector.LISP ||
                gs == GrammarSelector.Scheme ||
                gs == GrammarSelector.SASL)
            {
                dictCharToTokenType['\''] = TokenType.T_Apostrophe;
                dictQuoteDelimiterToTokenType['"'] = TokenType.T_StrLit;
                markQuotedTokens = true;
            }

            if (gs == GrammarSelector.APL)
            {
                dictCharToTokenType['\''] = TokenType.T_Apostrophe;
            }

            if (gs == GrammarSelector.CLU)
            {
                dictCharToTokenType['$'] = TokenType.T_Dollar;
            }

            if (gs == GrammarSelector.Smalltalk)
            {
                dictCharToTokenType['#'] = TokenType.T_Octothorpe;
                // ThAW 2014/02/03 : We want to recognize $; a sample Smalltalk character literal is $a (see page 319).
                dictCharToTokenType['$'] = TokenType.T_Dollar;
                // Use single quotes, not double quotes, as the string delimiter.  See the example string literal on page 319 of Kamin.
                dictQuoteDelimiterToTokenType['\''] = TokenType.T_StrLit;
            }

            if (gs == GrammarSelector.Prolog2)
            {
                dictCharToTokenType[','] = TokenType.T_Comma;
                dictCharToTokenType['.'] = TokenType.T_Dot;
                dictCharToTokenType['['] = TokenType.T_LeftSquareBracket;
                dictCharToTokenType[']'] = TokenType.T_RightSquareBracket;
                dictCharToTokenType['|'] = TokenType.T_OrBar;
                dictCharToTokenType[';'] = TokenType.T_Semicolon;
                dictCharToTokenType['{'] = TokenType.T_LeftCurlyBrace;
                dictCharToTokenType['}'] = TokenType.T_RightCurlyBrace;
                dictQuoteDelimiterToTokenType['\''] = TokenType.T_Ident;    // For constructing identifiers that contain spaces or special characters.
                dictQuoteDelimiterToTokenType['"'] = TokenType.T_StrLit;
                commentDelimiter = '%'; // See http://users.cs.cf.ac.uk/O.F.Rana/prolog/lectureP2/node10.html
            }

            if (gs == GrammarSelector.JSON)
            {
                dictCharToTokenType.Clear();
                dictCharToTokenType[','] = TokenType.T_Comma;
                dictCharToTokenType[':'] = TokenType.T_Colon;
                dictCharToTokenType['['] = TokenType.T_LeftSquareBracket;
                dictCharToTokenType[']'] = TokenType.T_RightSquareBracket;
                dictCharToTokenType['{'] = TokenType.T_LeftCurlyBrace;
                dictCharToTokenType['}'] = TokenType.T_RightCurlyBrace;
                dictQuoteDelimiterToTokenType['"'] = TokenType.T_StrLit;
                // Is it possible to place a comment in a real JSON expression?  What is the delimiter?
            }

            //markQuotedTokens = gs == GrammarSelector.LISP || gs == GrammarSelector.Scheme;

            if (gs == GrammarSelector.APL)
            {
                commentDelimiter = '#'; // We cannot use ';', since [;] is an APL operator.
            }
        }

        protected override void SetInputString(string str)
        {
            m_str = str;
            line_num = 1;
            col_num = 1;
            char_num = 0;
            lastTokenWasASingleQuote = false;
            quotedBracketDepth = 0;
        }

        private bool IsIntegerLiteral(string str)
        {
            var i = str.StartsWith("-") ? 1 : 0;

#if DEAD_CODE
            for (; i < str.Length; ++i)
            {

                if (!char.IsNumber(str[i]))
                {
                    return false;
                }
            }

            return true;
#else
            // str.Length > i : Do not accept "" or "-" as possible integers.
            return str.Length > i && str.Skip(i).All(c => char.IsNumber(c));
#endif
        }

#if DEAD_CODE
        private bool IsDoubleLiteral(string str)
        {
            var dotCount = 0;
            var i = str.StartsWith("-") ? 1 : 0;

            for (; i < str.Length; ++i)
            {

                if (str[i] == '.')
                {
                    ++dotCount;
                }
                else if (!char.IsNumber(str[i]))
                {
                    return false;
                }
            }

            return dotCount == 1;
        }
#endif

        protected override Token GetToken()
        {
            Token result = null;
            var startColNum = col_num;
            var local_lastTokenWasASingleQuote = lastTokenWasASingleQuote;

            lastTokenWasASingleQuote = false;
            sbToken.Clear();

            for (; ; )
            {

                if (char_num >= m_str.Length)
                {
                    return new Token(TokenType.T_EOF, "EOF", line_num, startColNum);
                }

                var cAsStr = m_str.Substring(char_num, 1);
                var c = m_str[char_num++];

                ++col_num;

                if (c == commentDelimiter)   // A comment.
                {

                    for (; ; )
                    {

                        if (char_num >= m_str.Length)
                        {
                            break;
                        }

                        c = m_str[char_num++];
                        ++col_num;

                        if (c == '\n')
                        {
                            ++line_num;
                            col_num = 1;
                            break;
                        }
                    }

                    startColNum = col_num;
                    continue;
                }

                if (c == '\n')
                {
                    ++line_num;
                    col_num = 1;
                    startColNum = 1;
                    continue;
                }

                if (char.IsWhiteSpace(c))
                {
                    startColNum = col_num;
                    continue;
                }

                if (dictQuoteDelimiterToTokenType.ContainsKey(c))
                {
                    var delimiter = c;

                    for (; ; )
                    {

                        if (char_num >= m_str.Length)
                        {
                            throw new TokenizerException("Quoted literal is not terminated before the end of the input.", line_num, startColNum);
                        }

                        c = m_str[char_num++];
                        ++col_num;

                        if (c == '\n')
                        {
                            throw new TokenizerException("Quoted literal is not terminated before the end of the line.", line_num, startColNum);
                        }
                        else if (c == delimiter)
                        {
                            return new Token(dictQuoteDelimiterToTokenType[delimiter], sbToken.ToString(), line_num, startColNum);
                        }
                        else
                        {
                            sbToken.Append(c);
                        }
                    }
                }

                if (dictCharToTokenType.ContainsKey(c))
                {

                    if (markQuotedTokens)
                    {
                        lastTokenWasASingleQuote = c == '\'';

                        if (c == '(' && (local_lastTokenWasASingleQuote || quotedBracketDepth > 0))
                        {
                            ++quotedBracketDepth;
                        }
                        else if (c == ')' && quotedBracketDepth > 0)
                        {
                            --quotedBracketDepth;
                        }
                    }

                    return new Token(dictCharToTokenType[c], cAsStr, line_num, startColNum);
                }

                // Else: Find the next ( ) ; whitespace \n or EOF, and interpret what's in between as a name.

                for (; ; )
                {
                    sbToken.Append(c);

                    if (char_num >= m_str.Length)
                    {
                        break;
                    }

                    c = m_str[char_num];

                    // TODO 2014/04/03?  Should we also check dictQuoteDelimiterToTokenType.ContainsKey(c) ?
                    if (dictCharToTokenType.ContainsKey(c) || c == commentDelimiter || c == '\n' || char.IsWhiteSpace(c))
                    {
                        break;
                    }

                    ++char_num;
                    ++col_num;
                }

                var tokenAsString = sbToken.ToString();
                int tokenAsInt;
                double tokenAsDouble;

                // The following allows for user-defined function names such as +1 (but not -1).

                if (IsIntegerLiteral(tokenAsString) && int.TryParse(tokenAsString, out tokenAsInt))
                {
                    result = new Token(TokenType.T_IntLit, tokenAsInt, line_num, startColNum);
                }
                //else if (IsDoubleLiteral(tokenAsString) && double.TryParse(tokenAsString, out tokenAsDouble))
                // Let "+1", etc. be recognized as identifiers, not floats.
                else if (tokenAsString[0] != '+' && double.TryParse(tokenAsString, out tokenAsDouble))
                {
                    result = new Token(TokenType.T_FltLit, tokenAsDouble, line_num, startColNum);
                }
                else if (tokenAsString == "quote" && markQuotedTokens && !local_lastTokenWasASingleQuote)
                {
                    // For all cases except the ones such as the expression "'quote", as in the unit test EvalInLISP().
                    result = new Token(TokenType.T_QuoteKeyword, tokenAsString, line_num, startColNum, false);
                }
                else
                {
                    result = new Token(TokenType.T_Ident, tokenAsString, line_num, startColNum,
                        markQuotedTokens && (local_lastTokenWasASingleQuote || quotedBracketDepth > 0));
                }

                break;
            }

            return result;
        }
    }
}
