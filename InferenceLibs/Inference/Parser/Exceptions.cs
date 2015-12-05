using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;

namespace Inference.Parser
{
    public class GrammarException : Exception
    {
        public GrammarException(string message)
            : base("Grammar error: " + message)
        {
        }
    }

    public class ExceptionWithLineAndColumnNumbers : Exception
    {
        public readonly int Line;
        public readonly int Column;

        protected ExceptionWithLineAndColumnNumbers(string message, int line, int column)
            : base(message)
        {
            Line = line;
            Column = column;
        }

        public string FullMessage
        {
            get
            {

                if (Line != 0 && Column != 0)
                {
                    return string.Format("{0} on line {1}, column {2}", Message, Line, Column);
                }
                else
                {
                    return Message;
                }
            }
        }
    }

    public class SyntaxException : ExceptionWithLineAndColumnNumbers
    {
        public SyntaxException(string message, int line = 0, int column = 0)
            //: base("Syntax error: " + message, line, column)
            : base(message, line, column)
        {
        }
    }

    public class InternalErrorException : Exception
    {
        public InternalErrorException(string message)
            : base("Internal parser error: " + message)
        {
        }
    }

    /*
    public class LRParserActionException : Exception
    {
        protected LRParserActionException(string message)
            : base(message)
        {
        }
    }
     */

    public class ReduceReduceConflictException : Exception //LRParserActionException
    {
        public ReduceReduceConflictException(string message)
            : base(message)
        {
        }
    }

    public class ShiftReduceConflictException : Exception //LRParserActionException
    {
        public ShiftReduceConflictException(string message)
            : base(message)
        {
        }
    }
}
