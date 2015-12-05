﻿using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;

namespace Inference.Resolution
{
    public class KnowledgeBaseException : Exception
    {
        public KnowledgeBaseException(string message)
            : base(message)
        {
        }
    }

    public class ContradictionException : KnowledgeBaseException
    {
        /*
        public ContradictionException()
            : base("A contradiction has been generated by resolution")
        {
        }
         */

        public ContradictionException(string message)
            : base(message)
        {
        }
    }
}
