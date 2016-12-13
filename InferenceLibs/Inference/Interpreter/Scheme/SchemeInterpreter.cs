using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.LISP;       // For ISExpression
using Inference.Parser;                 // For GrammarSelector

namespace Inference.Interpreter.Scheme
{
    public class SchemeInterpreter : InterpreterBase
    {
        private readonly SchemeGlobalInfo globalInfo;

        public SchemeInterpreter(bool quiet = false)
            : base(GrammarSelector.Scheme, quiet)
        {
            globalInfo = new SchemeGlobalInfo(tokenizer, parser);
            SetGlobalInfoOps(globalInfo);
        }

        public override string LanguageName { get { return "Scheme"; } }

        public override string Evaluate(object parseResult)
        {
            var expr = parseResult as IExpression<ISExpression>;

            return expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo).ToString();
        }
    }
}
