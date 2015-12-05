using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.LISP
{
    public class LISPInterpreter : InterpreterBase
    {
        private readonly LISPGlobalInfo globalInfo;

        public LISPInterpreter(bool quiet = false)
            : base(GrammarSelector.LISP, quiet)
        {
            globalInfo = new LISPGlobalInfo(tokenizer, parser);
            SetGlobalInfoOps(globalInfo);
        }

        public override string LanguageName { get { return "LISP"; } }

        public override string Evaluate(object parseResult)
        {
            var expr = parseResult as IExpression<ISExpression>;

            return expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo).ToString();
        }
    }
}
