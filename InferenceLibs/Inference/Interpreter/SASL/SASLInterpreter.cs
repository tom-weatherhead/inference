using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.LISP;
using Inference.Parser;

namespace Inference.Interpreter.SASL
{
    public class SASLInterpreter : InterpreterBase
    {
        private readonly SASLGlobalInfo globalInfo;

        public SASLInterpreter()
            : base(GrammarSelector.SASL)
        {
            globalInfo = new SASLGlobalInfo(tokenizer, parser);
            SetGlobalInfoOps(globalInfo);
        }

        public override string LanguageName { get { return "SASL"; } }

        public override string Evaluate(object parseResult)
        {
            var expr = parseResult as IExpression<ISExpression>;

            return expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo).ToString();
        }
    }
}
