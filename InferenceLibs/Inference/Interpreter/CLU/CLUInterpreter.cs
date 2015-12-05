using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.CLU
{
    public class CLUInterpreter : InterpreterBase
    {
        private readonly CLUGlobalInfo globalInfo;

        public CLUInterpreter()
            : base(GrammarSelector.CLU)
        {
            globalInfo = new CLUGlobalInfo(tokenizer, parser);
            SetGlobalInfoOps(globalInfo);
        }

        public override string LanguageName { get { return "CLU"; } }

        public override string Evaluate(object parseResult)
        {
            var expr = parseResult as ICLUExpression;

            return expr.Evaluate(globalInfo.GlobalEnvironment, null, globalInfo).ToString();
        }
    }
}
