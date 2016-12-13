using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.APL
{
    public class APLInterpreter : InterpreterBase
    {
        private readonly APLGlobalInfo globalInfo;

        public APLInterpreter()
            : base(GrammarSelector.APL)
        {
            globalInfo = new APLGlobalInfo(tokenizer, parser);
            SetGlobalInfoOps(globalInfo);
        }

        public override string LanguageName { get { return "APL"; } }

        public override string Evaluate(object parseResult)
        {
            var expr = parseResult as IExpression<IAPLValue>;

            return expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo).ToString();
        }
    }
}
