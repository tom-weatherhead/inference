using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;

namespace Inference.Interpreter.Smalltalk
{
    public class SmalltalkInterpreter : InterpreterBase
    {
        private readonly SmalltalkGlobalInfo globalInfo;

        public SmalltalkInterpreter()
            : base(GrammarSelector.Smalltalk)
        {
            globalInfo = new SmalltalkGlobalInfo(tokenizer, parser);
            SetGlobalInfoOps(globalInfo);
        }

        public override string LanguageName { get { return "Smalltalk"; } }

        public override string Evaluate(object parseResult)
        {
            var expr = parseResult as ISmalltalkExpression;

            return expr.Evaluate(null, globalInfo.ObjectInstance, null, globalInfo).ToString();
        }
    }
}
