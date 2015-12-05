using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Parser;                 // For GrammarSelector

namespace Inference.Interpreter.Prolog
{
    #region PrologInterpreter

    public class PrologInterpreter : InterpreterBase, IInterpreterFileLoader
    {
        public readonly PrologGlobalInfo globalInfo;
        private readonly bool grammarIsProlog2;

        public PrologInterpreter(GrammarSelector gs, bool unitTesting = false)
            : base(gs)
        {

            if (gs != GrammarSelector.Prolog && gs != GrammarSelector.Prolog2)
            {
                throw new Exception(string.Format("A non-Prolog GrammarSelector '{0}' was passed to the Prolog interpreter constructor", gs));
            }

            globalInfo = new PrologGlobalInfo(gs, tokenizer, parser);
            SetGlobalInfoOps(globalInfo);
            globalInfo.PathToDefaultDirectory = unitTesting ? DefaultDirectoryFromTests : DefaultDirectory;
            globalInfo.FileLoader = this;
            grammarIsProlog2 = gs == GrammarSelector.Prolog2;
        }

        public override string LanguageName { get { return "Prolog"; } }

        public override bool ProcessCommand(string input, out string output)
        {
            input = input.Trim();

            if (input == "first" || input.StartsWith("first "))
            {
                globalInfo.FindFirstSolution();
                output = "The interpreter will now search for the first solution only.";
            }
            else if (input == "all" || input.StartsWith("all "))
            {
                globalInfo.FindAllSolutions();
                output = "The interpreter will now search for all solutions.";
            }
            else if (input.StartsWith("[") && input.EndsWith("]"))
            {
                output = LoadFileList(input.Substring(1, input.Length - 2));
            }
            else
            {
                return base.ProcessCommand(input, out output);
            }

            return true;
        }

        private string LoadFileList(string str)
        {
            var strList = str.Split(',');
            var totalNumExpressionsEvaluated = 0;

            foreach (var str2 in strList)
            {
                var filename = str2.Trim();
                int numExpressionsEvaluated;

                if (filename.StartsWith("'") && filename.EndsWith("'"))
                {
                    filename = filename.Substring(1, filename.Length - 2).Trim();
                }

                if (string.IsNullOrEmpty(filename))
                {
                    continue;
                }

                if (!filename.Contains('.'))
                {
                    filename = filename + ".pl";
                }

                CompletePathAndLoadFile(filename, out numExpressionsEvaluated, string.Empty);
                totalNumExpressionsEvaluated += numExpressionsEvaluated;
            }

            return string.Format("{0} file(s) loaded; {1} expression(s) evaluated.", strList.Length, totalNumExpressionsEvaluated);
        }

        // Process commands that can only occur in files, e.g. Prolog's :- [file1, file2, file3]

        protected override bool ProcessFileOnlyCommand(string line, out string commandOutput)
        {
#if DEAD_CODE
            line = line.Trim();

            // Determine whether or not line contains something like ":- [file1, file2, file3].".
            // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse52

            if (line.StartsWith(":-") && line.EndsWith("."))
            {
                line = line.Substring(2, line.Length - 3).Trim();

                if (line.StartsWith("[") && line.EndsWith("]"))
                {
                    commandOutput = LoadFileList(line.Substring(1, line.Length - 2));
                    return true;
                }
            }
#endif
            commandOutput = string.Empty;
            return false;
        }

        // Returns true iff the trimmed line begins with a character that can begin a command in a file, e.g. not '[' in Prolog.

        protected override bool LineFromFileStartsWithCommandChar(string line)
        {
            return !line.TrimStart().StartsWith("[");
        }

        protected override bool IsTokenListACompleteStatement(List<Token> listOfTokens)
        {

            if (grammarIsProlog2)
            {
                // A complete Prolog2 statement, as a token list, ends with a dot followed by an EOF.
                return listOfTokens.Count >= 2 && listOfTokens[listOfTokens.Count - 2].TokenType == TokenType.T_Dot;
            }

            return base.IsTokenListACompleteStatement(listOfTokens);
        }

        public override string Evaluate(object parseResult)
        {
            var currentModuleName = string.Empty;

            return globalInfo.ProcessInput(parseResult, ref currentModuleName);
        }

        protected override string EvaluateFromListOfTokens(List<Token> listOfTokens, ref string currentModuleName)
        {
            return globalInfo.ProcessTokenList(listOfTokens, ref currentModuleName);
        }
    }

    #endregion
}
