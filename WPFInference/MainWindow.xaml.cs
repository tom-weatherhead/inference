using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Inference.Parser;
using Inference.Resolution;

namespace WPFInference
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;
        private readonly KnowledgeBase knowledgeBase = new KnowledgeBase();

        public MainWindow()
        {
            InitializeComponent();

            Inference.Persistence.PersistenceInitializer.Init();

            tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            parser = ParserFactory.Create(ParserSelector.LALR1, GrammarSelector.Inference);

            tbOutput.Text = "Ready.";
        }

        private void CloseCommand_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            Application.Current.Shutdown();
        }

        private void btnParse_Click(object sender, RoutedEventArgs e)
        {
            var strInput = tbInput.Text;

            if (string.IsNullOrEmpty(strInput))
            {
                return;
            }

            try
            {
                var parseTree = parser.Parse(tokenizer.Tokenize(strInput));
                var countBefore = knowledgeBase.ClauseDict.Count;

                knowledgeBase.AddClausesAndResolve(strInput, false);

                var countAfter = knowledgeBase.ClauseDict.Count;
                var sb = new StringBuilder();

                sb.AppendLine("Boolean expression entered:");
                sb.AppendLine(parseTree.ToString());
                sb.AppendLine(string.Format("{0} new clause(s) added.", countAfter - countBefore));
                tbOutput.Text = sb.ToString();
            }
            catch (Exception ex)
            {
                tbOutput.Text = string.Format("{0} caught: {1}", ex.GetType().FullName, ex.Message);
            }
        }

        private void ListClausesInKnowledgeBase(string preamble)
        {
            var sb = new StringBuilder();

            if (!string.IsNullOrEmpty(preamble))
            {
                sb.AppendLine(preamble);
            }

            sb.AppendLine("Contents of the knowledge base:");

            foreach (var clause in knowledgeBase.ClauseDict.Values)
            {
                sb.AppendLine("Clause: " + clause.ToString());
            }

            sb.AppendLine(string.Format("{0} clause(s) in the knowledge base.", knowledgeBase.ClauseDict.Count));
            tbOutput.Text = sb.ToString();
        }

        private void btnLoad_Click(object sender, RoutedEventArgs e)
        {

            try
            {
                knowledgeBase.Load();
                ListClausesInKnowledgeBase("Loading the knowledge base...");
            }
            catch (Exception ex)
            {
                tbOutput.Text = string.Format("{0} caught: {1}", ex.GetType().FullName, ex.Message);
            }
        }

        private void btnSave_Click(object sender, RoutedEventArgs e)
        {

            try
            {
                var numUnsavedClauses = knowledgeBase.clausesNotSavedToDatabase.Count;

                knowledgeBase.SaveAllUnsavedClauses();
                tbOutput.Text = string.Format("{0} clause(s) saved.", numUnsavedClauses);
            }
            catch (Exception ex)
            {
                tbOutput.Text = string.Format("{0} caught: {1}", ex.GetType().FullName, ex.Message);
            }
        }

        private void btnList_Click(object sender, RoutedEventArgs e)
        {
            ListClausesInKnowledgeBase(null);
        }
    }
}
