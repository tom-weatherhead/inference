using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Domain;
using Inference.Parser;

namespace Inference.Expert
{
    #region DomainSelector

    public enum DomainSelector
    {
        JackAutomotive,     // Based on CS 486 Project 3
        JackComputer        // Based on CS 486 Project 3
    }

    #endregion

    #region IExpertQuery

    public interface IExpertQuery
    {
        bool Query(string question);
    }

    #endregion

    #region ExpertQueryBase

    public class ExpertQueryBase : IExpertQuery
    {
        protected readonly Dictionary<string, bool> Answers = new Dictionary<string, bool>();

        public bool Query(string question)
        {

            if (!Answers.ContainsKey(question))
            {
                throw new Exception(string.Format("QueryBase.Query() : There is no automated answer for the question: '{0}'", question));
            }

            return Answers[question];
        }
    }

    #endregion

    #region DescribedLiteral

    public class DescribedLiteral : Literal
    {
        public readonly string Statement;

        public DescribedLiteral(Predicate p, bool neg, string statement)
            : base(p, neg)
        {
            Statement = statement;
        }

        public DescribedLiteral(DescribedLiteral src)   // Copy constructor
            : base(src.Predicate, src.IsNegated)
        {
            Statement = src.Statement;
        }

        public static Literal StringToLiteral(ITokenizer tokenizer, IParser parser, string str)
        {
            List<Token> listOfTokens = tokenizer.Tokenize(str);
            IBooleanExpression expr = parser.Parse(listOfTokens) as IBooleanExpression;

            if (expr == null)
            {
                throw new Exception(string.Format("The string '{0}' does not parse to a BooleanExpression.", str));
            }

            List<Clause> clauseList = Clause.ConvertBooleanExpressionToClausalForm(expr);

            if (clauseList.Count != 1)
            {
                throw new Exception(string.Format("The string '{0}' does not parse to a single Clause.", str));
            }

            Clause clause = clauseList[0];

            if (clause.LiteralList.Count != 1)
            {
                throw new Exception(string.Format("The string '{0}' does not parse to a single Literal.", str));
            }

            return clause.LiteralList[0];
        }

        public static DescribedLiteral Parse(ITokenizer tokenizer, IParser parser, string str, string statement)
        {
            Literal literal = StringToLiteral(tokenizer, parser, str);

            return new DescribedLiteral(literal.Predicate, literal.IsNegated, statement);
        }

        public override IExpression ApplySubstitution(Substitution sub)
        {
            Literal newLiteral = base.ApplySubstitution(sub) as Literal;
            string statement = Statement;

            foreach (Variable v in sub.SubstitutionList.Keys)
            {
                statement = statement.Replace(v.ToString(), sub.SubstitutionList[v].ToString());
            }

            return new DescribedLiteral(newLiteral.Predicate, newLiteral.IsNegated, statement);
        }
    }

    #endregion

    #region Question

    public class Question : DescribedLiteral
    {
        public readonly string QuestionText;
        //public readonly string NegatedStatement;  // TODO: Use this.

        public Question(DescribedLiteral dl, string q)
            : base(dl)
        {
            QuestionText = q;
        }

        public static Question Parse(ITokenizer tokenizer, IParser parser, string str, string statement, string question)
        {
            DescribedLiteral dl = DescribedLiteral.Parse(tokenizer, parser, str, statement);

            return new Question(dl, question);
        }

        public override IExpression ApplySubstitution(Substitution sub)
        {
            DescribedLiteral newDescribedLiteral = base.ApplySubstitution(sub) as DescribedLiteral;
            string question = QuestionText;

            foreach (Variable v in sub.SubstitutionList.Keys)
            {
                question = question.Replace(v.ToString(), sub.SubstitutionList[v].ToString());
            }

            return new Question(newDescribedLiteral, question);
        }
    }

    #endregion

    #region Fact

    public class Fact
    {
        public readonly DescribedLiteral Head;

        public Fact(DescribedLiteral head)
        {
            Head = head;
        }
    }

    #endregion

    #region Rule

    public class Rule : Fact
    {
        public readonly List<Literal> Conditions;

        public Rule(List<Literal> conditions, DescribedLiteral head)
            : base(head)
        {
            Conditions = conditions;
        }

        public static Rule Parse(ITokenizer tokenizer, IParser parser, List<string> conditionsAsStrings, string headAsString, string explanation)
        {
            List<Literal> conditions = new List<Literal>();

            foreach (string cond in conditionsAsStrings)
            {
                conditions.Add(DescribedLiteral.StringToLiteral(tokenizer, parser, cond));
            }

            return new Rule(conditions, DescribedLiteral.Parse(tokenizer, parser, headAsString, explanation));
        }
    }

    #endregion

    #region ModifiedAStarState

    public class ModifiedAStarState
    {
        public readonly Stack<DescribedLiteral> literalStack;
        public readonly List<string> reasons;
        public readonly string diagnosis;

        private ModifiedAStarState(Stack<DescribedLiteral> ls, List<string> r, string d)
        {
            literalStack = ls;
            reasons = (r != null) ? new List<string>(r) : new List<string>();
            diagnosis = d;
        }

        public ModifiedAStarState(List<string> r, string d)
            : this(new Stack<DescribedLiteral>(), r, d)
        {
        }

        public ModifiedAStarState(Stack<DescribedLiteral> ls)
            : this(ls, null, string.Empty)
        {
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            ModifiedAStarState that = obj as ModifiedAStarState;

            // TODO: Should we rewrite this so that we search for each member of this.literalStack in that.literalStack, and vice versa?
            // This might also affect the implementation of GetHashCode().

            if (that == null || literalStack.Count != that.literalStack.Count)
            {
                return false;
            }

            List<DescribedLiteral> thisList = new List<DescribedLiteral>(literalStack);
            List<DescribedLiteral> thatList = new List<DescribedLiteral>(that.literalStack);

            for (int i = 0; i < thisList.Count; ++i)
            {

                if (!thisList[i].Equals(thatList[i]))
                {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode()
        {
            int hashCode = 0;

            foreach (DescribedLiteral dl in literalStack)
            {
                hashCode *= 101;
                hashCode += dl.GetHashCode();
            }

            return hashCode;
        }
    }

    #endregion

    #region ModifiedAStarAlgorithm

    public class ModifiedAStarAlgorithm
    {
        private readonly Stack<ModifiedAStarState> openList = new Stack<ModifiedAStarState>();
        private readonly HashSet<ModifiedAStarState> closedList = new HashSet<ModifiedAStarState>();
        private readonly List<Fact> facts = new List<Fact>();
        private readonly List<Rule> rules = new List<Rule>();
        private readonly List<Question> questions = new List<Question>();
        private readonly IExpertQuery queryInterface;

        public ModifiedAStarAlgorithm(DomainSelector ds, IExpertQuery qi)
        {
            queryInterface = qi;

            switch (ds)
            {
                case DomainSelector.JackAutomotive:
                    PopulateRulesWithJackAutomotive();
                    break;

                case DomainSelector.JackComputer:
                    PopulateRulesWithJackComputer();
                    break;

                default:
                    throw new Exception("ModifiedAStarAlgorithm constructor: Unrecognized domain selector.");
            }
        }

        public ModifiedAStarAlgorithm(DomainSelector ds)
            : this(ds, null)
        {
        }

        private void PopulateRulesWithJackAutomotive()
        {
            ITokenizer tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            IParser parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Inference);
            //IParser parser = new LR1Parser(GrammarSelector.Inference);

            questions.Clear();
            questions.Add(Question.Parse(tokenizer, parser, "@radioWorks(?car)", "The car's radio works.", "Does the car's radio work?"));
            questions.Add(Question.Parse(tokenizer, parser, "@containsBulletHole(?car)", "There is a bullet hole in the car.", "Is there a bullet hole in the car?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isAnimalFurOnDriversSeat(?car)", "There is animal fur on the car's driver's seat.", "Is there animal fur on the car's driver's seat?"));
            questions.Add(Question.Parse(tokenizer, parser, "@areClawMarksOnSteeringWheel(?car)", "There are claw marks on the car's steering wheel.", "Are there claw marks on the car's steering wheel?"));
            questions.Add(Question.Parse(tokenizer, parser, "@containsDice(?car)", "Dice were found in the car.", "Were dice found in the car?"));
            questions.Add(Question.Parse(tokenizer, parser, "@containsPlayingCards(?car)", "Playing cards were found in the car.", "Were playing cards found in the car?"));
            questions.Add(Question.Parse(tokenizer, parser, "@containsGlowingPolyhedra(?car)", "Glowing polyhedra were found in the car.", "Were glowing polyhedra found in the car?"));
            questions.Add(Question.Parse(tokenizer, parser, "@unseenVoiceSpeaksFromTrunk(?car)", "An invisible voice speaks from the car's trunk.", "Does an invisible voice speak from the car's trunk?"));
            questions.Add(Question.Parse(tokenizer, parser, "@voiceRecitesShakespeareanSonnets(?car)", "The voice recites Shakespearean sonnets.", "Does the voice recite Shakespearean sonnets?"));

            rules.Clear();
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@radioWorks(?car)" }, "@isBatteryCharged(?car)", "The car's radio works if and only if the car's battery is charged."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "!@radioWorks(?car)" }, "!@isBatteryCharged(?car)", "The car's radio works if and only if the car's battery is charged."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "!@isBatteryCharged(?car)" }, "@problemIs(?car, \"The car's battery is dead.\")", "The car requires a charged battery in order to operate."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@containsBulletHole(?car)" }, "@problemIs(?car, \"Someone shot the car.\")", "Bullet damage impairs automotive functionality."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@isAnimalFurOnDriversSeat(?car)", "@areClawMarksOnSteeringWheel(?car)" }, "@animalWasDriving(?car)", "An animal has been driving the car."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@containsDice(?car)" }, "@containsGamblingMaterials(?car)", "Dice are gambling materials."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@containsPlayingCards(?car)" }, "@containsGamblingMaterials(?car)", "Playing cards are gambling materials."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@animalWasDriving(?car)", "@containsGamblingMaterials(?car)" },
                "@problemIs(?car, \"An animal drove the car into the ground during a cross-country gambling binge.\")",
                "Animals carry gambling materials when driving only when they are gambling compulsively."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@containsGlowingPolyhedra(?car)" },
                "@problemIs(?car, \"A surrealistic icon sucked the life force out of the car.\")",
                "The icon's glow is the car's extracted life force."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@unseenVoiceSpeaksFromTrunk(?car)", "@voiceRecitesShakespeareanSonnets(?car)" },
                "@problemIs(?car, \"The car has been used to haul the recycled paper of old books.\")",
                "The subject matter of a book escapes after recycling, and is retained in nearby automotive parts."));
        }

        private void PopulateRulesWithJackComputer()
        {
            ITokenizer tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            IParser parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Inference);
            //IParser parser = new LR1Parser(GrammarSelector.Inference);

            questions.Clear();
            questions.Add(Question.Parse(tokenizer, parser, "@isPluggedIn(?computer)", "The computer is plugged in.", "Is the computer plugged in?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isPowerAvailable()", "Power is available at the socket.", "Is power available at the socket?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isSwitchedOn(?computer)", "The computer is switched on.", "Is the computer switched on?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isMonitorPluggedIn(?computer)", "The monitor is plugged in.", "Is the monitor plugged in?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isMonitorSwitchedOn(?computer)", "The monitor is switched on.", "Is the monitor switched on?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isConnectedToMonitor(?computer)", "The computer is connected to its monitor.", "Is the computer connected to its monitor?"));
            questions.Add(Question.Parse(tokenizer, parser, "@doesMonitorDisplayAPicture(?computer)", "The monitor displays a picture.", "Does the monitor display a picture?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isFloppyDriveFuseBlown(?computer)", "The floppy drive's fuse is blown.", "Is the floppy drive's fuse blown?"));
            questions.Add(Question.Parse(tokenizer, parser, "@doesFloppyDriveContainDisk(?computer)", "There is a disk in the computer's floppy drive.", "Is there a disk in the computer's floppy drive?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isFloppyDiskFormatted(?computer)", "The disk in the computer's floppy drive is formatted.", "Is the disk in the computer's floppy drive formatted?"));
            questions.Add(Question.Parse(tokenizer, parser, "@doesFloppyDiskContainFiles(?computer)",
                "The disk in the computer's floppy drive is known to contain files.",
                "Is the disk in the computer's floppy drive known to contain files?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isDataReadFromFloppyDrive(?computer)",
                "Data is successfully read from the computer's floppy drive.",
                "Is data successfully read from the computer's floppy drive?"));
            questions.Add(Question.Parse(tokenizer, parser, "@isBulletHoleInMonitor(?computer)", "There is a bullet hole in the monitor.", "Is there a bullet hole in the monitor?"));

            rules.Clear();
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "!@isPluggedIn(?computer)" }, "@problemIs(?computer, \"The computer is not plugged in.\")",
                "The computer must be plugged in in order to operate."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "!@isSwitchedOn(?computer)" }, "@problemIs(?computer, \"The computer is not switched on.\")",
                "The computer must be switched on in order to operate."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "!@isPowerAvailable()" }, "@problemIs(?computer, \"Power is not available at the socket.\")",
                "The computer requires electrical power in order to operate."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@isPluggedIn(?computer)", "@isPowerAvailable()", "@isSwitchedOn(?computer)" }, "@motherboardHasPower(?computer)",
                "The computer's motherboard has power."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@motherboardHasPower(?computer)" }, "@isFanOperating(?computer)", "The computer's fan is operating."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "!@isMonitorPluggedIn(?computer)" }, "@problemIs(?computer, \"The computer's monitor is not plugged in.\")",
                "The monitor must be plugged in in order to operate."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "!@isMonitorSwitchedOn(?computer)" }, "@problemIs(?computer, \"The computer's monitor is not switched on.\")",
                "The monitor must be switched on in order to operate."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@isMonitorPluggedIn(?computer)", "@isPowerAvailable()", "@isMonitorSwitchedOn(?computer)" }, "@monitorHasPower(?computer)",
                "The computer's monitor has power."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "!@isConnectedToMonitor(?computer)" }, "@problemIs(?computer, \"The computer is not connected to its monitor.\")",
                "The monitor must be connected to its computer."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@monitorHasPower(?computer)", "@isConnectedToMonitor(?computer)", "!@doesMonitorDisplayAPicture(?computer)" },
                "@problemIs(?computer, \"The monitor is blown.\")", "The computer's monitor is blown."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@motherboardHasPower(?computer)" }, "@floppyDriveHasPower(?computer)",
                "The computer's floppy drive draws its power from the motherboard."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@isFloppyDriveFuseBlown(?computer)" }, "@problemIs(?computer, \"The computer's floppy drive's fuse is blown.\")",
                "The floppy drive won't operate with a blown fuse."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@floppyDriveHasPower(?computer)", "!@isFloppyDriveFuseBlown(?computer)" }, "@floppyDriveSpins(?computer)",
                "The computer's floppy drive spins, since it has power."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@doesFloppyDriveContainDisk(?computer)", "@floppyDriveSpins(?computer)" }, "@floppyDriveOperates(?computer)",
                "The computer's floppy drive operates."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@doesFloppyDriveContainDisk(?computer)", "!@isFloppyDiskFormatted(?computer)" },
                "@problemIs(?computer, \"The disk in the computer's floppy drive is not formatted.\")", "A floppy disk must be formatted before it can be used."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@floppyDriveOperates(?computer)", "@isFloppyDiskFormatted(?computer)", "@doesFloppyDiskContainFiles(?computer)", "!@isDataReadFromFloppyDrive(?computer)" },
                "@problemIs(?computer, \"The computer's floppy drive is out of alignment.\")", "The computer's floppy drive is out of alignment."));
            rules.Add(Rule.Parse(tokenizer, parser, new List<string>() { "@isBulletHoleInMonitor(?computer)" }, "@problemIs(?computer, \"Someone shot the computer's monitor.\")",
                "A bullet hole allows outside air to contaminate the picture tube's interior."));
        }

        private bool TryToUnifyFact(ModifiedAStarState currentState, Fact fact, Stack<ModifiedAStarState> newStates)
        {
            DescribedLiteral literal = currentState.literalStack.Peek();
            Substitution substitution = literal.Unify(fact.Head);

            if (substitution == null)
            {
                return false;
            }

            DescribedLiteral literal2 = fact.Head.ApplySubstitution(substitution) as DescribedLiteral;
            Rule rule = fact as Rule;
            string diagnosis = currentState.diagnosis;

            if (string.IsNullOrEmpty(diagnosis))
            {
                Variable v = new Variable("explanationString");

                if (literal2.Predicate.Name == "problemIs" && substitution.SubstitutionList.ContainsKey(v))
                {
                    StringLiteral varValue = substitution.SubstitutionList[v] as StringLiteral;

                    if (varValue != null)
                    {
                        diagnosis = varValue.Value;
                    }
                }
            }

            ModifiedAStarState newState = new ModifiedAStarState(currentState.reasons, diagnosis);

            foreach (DescribedLiteral l in currentState.literalStack)
            {
                DescribedLiteral l2 = l.ApplySubstitution(substitution) as DescribedLiteral;

                if (!l2.Equals(literal2))
                {
                    newState.literalStack.Push(l2);
                }
            }

            if (string.IsNullOrEmpty(literal2.Statement))
            {
                Console.WriteLine("TryToUnifyFact() : Empty reason.  literal2 == {0}", literal2.ToString());
            }

            newState.reasons.Add(literal2.Statement);

            if (rule != null)
            {
                // "fact" is actually a rule.

                //foreach (Literal l in rule.Conditions)
                for (int i = rule.Conditions.Count - 1; i >= 0; --i)    // Reverse the order of the pushes onto the newState.literalStack
                {
                    Literal l = rule.Conditions[i];
                    Literal l2 = l.ApplySubstitution(substitution) as Literal;
                    DescribedLiteral dl = new DescribedLiteral(l2.Predicate, l2.IsNegated, string.Empty);   // Construct a DescribedLiteral from a Literal.

                    if (!newState.literalStack.Contains(dl))
                    {
                        newState.literalStack.Push(dl);
                    }
                }
            }

            if (!openList.Contains(newState) && !closedList.Contains(newState) && !newStates.Contains(newState))
            {
                newStates.Push(newState);
            }

            return true;
        }

        private Question GetQuestion(DescribedLiteral literal)
        {

            foreach (Question q in questions)
            {
                Substitution unifier = q.Unify(literal);

                if (unifier != null)
                {
                    return q.ApplySubstitution(unifier) as Question;
                }
            }

            throw new Exception(string.Format("ModifiedAStarAlgorithm.GetQuestion() : No question found for the literal '{0}'.", literal.ToString()));
        }

        private void GenerateSuccessorStates(ModifiedAStarState currentState)
        {
            Stack<ModifiedAStarState> newStates = new Stack<ModifiedAStarState>();
            bool unified = false;

            foreach (Fact fact in facts)
            {

                if (TryToUnifyFact(currentState, fact, newStates))
                {
                    unified = true;
                }
            }

            foreach (Rule rule in rules)
            {

                if (TryToUnifyFact(currentState, rule, newStates))
                {
                    unified = true;
                }
            }

            if (!unified)
            {
                // First, search the facts list to see if we already know that currentState.literalStack.Peek() is false.
                DescribedLiteral literal = currentState.literalStack.Peek();
                DescribedLiteral inverseLiteral = new DescribedLiteral(literal.Predicate, !literal.IsNegated, string.Empty);

                foreach (Fact fact in facts)
                {

                    if (inverseLiteral.Equals(fact.Head))
                    {
                        // We know that literal is false, so don't ask the user.
                        return;
                    }
                }

                // Use the shell to ask the user whether or not currentState.literalStack.Peek() is true.
                DescribedLiteral questionLiteral = new DescribedLiteral(literal.Predicate, false, literal.Statement);
                Question q = GetQuestion(questionLiteral);

                if (q == null)
                {
                    return;
                }

                bool booleanAnswer;

                if (queryInterface != null)
                {
                    booleanAnswer = queryInterface.Query(q.QuestionText);
                }
                else
                {
                    string response;

                    do
                    {
                        Console.WriteLine("Yes or no question: {0}", q.QuestionText);
                        response = Console.ReadLine().ToLower();
                    }
                    while (string.IsNullOrEmpty(response) || (response[0] != 'y' && response[0] != 'n'));

                    booleanAnswer = response[0] == 'y';
                }

                string st = q.Statement;

                if (literal.IsNegated)
                {
                    booleanAnswer = !booleanAnswer;
                }

                // booleanAnswer == literal.IsNegated is the same as !(booleanAnswer XOR literal.IsNegated).
                bool negateNewFact = booleanAnswer == literal.IsNegated;

                if (negateNewFact)
                {
                    st = "Not: " + st;
                }

                DescribedLiteral dl = new DescribedLiteral(literal.Predicate, negateNewFact, st);

                facts.Add(new Fact(dl));

                if (booleanAnswer)
                {
                    currentState.literalStack.Pop();
                    currentState.reasons.Add(dl.Statement);

                    if (!openList.Contains(currentState) && !closedList.Contains(currentState) && !newStates.Contains(currentState))
                    {
                        newStates.Push(currentState);
                    }
                }
            }

            while (newStates.Count > 0)
            {
                openList.Push(newStates.Pop());
            }
        }

        public ModifiedAStarState Search()
        {
            ITokenizer tokenizer = TokenizerFactory.Create(GrammarSelector.Inference);
            IParser parser = ParserFactory.Create(ParserSelector.LL1, GrammarSelector.Inference);
            Stack<DescribedLiteral> literalStack = new Stack<DescribedLiteral>();

            literalStack.Push(DescribedLiteral.Parse(tokenizer, parser, "@problemIs(?object, ?explanationString)", string.Empty));
            //literalStack.Push(DescribedLiteral.Parse(parser, "@problemIs(?object, ?explanationString)", "?explanationString"));

            ModifiedAStarState startState = new ModifiedAStarState(literalStack);

            openList.Clear();
            closedList.Clear();
            openList.Push(startState);

            while (openList.Count > 0)
            {
                ModifiedAStarState currentState = openList.Pop();

                if (currentState.literalStack.Count == 0)
                {
                    return currentState;
                }

                closedList.Add(currentState);
                GenerateSuccessorStates(currentState);
            }

            return null;
        }

        public void SearchAndReport()
        {
            ModifiedAStarState goalState = Search();

            if (goalState == null)
            {
                Console.WriteLine("Unable to diagnose the problem.");
                return;
            }

            Console.WriteLine("Diagnosis: {0}", goalState.diagnosis);
            Console.WriteLine("Reasons:");

            //foreach (string reason in goalState.reasons)
            for (int i = goalState.reasons.Count - 1; i >= 0; --i)
            {
                string reason = goalState.reasons[i];

                Console.WriteLine("  " + reason);
                //Console.WriteLine("  " + (string.IsNullOrEmpty(reason) ? "(Empty)" : reason));
            }

            /*
            Console.WriteLine("Facts:");

            foreach (Fact fact in facts)
            {
                Console.WriteLine("  {0}; {1}", fact.Head.Statement, fact.Head.ToString());
            }
             */
        }
    }

    #endregion
}
