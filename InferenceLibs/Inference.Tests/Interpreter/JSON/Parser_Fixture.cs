using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Interpreter.JSON;
using Inference.Parser;
using NUnit.Framework;

namespace Inference.Tests.Interpreter.JSON
{
    [TestFixture]
    public class Parser_Fixture
    {
        private readonly ITokenizer tokenizer;
        private readonly IParser parser;

        public Parser_Fixture()
        {
            tokenizer = TokenizerFactory.Create(GrammarSelector.JSON);
            parser = ParserFactory.Create(ParserSelector.SLR1, GrammarSelector.JSON);
        }

        private IValue GetParsedValue(string input)
        {
            var parseResult = parser.Parse(tokenizer.Tokenize(input));

            Assert.IsNotNull(parseResult);
            //Assert.AreEqual(input, parseResult.ToString());   // This fails on multi-line or whitespace-formatted input.
            Assert.IsTrue(parseResult is IValue);
            return (IValue)parseResult;
        }

        private string GetParsedValueAsString(string input)
        {
            return GetParsedValue(input).ToString();
        }

        [Test]
        public void RecognizeTest()
        {
            parser.Recognize(tokenizer.Tokenize("7"));
            parser.Recognize(tokenizer.Tokenize("\"string\""));
            parser.Recognize(tokenizer.Tokenize("[2, 3, 5, 7]"));
            parser.Recognize(tokenizer.Tokenize("{\"key1\": 2, \"key2\": 3, \"key3\": 5, \"key4\": 7}"));
        }

        [Test]
        public void IntegerLiteralTest()
        {
            const int expectedValue = 7;
            var value = GetParsedValue(expectedValue.ToString());

            Assert.IsTrue(value is JSONIntegerLiteral);

            var intlit = (JSONIntegerLiteral)value;

            Assert.AreEqual(expectedValue, intlit.Value);
            Assert.AreEqual(expectedValue, ((JSONIntegerLiteral)value.EvaluateExpression(null)).Value);
        }

        [Test]
        public void StringLiteralTest()
        {
            const string expectedValue = "\"This is text.\"";
            var value = GetParsedValue(expectedValue);

            Assert.IsTrue(value is JSONStringLiteral);

            var strlit = (JSONStringLiteral)value;

            Assert.AreEqual(expectedValue, strlit.ToString());
            Assert.AreEqual(expectedValue, ((JSONStringLiteral)value.EvaluateExpression(null)).ToString());
        }

        [Test]
        public void ArrayTest()
        {
            const string expectedValue = "[2, 3, 5, 7]";
            var value = GetParsedValue(expectedValue);

            Assert.IsTrue(value is JSONArray);

            var array = (JSONArray)value;

            Assert.AreEqual(expectedValue, array.ToString());
            Assert.AreEqual(4, array.List.Count);
            Assert.AreEqual(2, ((JSONIntegerLiteral)array.List[0]).Value);
            Assert.AreEqual(3, ((JSONIntegerLiteral)array.List[1]).Value);
            Assert.AreEqual(5, ((JSONIntegerLiteral)array.List[2]).Value);
            Assert.AreEqual(7, ((JSONIntegerLiteral)array.List[3]).Value);
            Assert.AreEqual(2, ((JSONIntegerLiteral)value.EvaluateExpression("[0]")).Value);
            Assert.AreEqual(3, ((JSONIntegerLiteral)value.EvaluateExpression("[1]")).Value);
            Assert.AreEqual(5, ((JSONIntegerLiteral)value.EvaluateExpression("[2]")).Value);
            Assert.AreEqual(7, ((JSONIntegerLiteral)value.EvaluateExpression("[3]")).Value);
        }

        [Test]
        public void ObjectTest()
        {
            const string inputValue = "{\"key1\": 2, \"key2\": 3, \"key3\": 5, \"key4\": 7}";
            var value = GetParsedValue(inputValue);

            Assert.IsTrue(value is JSONObject);

            var obj = (JSONObject)value;

            //Assert.AreEqual(inputValue, obj.ToString()); // This fails because the keys in the Dictionary are not sorted.

            Assert.IsTrue(obj.Dict.ContainsKey("key1"));
            Assert.AreEqual(2, ((JSONIntegerLiteral)obj.Dict["key1"]).Value);
            Assert.AreEqual(2, ((JSONIntegerLiteral)value.EvaluateExpression(".key1")).Value);

            Assert.IsTrue(obj.Dict.ContainsKey("key2"));
            Assert.AreEqual(3, ((JSONIntegerLiteral)obj.Dict["key2"]).Value);
            Assert.AreEqual(3, ((JSONIntegerLiteral)value.EvaluateExpression(".key2")).Value);

            Assert.IsTrue(obj.Dict.ContainsKey("key3"));
            Assert.AreEqual(5, ((JSONIntegerLiteral)obj.Dict["key3"]).Value);
            Assert.AreEqual(5, ((JSONIntegerLiteral)value.EvaluateExpression(".key3")).Value);

            Assert.IsTrue(obj.Dict.ContainsKey("key4"));
            Assert.AreEqual(7, ((JSONIntegerLiteral)obj.Dict["key4"]).Value);
            Assert.AreEqual(7, ((JSONIntegerLiteral)value.EvaluateExpression(".key4")).Value);
        }

        [Test]
        public void LifeGliderTest()
        {
            const string inputValue = @"
{
  ""name"": ""Glider"",
  ""width"": 3,
  ""height"": 3,
  ""rows"": [
    ""4"",
    ""2"",
    ""e""
  ]
}";
            var value = GetParsedValue(inputValue);

            Assert.IsTrue(value is JSONObject);

            Assert.AreEqual("Glider", ((JSONStringLiteral)value.EvaluateExpression(".name")).Value);
            Assert.AreEqual(3, ((JSONIntegerLiteral)value.EvaluateExpression(".width")).Value);
            Assert.AreEqual(3, ((JSONIntegerLiteral)value.EvaluateExpression(".height")).Value);
            Assert.AreEqual("4", ((JSONStringLiteral)value.EvaluateExpression(".rows[0]")).Value);
            Assert.AreEqual("2", ((JSONStringLiteral)value.EvaluateExpression(".rows[1]")).Value);
            Assert.AreEqual("e", ((JSONStringLiteral)value.EvaluateExpression(".rows[2]")).Value);
        }

        [Test]
        public void ObjectToStringTest()
        {
            const string expectedValue = "{\"key1\": 2}";
            var value = GetParsedValue(expectedValue);

            Assert.AreEqual(expectedValue, value.ToString());   // This works, despite unordered dictionary keys, because there is only one key.
            Assert.IsTrue(value is JSONObject);
            Assert.AreEqual(2, ((JSONIntegerLiteral)value.EvaluateExpression(".key1")).Value);
        }

        [Test]
        public void EmptyArrayTest()
        {
            const string expectedValue = "[]";
            var value = GetParsedValue(expectedValue);

            Assert.AreEqual(expectedValue, value.ToString());   // This works, despite unordered dictionary keys, because there is only one key.
            Assert.IsTrue(value is JSONArray);
            Assert.AreEqual(0, ((JSONArray)value).List.Count);
        }

        [Test]
        public void EmptyObjectTest()
        {
            const string expectedValue = "{}";
            var value = GetParsedValue(expectedValue);

            Assert.AreEqual(expectedValue, value.ToString());   // This works, despite unordered dictionary keys, because there is only one key.
            Assert.IsTrue(value is JSONObject);
            Assert.AreEqual(0, ((JSONObject)value).Dict.Keys.Count);
        }
    }
}
