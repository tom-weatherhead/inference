using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Inference.Utilities;
using NUnit.Framework;

namespace Inference.Tests.Utilities
{
    [TestFixture]
    public class RedBlackTree_Fixture
    {
        private readonly IDictionary<int, string> rbtree;

        public RedBlackTree_Fixture()
        {
            rbtree = new RedBlackTree<int, string>();
        }

        [SetUp]
        public void SetupTest()
        {
            rbtree.Clear();
        }

        private void InsertRangeOfIntsAndStrings(IEnumerable<int> numList)
        {
            rbtree.AddItems(numList.Select(n => new KeyValuePair<int, string>(n, n.ToString())));
        }

        private string GetStringOfValuesFromInOrderTraversal()
        {
            return string.Join(" ", rbtree.Select(kvp => kvp.Value));
        }

        private void AssertKeyValueCorrespondence()
        {

            foreach (var kvp in rbtree)
            {
                Assert.AreEqual(kvp.Key.ToString(), kvp.Value);
            }
        }

        [Test]
        public void InsertTest1()
        {
            rbtree.Add(2, "2");

            Assert.AreEqual("2", GetStringOfValuesFromInOrderTraversal());
            AssertKeyValueCorrespondence();
        }

        [Test]
        public void InsertTest2()
        {
            InsertRangeOfIntsAndStrings(new List<int>() { 1, 2, 3 });

            Assert.AreEqual("1 2 3", GetStringOfValuesFromInOrderTraversal());
            AssertKeyValueCorrespondence();
        }

        [Test]
        public void InsertTest3()
        {
            var sorted = 1.To(100);
            var shuffled = sorted.ShuffleCopy();

            InsertRangeOfIntsAndStrings(shuffled);

            Assert.AreEqual(string.Join(" ", sorted), GetStringOfValuesFromInOrderTraversal());
            AssertKeyValueCorrespondence();
        }

        [Test]
        public void InsertTest4Ascending()
        {
            var sorted = new List<int>();

            for (var i = 1; i <= 100; ++i)
            {
                sorted.Add(i);
                rbtree.Add(i, i.ToString());

                Assert.AreEqual(string.Join(" ", sorted), GetStringOfValuesFromInOrderTraversal());
                AssertKeyValueCorrespondence();
            }
        }

        [Test]
        public void InsertTest5Descending()
        {
            var sorted = new List<int>();

            for (var i = 100; i >= 1; --i)
            {
                sorted.Insert(0, i);
                rbtree.Add(i, i.ToString());

                Assert.AreEqual(string.Join(" ", sorted), GetStringOfValuesFromInOrderTraversal());
                AssertKeyValueCorrespondence();
            }
        }

        [Test]
        public void DeleteTest1()
        {
            const int numNumbers = 20;
            const int firstNumberToRemove = 10;
            var sorted = 1.To(numNumbers);
            var shuffled = sorted.ShuffleCopy();
            bool successfulDeletion;

            InsertRangeOfIntsAndStrings(shuffled);
            //InsertRangeOfIntsAndStrings(sorted);

            sorted.Remove(firstNumberToRemove);
            successfulDeletion = rbtree.Remove(firstNumberToRemove);

            Assert.IsTrue(successfulDeletion);

            sorted.Remove(3);
            rbtree.Remove(3);

            sorted.Remove(16);
            rbtree.Remove(16);

            sorted.Remove(1);
            rbtree.Remove(1);

            sorted.Remove(20);
            rbtree.Remove(20);

            successfulDeletion = rbtree.Remove(firstNumberToRemove);    // Try to delete firstNumberToRemove again.

            Assert.IsFalse(successfulDeletion);

            Assert.AreEqual(string.Join(" ", sorted), GetStringOfValuesFromInOrderTraversal());
            AssertKeyValueCorrespondence();
        }

        [Test]
        public void DeleteTest2DeleteAll()
        {
            var sorted = 1.To(100);
            var shuffled = sorted.ShuffleCopy();
            var r = new Random();

            InsertRangeOfIntsAndStrings(shuffled);

            while (sorted.Count > 0)
            {
                var n = sorted[r.Next(sorted.Count)];

                rbtree.Remove(n);
                sorted.Remove(n);

                Assert.AreEqual(string.Join(" ", sorted), GetStringOfValuesFromInOrderTraversal());
                AssertKeyValueCorrespondence();
            }

            Assert.AreEqual(string.Empty, GetStringOfValuesFromInOrderTraversal());
        }

        [Test]
        public void DeleteTest3Ascending()
        {
            const int n = 20;
            var numbers = 1.To(n);

            InsertRangeOfIntsAndStrings(numbers);

            foreach (var i in numbers)
            {
                rbtree.Remove(i);

                AssertKeyValueCorrespondence();
            }

            Assert.AreEqual(string.Empty, GetStringOfValuesFromInOrderTraversal());
        }

        [Test]
        public void DeleteTest4Descending()
        {
            const int n = 20;
            var numbers = 1.To(n);

            InsertRangeOfIntsAndStrings(numbers);

            for (var i = n; i >= 1; --i)
            {
                rbtree.Remove(i);

                AssertKeyValueCorrespondence();
            }

            Assert.AreEqual(string.Empty, GetStringOfValuesFromInOrderTraversal());
        }

        [Test]
        public void FindTest()
        {
            // 1) Try to find a key in an empty tree.
            string valueThatWasFound;
            bool keyWasFound = rbtree.TryGetValue(4, out valueThatWasFound);

            Assert.IsFalse(keyWasFound);
            Assert.AreEqual(default(string), valueThatWasFound);

            // 2) Find a present key in a non-empty tree.
            InsertRangeOfIntsAndStrings(new List<int>() { 2, 4, 6, 8, 10, 12, 14 });

            keyWasFound = rbtree.TryGetValue(4, out valueThatWasFound);

            Assert.IsTrue(keyWasFound);
            Assert.AreEqual("4", valueThatWasFound);

            // 2) Try to find an absent key in a non-empty tree.
            keyWasFound = rbtree.TryGetValue(9, out valueThatWasFound);

            Assert.IsFalse(keyWasFound);
            Assert.AreEqual(default(string), valueThatWasFound);
        }

        [Test]
        public void FindTest2FindAll()
        {
            var sorted = 1.To(20);

            InsertRangeOfIntsAndStrings(sorted.ShuffleCopy());

            foreach (var n in sorted)
            {
                string valueThatWasFound;
                var keyWasFound = rbtree.TryGetValue(n, out valueThatWasFound);

                Assert.IsTrue(keyWasFound);
                Assert.AreEqual(n.ToString(), valueThatWasFound);
            }
        }

        [Test]
        public void DeleteAndFindSuccessorTest()
        {
            InsertRangeOfIntsAndStrings(new List<int>() { 2, 1, 3 });
            rbtree.Remove(2);

            string valueThatWasFound;
            bool keyWasFound = rbtree.TryGetValue(3, out valueThatWasFound);

            Assert.IsTrue(keyWasFound);
            Assert.AreEqual("3", valueThatWasFound);
        }
    }
}
