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
    public class BTree_DataLeaves_Fixture
    {
        private readonly IDictionary<int, string> btree;

        public BTree_DataLeaves_Fixture()
        {
            btree = new BTree<int, string>(true);
        }

        [SetUp]
        public void SetupTest()
        {
            btree.Clear();
        }

        private void InsertRangeOfIntsAndStrings(IEnumerable<int> numList)
        {
            btree.AddItems(numList.Select(n => new KeyValuePair<int, string>(n, n.ToString())));
        }

        private string GetStringOfValuesFromInOrderTraversal()
        {
            return string.Join(" ", btree.Select(kvp => kvp.Value));
        }

        private void AssertKeyValueCorrespondence()
        {

            foreach (var kvp in btree)
            {
                Assert.AreEqual(kvp.Key.ToString(), kvp.Value);
            }
        }

        [Test]
        public void InsertTest1()
        {
            btree.Add(2, "2");

            Assert.AreEqual("2", GetStringOfValuesFromInOrderTraversal());
            AssertKeyValueCorrespondence();
        }

        [Test]
        public void InsertTest2()
        {
            InsertRangeOfIntsAndStrings(new List<int>() { 17, 5, 11, 2, 19, 7, 13, 3 });

            Assert.AreEqual("2 3 5 7 11 13 17 19", GetStringOfValuesFromInOrderTraversal());
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
                btree.Add(i, i.ToString());

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
                btree.Add(i, i.ToString());

                Assert.AreEqual(string.Join(" ", sorted), GetStringOfValuesFromInOrderTraversal());
                AssertKeyValueCorrespondence();
            }
        }

        [Test]
        public void DeleteTest1()
        {
            var sorted = 1.To(20);
            var shuffled = sorted.ShuffleCopy();
            bool successfulDeletion;

            InsertRangeOfIntsAndStrings(shuffled);
            //InsertRangeOfIntsAndStrings(sorted);

            sorted.Remove(10);
            successfulDeletion = btree.Remove(10);

            Assert.IsTrue(successfulDeletion);

            sorted.Remove(3);
            btree.Remove(3);

            sorted.Remove(16);
            btree.Remove(16);

            sorted.Remove(1);
            btree.Remove(1);

            sorted.Remove(20);
            btree.Remove(20);

            successfulDeletion = btree.Remove(10);    // Try to delete 10 again.

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

                btree.Remove(n);
                sorted.Remove(n);

                Assert.AreEqual(string.Join(" ", sorted), GetStringOfValuesFromInOrderTraversal());
                AssertKeyValueCorrespondence();
            }

            Assert.AreEqual(string.Empty, GetStringOfValuesFromInOrderTraversal());
        }

        [Test]
        public void FindTest1()
        {
            // 1) Try to find a key in an empty tree.
            string valueThatWasFound;
            bool keyWasFound = btree.TryGetValue(4, out valueThatWasFound);

            Assert.IsFalse(keyWasFound);
            Assert.AreEqual(default(string), valueThatWasFound);

            // 2) Find a present key in a non-empty tree.
            InsertRangeOfIntsAndStrings(new List<int>() { 2, 4, 6, 8, 10, 12, 14, 16 });

            keyWasFound = btree.TryGetValue(4, out valueThatWasFound);

            Assert.IsTrue(keyWasFound);
            Assert.AreEqual("4", valueThatWasFound);

            // 2) Try to find an absent key in a non-empty tree.
            keyWasFound = btree.TryGetValue(9, out valueThatWasFound);

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
                var keyWasFound = btree.TryGetValue(n, out valueThatWasFound);

                Assert.IsTrue(keyWasFound);
                Assert.AreEqual(n.ToString(), valueThatWasFound);
            }
        }
    }
}
