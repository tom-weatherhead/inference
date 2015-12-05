using System;
using System.Collections.Generic;
using Inference.Utilities;
using NUnit.Framework;

namespace Inference.Tests.Utilities
{
    class OddEvenIntComparer : Comparer<int>
    {
        public override int Compare(int x, int y)
        {
            bool xIsEven = (x % 2 == 0);
            bool yIsEven = (y % 2 == 0);

            // We want all odd numbers first, followed by all even numbers.

            if (!xIsEven && yIsEven)
            {
                return 1;
            }

            if (xIsEven && !yIsEven)
            {
                return -1;
            }

            return y - x;   // If x and y have the same parity, the smaller one has the higher priority.
        }
    }

    [TestFixture]
    public class PriorityQueue_Fixture
    {
        private readonly PriorityQueue<int> pq;

        public PriorityQueue_Fixture()
        {
            pq = new PriorityQueue<int>();
        }

        [SetUp]
        public void SetupTest()
        {
            pq.Clear();
        }

        [Test]
        public void PriorityTest1()
        {
            pq.Enqueue(3);
            pq.Enqueue(5);
            pq.Enqueue(1);
            pq.Enqueue(2);
            pq.Enqueue(4);

            Assert.IsFalse(pq.IsEmpty);
            Assert.AreEqual(5, pq.Peek());
            Assert.AreEqual(5, pq.Dequeue());
            Assert.AreEqual(4, pq.Dequeue());
            Assert.AreEqual(3, pq.Dequeue());
            Assert.AreEqual(2, pq.Dequeue());
            Assert.AreEqual(1, pq.Dequeue());
            Assert.IsTrue(pq.IsEmpty);
        }

        [Test]
        public void PeekEmptyTest()
        {
            Assert.Throws<EmptyPriorityQueueException>(() => pq.Peek());
        }

        [Test]
        public void TryPeekEmptyTest()
        {
            int n;
            bool success = pq.TryPeek(out n);

            Assert.IsFalse(success);
            Assert.AreEqual(default(int), n);
        }

        [Test]
        public void TryPeekNotEmptyTest()
        {
            pq.Enqueue(3);
            pq.Enqueue(5);
            pq.Enqueue(1);

            int n;
            bool success = pq.TryPeek(out n);

            Assert.IsTrue(success);
            Assert.AreEqual(5, n);
        }

        [Test]
        public void DequeueEmptyTest()
        {
            Assert.Throws<EmptyPriorityQueueException>(() => pq.Dequeue());
        }

        [Test]
        public void TryDequeueEmptyTest()
        {
            int n;
            bool success = pq.TryDequeue(out n);

            Assert.IsFalse(success);
            Assert.AreEqual(default(int), n);
        }

        [Test]
        public void TryDequeueNotEmptyTest()
        {
            pq.Enqueue(3);
            pq.Enqueue(5);
            pq.Enqueue(1);

            int n;
            bool success = pq.TryDequeue(out n);

            Assert.IsTrue(success);
            Assert.AreEqual(5, n);
            Assert.AreEqual(3, pq.Peek());
        }

        [Test]
        public void CustomComparerTest()
        {
            IComparer<int> comparer = new OddEvenIntComparer();
            PriorityQueue<int> pqCustom = new PriorityQueue<int>(comparer);

            // TODO: Call Jenny.
            pqCustom.Enqueue(8);
            pqCustom.Enqueue(6);
            pqCustom.Enqueue(7);
            pqCustom.Enqueue(5);
            pqCustom.Enqueue(3);
            pqCustom.Enqueue(0);
            pqCustom.Enqueue(9);

            Assert.AreEqual(3, pqCustom.Dequeue());
            Assert.AreEqual(5, pqCustom.Dequeue());
            Assert.AreEqual(7, pqCustom.Dequeue());
            Assert.AreEqual(9, pqCustom.Dequeue());
            Assert.AreEqual(0, pqCustom.Dequeue());
            Assert.AreEqual(6, pqCustom.Dequeue());
            Assert.AreEqual(8, pqCustom.Dequeue());
            Assert.IsTrue(pqCustom.IsEmpty);
        }

        [Test]
        public void HundredIntTest()
        {
            const int upperLimit = 100;
            var list = 1.To(upperLimit).Shuffle().ToPriorityQueue().DequeueAllToList();

            Assert.AreEqual(upperLimit, list.Count);

            for (int i = 0; i < upperLimit; ++i)
            {
                Assert.AreEqual(upperLimit - i, list[i]);
            }
        }

        [Test]
        public void CloneTest()
        {
            const int randomNumberRange = 10000;
            const int numberOfElements = 100;

            var r = new Random();

            for (int i = 0; i < numberOfElements; ++i)
            {
                pq.Enqueue(r.Next(randomNumberRange));
            }

            var pqClone = pq.Clone();

            Assert.AreEqual(pq.Count, pqClone.Count);

            while (!pq.IsEmpty)
            {
                Assert.AreEqual(pq.Dequeue(), pqClone.Dequeue());
            }
        }
    }
}
