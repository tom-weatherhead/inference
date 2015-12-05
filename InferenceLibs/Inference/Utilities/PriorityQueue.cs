using System;
using System.Collections;
using System.Collections.Generic;

namespace Inference.Utilities
{
    public interface IQueue<T>
    {
        int Count { get; }
        void Clear();
        bool Contains(T t);
        bool IsEmpty { get; }
        T Peek();
        bool TryPeek(out T t);
        void Enqueue(T t);
        T Dequeue();
        bool TryDequeue(out T t);
        List<T> DequeueAllToList();
    }

    public class EmptyPriorityQueueException : Exception
    {
        public EmptyPriorityQueueException()
            : base("Invalid operation; the priority queue is empty.")
        {
        }
    }

    // Translated from C++ (see Stroustrup, p. 478).

    public class PriorityQueue<T> : IQueue<T>, IEnumerable<T> //where T : IComparable<T>
    {
        private readonly List<T> list;
        private readonly IComparer<T> comparer;

        public PriorityQueue(int capacity = 0)
            : this(Comparer<T>.Default, capacity)
        {
        }

        public PriorityQueue(IComparer<T> comparer, int capacity = 0)
        {
            this.list = new List<T>(capacity);
            this.comparer = comparer;
        }

        public PriorityQueue<T> Clone()
        {
            var result = new PriorityQueue<T>(this.comparer, this.Count);

            this.list.ForEach(element => result.Enqueue(element));
            return result;
        }

        // **** Implementation of IEnumerable<T> ****

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.list.GetEnumerator();
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return this.list.GetEnumerator();
        }

        // **** Implementation of IQueue<T> ****

        public int Count
        {
            get
            {
                return list.Count;
            }
        }

        public void Clear()
        {
            list.Clear();
        }

        public bool Contains(T t)
        {
            return list.Contains(t);
        }

        public bool IsEmpty
        {
            get
            {
                return list.Count == 0;
            }
        }

        public T Peek()
        {

            if (IsEmpty)
            {
                throw new EmptyPriorityQueueException();
            }

            return list[0];
        }

        public bool TryPeek(out T t)
        {

            try
            {
                t = Peek();
                return true;
            }
            catch
            {
            }

            t = default(T);
            return false;
        }

        private void UpHeap(int nIndex)
	    {

		    while (nIndex > 0)
		    {
			    int nParentIndex = (nIndex - 1) / 2;

                //if (list[nParentIndex].CompareTo(list[nIndex]) >= 0)    // ie. if list[nParentIndex] >= list[nIndex]; the heap property already holds.
                if (comparer.Compare(list[nParentIndex], list[nIndex]) >= 0)    // ie. if list[nParentIndex] >= list[nIndex]; the heap property already holds.
                {
		    		break;
			    }

			    T temp = list[nParentIndex];

			    list[nParentIndex] = list[nIndex];
			    list[nIndex] = temp;
			    nIndex = nParentIndex;
		    }
	    }

        public void Enqueue(T t)
        {
            list.Add(t);
            UpHeap(list.Count - 1);
        }

        private void DownHeap(int nIndex)
        {

            for (; ; )
            {
                // The desired child is the larger of the two (if there are two).
                int nLeftChildIndex = 2 * nIndex + 1;

                if (nLeftChildIndex >= list.Count)
                {
                    // list[nIndex] has no children.
                    break;
                }

                int nRightChildIndex = nLeftChildIndex + 1;
                int nChildIndex = (
                    nRightChildIndex == list.Count ||		// There's just one child (the left child).
                    //list[nLeftChildIndex].CompareTo(list[nRightChildIndex]) > 0 // The left child is greater than the right child.
                    comparer.Compare(list[nLeftChildIndex], list[nRightChildIndex]) > 0 // The left child is greater than the right child.
                    ) ? nLeftChildIndex : nRightChildIndex;

                //if (list[nIndex].CompareTo(list[nChildIndex]) >= 0) // I.e. if list[nIndex] >= list[nChildIndex]; the heap property already holds.
                if (comparer.Compare(list[nIndex], list[nChildIndex]) >= 0) // I.e. if list[nIndex] >= list[nChildIndex]; the heap property already holds.
                {
                    break;
                }

                T temp = list[nChildIndex];

                list[nChildIndex] = list[nIndex];
                list[nIndex] = temp;
                nIndex = nChildIndex;
            }
        }

        public T Dequeue()
	    {
		
		    if (IsEmpty)
		    {
                throw new EmptyPriorityQueueException();
		    }

            var result = list[0];

            list[0] = list[list.Count - 1];
            list.RemoveAt(list.Count - 1);
            DownHeap(0);
            return result;
	    }

        public bool TryDequeue(out T t)
        {

            try
            {
                t = Dequeue();
                return true;
            }
            catch
            {
            }

            t = default(T);
            return false;
        }

        public List<T> DequeueAllToList()
        {
            var result = new List<T>(this.Count);

            while (!this.IsEmpty)
            {
                result.Add(this.Dequeue());
            }

            return result;
        }

        public void FindAndUpHeap(T t)
        {
            UpHeap(this.list.FindIndex(t2 => t2.Equals(t)));
        }

        public T Find(T t)
        {
            return this.list.Find(t2 => t2.Equals(t));
        }
    }
}
