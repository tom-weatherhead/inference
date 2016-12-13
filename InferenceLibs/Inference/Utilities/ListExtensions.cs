using System;
using System.Collections.Generic;

namespace Inference.Utilities
{
    public static class ListExtensions
    {
        public static List<int> To(this int n, int upperLimit)
        {
            int capacity = upperLimit - n + 1;

            if (capacity <= 0)
            {
                return new List<int>();
            }

            var result = new List<int>(capacity);

            for (; n <= upperLimit; ++n)
            {
                result.Add(n);
            }

            return result;
        }

        public static List<T> Shuffle<T>(this List<T> list)
        {
            var r = new Random();

            for (int i = list.Count - 1; i > 0; --i)
            {
                int j = r.Next(i + 1);

                if (i != j)
                {
                    T temp = list[i];

                    list[i] = list[j];
                    list[j] = temp;
                }
            }

            return list;
        }

        public static List<T> ShuffleCopy<T>(this List<T> list)
        {
            return new List<T>(list).Shuffle();
        }

        public static PriorityQueue<T> ToPriorityQueue<T>(this List<T> list, IComparer<T> comparer = null)
        {
            var result = new PriorityQueue<T>(comparer ?? Comparer<T>.Default, list.Count);

            list.ForEach(element => result.Enqueue(element));
            return result;
        }

        // AddItems() is so named to avoid conflicting with List<T>.AddRange().

        public static void AddItems<T>(this ICollection<T> collection, IEnumerable<T> items)
        {

            foreach (var item in items)
            {
                collection.Add(item);
            }
        }

        public static void AddRangeUnique<T>(this List<T> list, IEnumerable<T> items)
        {

            foreach (var item in items)
            {

                if (!list.Contains(item))
                {
                    list.Add(item);
                }
            }
        }
    }
}
