﻿using System;
using System.Collections;           // For IEnumerable and IEnumerator.
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Inference.Utilities
{
    public abstract class DictionaryTreeBase<TKey, TValue> : IDictionary<TKey, TValue>
    {
        // Abstract methods.
        public abstract void Clear();
        protected abstract void Insert(TKey key, TValue value);
        protected abstract bool Delete(TKey keyToDelete);
        protected abstract bool Find(TKey keyToFind, out TValue valueThatWasFound);
        protected abstract List<KeyValuePair<TKey, TValue>> InOrderTraversal();

        // Properties.

        public int Count
        {
            get
            {
                return InOrderTraversal().Count;
            }
        }

        public bool IsReadOnly
        {
            get
            {
                return false;
            }
        }

        public TValue this[TKey key]    // The Item property.
        {
            get
            {
                TValue value;

                if (!TryGetValue(key, out value))
                {
                    throw new KeyNotFoundException();
                }

                return value;
            }
            set
            {
                Add(key, value);
            }
        }

        public ICollection<TKey> Keys
        {
            get
            {
                return InOrderTraversal().Select(kvp => kvp.Key).ToList();
            }
        }

        public ICollection<TValue> Values
        {
            get
            {
                return InOrderTraversal().Select(kvp => kvp.Value).ToList();
            }
        }

        // Methods.

        public void Add(KeyValuePair<TKey, TValue> kvp)
        {
            Add(kvp.Key, kvp.Value);
        }

        public void Add(TKey key, TValue value)
        {
            Insert(key, value);
        }

        // Clear() is an abstract method in this class; it was declared above.

        public bool Contains(KeyValuePair<TKey, TValue> kvp)
        {
#if DEAD_CODE
            TValue value;

            return TryGetValue(kvp.Key, out value) && value.Equals(kvp.Value);
#else
            // This is less efficient than the above, but I think it may be safer in the case where value == null.
            // I don't think it would be wise to call value.Equals(kvp.Value) when value == null.
            return InOrderTraversal().Contains(kvp);
#endif
        }

        public bool ContainsKey(TKey key)
        {
            TValue value;

            return TryGetValue(key, out value);
        }

        public void CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex)
        {
            InOrderTraversal().CopyTo(array, arrayIndex);
        }

        IEnumerator IEnumerable.GetEnumerator() // This method is necessary, but it cannot be public.
        {
            return InOrderTraversal().GetEnumerator();
        }

        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator()
        {
            return InOrderTraversal().GetEnumerator();
        }

        public bool Remove(KeyValuePair<TKey, TValue> kvp)
        {

            if (!Contains(kvp))
            {
                return false;
            }

            return Remove(kvp.Key);
        }

        public bool Remove(TKey key)
        {
            return Delete(key);
        }

        public bool TryGetValue(TKey key, out TValue value)
        {
            return Find(key, out value);
        }
    }
}
