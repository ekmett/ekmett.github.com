using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace Digitigrade.Collections {
    /// <summary>
    /// Variant on Witold Litwin's 1980 sorted linear hash table. This is here to 
    /// prove correctness of my linear hashing logic before working on the linear bloom.
    /// NB: This is not-thread safe, and relies on having an exclusive writer or multiple readers
    /// managed by external code.
    /// </summary>
    /// <typeparam name="T">The type of items to store in the collection</typeparam>
    
    [DebuggerDisplay("Count = {count}")]
    public class SortedLinearHashTable<T> : ICollection<T> {

        public const int DefaultSize = 4;
        public const int MinimalSize = 2;

        internal IEqualityComparer<T> comparer;
        internal SinglyLinkedList<T>[] data;
        internal int count;

        private int mask;
        private int Stride { 
	    get { return (mask + 1) / 2; } 
	}

        #region Constructors
        public SortedLinearHashTable() : this(PreparedEqualityComparer<T>.Default, DefaultSize) {}
        public SortedLinearHashTable(IEqualityComparer<T> comparer) : this(comparer, DefaultSize) {}
        public SortedLinearHashTable(IEqualityComparer<T> comparer, int initialSize) {
            this.comparer = comparer;
            this.data = new SinglyLinkedList<T>[Math.Max(initialSize,MinimalSize)];
        }

        #endregion

        #region ICollection<T> Members

        public void Add(T item) {
            AddBucket();
            data[GetHashCode(item)].Add(item);
        }

        public void Clear() {
            this.data = new SinglyLinkedList<T>[MinimalSize];
            this.count = 0;
            this.mask = 0;
        }

        public bool Contains(T item) {
            return data[GetHashCode(item)].Contains(item, comparer);
        }

        internal bool Any(Func<T,bool> predicate, int hash) {
            return GetSlot(hash).Any<T>(predicate);
        }

        public void CopyTo(T[] array, int arrayIndex) {
            if (array == null) throw new ArgumentNullException("array");
            if (arrayIndex < 0 || arrayIndex > array.Length) throw new ArgumentOutOfRangeException("arrayIndex");
            if (arrayIndex + count > array.Length) throw new ArgumentException("arrayIndex + count exceeds array length");
            for (var i = 0; i < count; i++) {
                foreach (T item in data[i]) {
                    array[arrayIndex++] = item;
                }
            }
        }

        public int Count {
            get { return count; }
        }

        public bool IsReadOnly {
            get { return false; }
        }

        public bool Remove(T item) {
            bool removed = data[GetHashCode(item)].Remove(item,comparer);
            RemoveBucket();
            return removed;
        }

        public int Remove(Func<T,bool> predicate) {
            int debt = 0;
            for (int i = 0; i < count; ++i) {
                debt += data[i].Remove(predicate);
            }
            int removed = debt;
            while (debt-- > 0) RemoveBucket();
            return removed;
            
        }

        internal int Remove(Func<T,bool> predicate, int hash) {
            int debt = GetSlot(hash).Remove(predicate);
            int removed = debt;
            while (debt-- > 0) RemoveBucket();
            return removed;
        }

        #endregion
        #region IEnumerable<T> Members

        public IEnumerator<T> GetEnumerator() {
            for (var i = 0; i < count; i++) {
                foreach (T item in data[i]) {
                    yield return item;
                }
            }

        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator() {
            return GetEnumerator();            
        }

        #endregion

        #region Implementation Details

        internal void RemoveBucket() {
            Debug.Assert(count > 0);

            if (--count < Stride) {
                mask -= Stride;
            }

            // 0,1,2,3,4,5,6,7,8... count
            // 0,1,3,3,7,7,7,7,15.. mask
            // 0,1,2,2,4,4,4,4,8... stride
            
            SinglyLinkedList<T> left = data[count - Stride];
            
            foreach (T item in data[count]) {
                left.Add(item);
            }
            
            data[count] = null; // prevent dangling references
            
            if (data.Length > MinimalSize && count * 4 < data.Length) { 
                // if we can shrink and we've lost a lot of mass, then do so.
                // TODO: shrink more if possible
                int newSize = Math.Max(data.Length / 2, MinimalSize);
                SinglyLinkedList<T>[] newData = new SinglyLinkedList<T>[newSize];
                Array.Copy(data, newData, newSize);
                data = newData;
            }
        }

        internal void AddBucket() {
            // make sure we allocate room if we need it
            if (count == data.Length) {
                SinglyLinkedList<T>[] newData = new SinglyLinkedList<T>[count * 2];
                Array.Copy(data, newData, count);
                data = newData;
            }

            // create the new bucket
            SinglyLinkedList<T> rightList = data[count] = new SinglyLinkedList<T>();

            if (count != 0) {
                int left = count - Stride;
                SinglyLinkedList<T> oldList = data[left];
                SinglyLinkedList<T> leftList = data[left] = new SinglyLinkedList<T>();                
                foreach (T element in oldList) {
                    int hashCode = comparer.GetHashCode(element);
                    if ((hashCode & mask) == left) {
                        leftList.Add(element);
                    } else {
                        Debug.Assert((hashCode & mask) == count);
                        rightList.Add(element);
                    }
                }
            }
                                        // 0,1,2,3,4,5,6,7,8... count
            mask |= ++count;            // 0,1,3,3,7,7,7,7,15.. mask
            //stride = (mask + 1) >> 1; // 0,1,2,2,4,4,4,4,8... stride

        }

        internal int GetHashCode(T item) {
            int hashCode = comparer.GetHashCode(item) & mask;
            
            if (hashCode >= count)
                hashCode -= Stride;
            
            return hashCode;
        }

        internal SinglyLinkedList<T> GetSlot(int hashCode) {
            hashCode &= mask;
            if (hashCode >= count)
                hashCode -= Stride;
            return data[hashCode];
        }


        internal void Audit() {
            Debug.Assert(data.Length == MinimalSize || count < 4 * data.Length);
            for (int i = 0; i < count; i++) {
                foreach (var item in data[i]) {
                    Debug.Assert(GetHashCode(item) == i);
                }
            }
        }
        #endregion
    }
}
