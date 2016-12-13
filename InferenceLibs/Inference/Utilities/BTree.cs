﻿// Translated from Pascal; see MySourceCode\Pascal\University\CS340\BTREE.P

/*
{ CS 340 Assignment 3                          Tom Weatherhead 89010169 }

{ This program implements the B-tree abstract data type, including insertion,
  deletion, print, and initialization routines which are called from a
  parser front-end. }


program btree( input, output );

const
    max_nodes_on_line = 4;
    max_q_size = 1000;
    tree_order = 5;

{ Initialize the print_tree queue }

procedure queue_init( var q_ptr : q_ptr_type );
begin
    new( q_ptr );
    q_ptr^.head := 1;
    q_ptr^.tail := 1;
    q_ptr^.size := 0;
end;


{ Enqueue a node in to the print_tree queue }

procedure enqueue( q_ptr : q_ptr_type; node : node_ptr_type );
begin
    q_ptr^.queue[q_ptr^.tail] := node;
    q_ptr^.tail := (q_ptr^.tail mod max_q_size) + 1;
    q_ptr^.size := q_ptr^.size + 1;
end;


{ Remove a node from the print_tree queue }

function dequeue( q_ptr : q_ptr_type ) : node_ptr_type;
var
    node : node_ptr_type;
begin
    node := q_ptr^.queue[q_ptr^.head];
    q_ptr^.head := (q_ptr^.head mod max_q_size) + 1;
    q_ptr^.size := q_ptr^.size - 1;
    dequeue := node;
end;


{ Print a tree, level by level }

procedure print_tree;
var
    i, num_keys, nodes_on_line : integer;
    q_ptr : q_ptr_type;
    node : node_ptr_type;
begin
    nodes_on_line := 0;
    queue_init( q_ptr );
    enqueue( q_ptr, nil );
    enqueue( q_ptr, root );

    repeat
	node := dequeue( q_ptr );

	if node = nil then
	begin

	    if nodes_on_line > 0 then
	    begin
		writeln;
		nodes_on_line := 0;
	    end;

	    writeln('-------------------------------------------------------');
	    enqueue( q_ptr, nil );
	end
	else
	begin
	    num_keys := node^.num_keys;

	    for i := 1 to num_keys do
	    begin

		if node^.child[i] <> nil then
		    enqueue( q_ptr, node^.child[i] );

		write(node^.key[i]:1);

		if i = num_keys then
		    write('  ')
		else
		    write('-');

	    end;

	    if node^.child[num_keys+1] <> nil then
		enqueue( q_ptr, node^.child[num_keys+1] );

	    nodes_on_line := nodes_on_line + 1;

	    if nodes_on_line >= max_nodes_on_line then
	    begin
		nodes_on_line := 0;
		writeln;
	    end;
	end;
    until((node = nil) and (q_ptr^.size <= 1));
end;


{ Parse commands from the input stream and carry them out. }

procedure parse_input;
var
    command : char;
    key : key_type;
begin

    repeat
	read( command );

	case command of
	    'M':
	    begin
		{ writeln('Echo: M'); }
		init_tree;
		readln;
	    end;
	    'I':
	    begin
		readln( key );
		{ writeln('Echo: I ',key:1); }
		insert( key );
	    end;
	    'D':
	    begin
		readln( key );
		{ writeln('Echo: D ',key:1); }
		delete( key );
	    end;
	    'P':
	    begin
		{ writeln('Echo: P'); }
		print_tree;
		readln;
	    end;
	    otherwise
		readln;
	end;

    until( command = 'E' );

    { writeln('Echo: E'); }
end;


{ Main program }

begin
    parse_input;
end.
 */

#define VERIFY_TREE
//#define EXTRA_VERIFY_TREE
#define USE_DEBUG_EXCEPTIONS
//#define USE_CONSOLE_WRITELINE

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Inference.Utilities
{
    public class BTreeNode<TKey, TValue>
    {
        private readonly BTree<TKey, TValue> tree;
        public readonly List<TKey> keys = new List<TKey>();
        public readonly List<TValue> values = new List<TValue>();
        public readonly List<BTreeNode<TKey, TValue>> children = new List<BTreeNode<TKey, TValue>>();    // children.Count == keys.Count + 1
        public BTreeNode<TKey, TValue> parent;
        public int selfIdx;
        public bool isLeaf;

        public BTreeNode(BTree<TKey, TValue> tree, bool isLeaf)
        {
            this.tree = tree;
            this.isLeaf = isLeaf;
        }

        public int NumKeys
        {
            get
            {
                return keys.Count;
            }
        }

        // Find a place in a leaf where a given key may be inserted.  Return null in destNode if key found.

        public void FindGap(TKey key, out BTreeNode<TKey, TValue> destNode, out int idx)
        {
            var keyFound = false;

            idx = -1;

            for (var i = 0; i < NumKeys; ++i)
            {
                var comparisonResult = tree.comparer.Compare(key, keys[i]);

                if (comparisonResult == 0 && (isLeaf || !tree.allDataInLeaves))
                {
                    keyFound = true;
                    break;
                }
                else if (comparisonResult <= 0)
                {
                    idx = i;
                    break;
                }
            }

            if (idx < 0 && !keyFound)
            {
                idx = NumKeys;
            }

            if (keyFound)
            {
                destNode = null;
            }
            else if (children[idx] == null)
            {
                destNode = this;
            }
            else
            {
                children[idx].FindGap(key, out destNode, out idx);
            }
        }

        // Find the location of a given key.  Return null in destNode if key not found.

        public void FindKey(TKey key, out BTreeNode<TKey, TValue> destNode, out int idx)
        {
            var keyFound = false;

            idx = -1;

            for (var i = 0; i < NumKeys; ++i)
            {
                var comparisonResult = tree.comparer.Compare(key, keys[i]);

                if (comparisonResult == 0 && (isLeaf || !tree.allDataInLeaves))
                {
                    keyFound = true;
                    idx = i;
                    break;
                }
                else if (comparisonResult <= 0)
                {
                    idx = i;
                    break;
                }
            }

            if (idx < 0)
            {
                idx = NumKeys;
            }

            if (keyFound)
            {
                destNode = this;
            }
            else if (children[idx] == null)
            {
                destNode = null;
            }
            else
            {
                children[idx].FindKey(key, out destNode, out idx);
            }
        }

        // Shift the keys in a given node, and to the right of (and including) a given starting point, upwards.

        public void ShiftKeysUp(int first)
        {
            children.Insert(first, null);

            for (var i = first + 1; i < children.Count; ++i)
            {

                if (children[i] != null)
                {
                    children[i].selfIdx = i;
                }
            }

            keys.Insert(first, default(TKey));

            if (isLeaf || !tree.allDataInLeaves)
            {
                values.Insert(first, default(TValue));
            }
        }

        // Shift the keys in a given node, and to the right of (and including) a given starting point, downwards.

        public void ShiftKeysDown(int first)
        {
            children.RemoveAt(first - 1);

            for (var i = first - 1; i < children.Count; ++i)
            {

                if (children[i] != null)
                {
                    children[i].selfIdx = i;
                }
            }

            keys.RemoveAt(first - 1);

            if (isLeaf || !tree.allDataInLeaves)
            {
                values.RemoveAt(first - 1);
            }
        }

        // Destructive.
        // Remove and return the last key and value of the inorder traversal of the subtree that has the given node as its root.

        public void LastKey(out BTreeNode<TKey, TValue> leaf, out TKey keyToReturn, out TValue valueToReturn)
        {

#if USE_DEBUG_EXCEPTIONS
            if (children.Count != keys.Count + 1)
            {
                throw new Exception(string.Format("LastKey(): Invalid node: It has {0} keys and {1} children.", keys.Count, children.Count));
            }
#endif

            if (children[children.Count - 1] != null)
            {
                children[children.Count - 1].LastKey(out leaf, out keyToReturn, out valueToReturn);
                return;
            }

            keyToReturn = keys[keys.Count - 1];
            valueToReturn = values[keys.Count - 1];

            values.RemoveAt(keys.Count - 1);
            keys.RemoveAt(keys.Count - 1);
            children.RemoveAt(children.Count - 1);  // Remove the last child, which is null.
            leaf = this;
        }

        // Combine the given node with its right sibling, and the key in the parent in between the two nodes.

        public void CombineNodes()  // left_sib == this
        {
            var leftSelfIdx = selfIdx;
            var rightSib = parent.children[leftSelfIdx + 1];

#if EXTRA_VERIFY_TREE
            AssertBasicNodeProperties();
            rightSib.AssertBasicNodeProperties();
#endif

            if (!isLeaf || !tree.allDataInLeaves)
            {
                keys.Add(parent.keys[leftSelfIdx]);
            }
            else
            {
                children.RemoveAt(children.Count - 1);
            }

            if (!tree.allDataInLeaves)
            {
                values.Add(parent.values[leftSelfIdx]);
            }

            parent.ShiftKeysDown(leftSelfIdx + 1);
            parent.children[leftSelfIdx] = this;

            var oldNumChildren = children.Count;

            children.AddRange(rightSib.children);
            keys.AddRange(rightSib.keys);

            if (isLeaf || !tree.allDataInLeaves)
            {
                values.AddRange(rightSib.values);
            }

            for (var i = oldNumChildren; i < children.Count; ++i)
            {

                if (children[i] != null)
                {
                    children[i].selfIdx = i;
                    children[i].parent = this;
                }
            }

#if EXTRA_VERIFY_TREE
            AssertBasicNodeProperties();
#endif
        }

        public bool Find(TKey keyToFind, out TValue valueThatWasFound)
        {

            for (var i = 0; i < NumKeys; ++i)
            {
                var keyComparison = tree.comparer.Compare(keyToFind, keys[i]);

                if (keyComparison == 0 && (isLeaf || !tree.allDataInLeaves))
                {
                    valueThatWasFound = values[i];
                    return true;
                }
                else if (keyComparison <= 0)
                {
                    return tree.Find(children[i], keyToFind, out valueThatWasFound);
                }
            }

            return tree.Find(children[NumKeys], keyToFind, out valueThatWasFound);
        }

        public void InOrderTraversal(List<KeyValuePair<TKey, TValue>> result)
        {

#if USE_DEBUG_EXCEPTIONS
            if (children.Count != keys.Count + 1)
            {
                throw new Exception(string.Format("InOrderTraversal(): Invalid node: It has {0} keys and {1} children.", keys.Count, children.Count));
            }
#endif

            for (var i = 0; ; ++i)
            {

                if (children[i] != null)
                {
                    children[i].InOrderTraversal(result);
                }

                if (i >= NumKeys)
                {
                    break;
                }

                // If all data is in the leaves, don't add key-value pairs from non-leaves.

                if (isLeaf || !tree.allDataInLeaves)
                {
                    result.Add(new KeyValuePair<TKey, TValue>(keys[i], values[i]));
                }
            }
        }

#if VERIFY_TREE
        public void AssertBasicNodeProperties()
        {

            if (parent != null && NumKeys == 0)
            {
                throw new Exception("AssertBasicNodeProperties(): No keys in a non-root node.");
            }

            if (children.Count != NumKeys + 1)
            {
                throw new Exception(string.Format("AssertBasicNodeProperties(): Node has {0} keys and {1} children.", NumKeys, children.Count));
            }

            if (!isLeaf && tree.allDataInLeaves)
            {

                if (values.Count != 0)
                {
                    throw new Exception(string.Format("AssertBasicNodeProperties(): Non-leaf node has {0} values when it should have none.", values.Count));
                }
            }
            else
            {

                if (NumKeys != values.Count)
                {
                    throw new Exception(string.Format("AssertBasicNodeProperties(): Node has {0} keys and {1} values.", NumKeys, values.Count));
                }
            }
        }

        public void AssertBasicSubtreeProperties()
        {
            AssertBasicNodeProperties();

            if (!isLeaf)
            {

                foreach (var child in children)
                {
                    child.AssertBasicSubtreeProperties();
                }
            }
        }

        public void AssertSelfIdxValidity()
        {

            for (var i = 0; i < children.Count; ++i)
            {

                if (children[i] == null)
                {
                    continue;
                }

                if (children[i].selfIdx != i)
                {
                    throw new Exception(string.Format("AssertSelfIdxValidity(): selfIdx is {0} when it should be {1}.", children[i].selfIdx, i));
                }

                children[i].AssertSelfIdxValidity();
            }
        }

        public void AssertKeyUniqueness(HashSet<TKey> keySet)
        {

            for (var i = 0; ; ++i)
            {

                if (children[i] != null)
                {
                    children[i].AssertKeyUniqueness(keySet);
                }

                if (i >= NumKeys)
                {
                    break;
                }

                if (!isLeaf && tree.allDataInLeaves)
                {
                    continue;
                }

                if (keySet.Contains(keys[i]))
                {
                    throw new Exception(string.Format("AssertKeyUniqueness(): Failed on key {0}.", keys[i]));
                }

                keySet.Add(keys[i]);
            }
        }

        public int CalculateHeightsAndVerifyBalance()
        {
            var childHeight = 0;

            if (children[0] != null)
            {
                childHeight = children[0].CalculateHeightsAndVerifyBalance();
            }

            for (var i = 1; i < children.Count; ++i)
            {
                var otherChildHeight = 0;

                if (children[i] != null)
                {
                    otherChildHeight = children[i].CalculateHeightsAndVerifyBalance();
                }

                if (childHeight != otherChildHeight)
                {
                    throw new Exception(string.Format("CalculateHeightsAndVerifyBalance(): Unbalanced node: Heights {0} and {1}.",
                        childHeight, otherChildHeight));
                }
            }

            if (isLeaf != (childHeight == 0))
            {
                throw new Exception("CalculateHeightsAndVerifyBalance(): isLeaf has the wrong value.");
            }

            return childHeight + 1;
        }
#endif
    }

    public class BTree<TKey, TValue> : DictionaryTreeBase<TKey, TValue>
    {
        private const int treeOrder = 5;    // Maximum number of keys per node.
        public BTreeNode<TKey, TValue> root;
        public readonly IComparer<TKey> comparer;
        public readonly bool allDataInLeaves;

        public BTree(bool allDataInLeaves = false, IComparer<TKey> comparer = null, IEnumerable<KeyValuePair<TKey, TValue>> source = null)
        {
            this.comparer = comparer ?? Comparer<TKey>.Default;
            this.allDataInLeaves = allDataInLeaves;
            Clear();

            if (source != null)
            {
                this.AddItems(source);
            }
        }

        public BTree(IEnumerable<KeyValuePair<TKey, TValue>> source)
            : this(false, null, source)
        {
        }

        public override void Clear()
        {
            root = new BTreeNode<TKey, TValue>(this, true);
            root.children.Add(null);
            root.parent = null;
            root.selfIdx = -1;
        }

        // Attempt to add the given key to the tree (without balancing it).

        private BTreeNode<TKey, TValue> AddKeyAndValue(TKey key, TValue value)
        {
            BTreeNode<TKey, TValue> node;
            int idx;

            root.FindGap(key, out node, out idx);

            if (node != null)
            {
                node.ShiftKeysUp(idx);
                node.keys[idx] = key;
                node.values[idx] = value;
            }

            return node;
        }

        // Restore the tree that has just had a key added to it, starting at the given node (where the key was added).
        // Perform node-splitting where necessary.

        private void InsertRestore(BTreeNode<TKey, TValue> node)
        {

            while (node.NumKeys >= treeOrder)
            {
                // Split node.
                var mid = (node.NumKeys - 1) / 2; // Was (node.NumKeys + 1) / 2
                var leftSelfIdx = node.selfIdx;
                var parent = node.parent;

                if (parent == null)
                {
                    root = new BTreeNode<TKey, TValue>(this, false);
                    root.parent = null;
                    root.selfIdx = -1;
                    root.keys.Add(node.keys[mid]);

                    if (!allDataInLeaves)
                    {
                        root.values.Add(node.values[mid]);
                    }

                    root.children.Add(node);
                    parent = root;
                    node.parent = root;
                    node.selfIdx = 0;
                    leftSelfIdx = 0;
                }
                else
                {
                    parent.ShiftKeysUp(leftSelfIdx);
                    node.selfIdx = leftSelfIdx;
                    parent.children[leftSelfIdx] = node;    // Is this correct?

                    if (leftSelfIdx < parent.keys.Count)
                    {
                        parent.keys[leftSelfIdx] = node.keys[mid];

                        if (!allDataInLeaves)
                        {
                            parent.values[leftSelfIdx] = node.values[mid];
                        }
                    }
                    else
                    {
                        parent.keys.Add(node.keys[mid]);

                        if (!allDataInLeaves)
                        {
                            parent.values.Add(node.values[mid]);
                        }
                    }
                }

#if USE_CONSOLE_WRITELINE
                Console.WriteLine("InsertRestore(): Key {0} moved to parent.", node.keys[mid]);
#endif

                // Create new node for second half of split node.
                var temp = new BTreeNode<TKey, TValue>(this, node.isLeaf);

                if (leftSelfIdx + 1 < parent.children.Count)
                {
                    parent.children[leftSelfIdx + 1] = temp;
                }
                else
                {
                    parent.children.Add(temp);
                }

                temp.parent = parent;
                temp.selfIdx = leftSelfIdx + 1;

                temp.children.AddRange(node.children.Skip(mid + 1));
                temp.keys.AddRange(node.keys.Skip(mid + 1));

                if (temp.isLeaf || !allDataInLeaves)
                {
                    temp.values.AddRange(node.values.Skip(mid + 1));
                }

                if (node.isLeaf && allDataInLeaves)
                {
                    node.children.RemoveRange(mid + 2, node.children.Count - mid - 2);
                    node.keys.RemoveRange(mid + 1, node.keys.Count - mid - 1);
                    node.values.RemoveRange(mid + 1, node.values.Count - mid - 1);
                    // Now the last (rightmost) key in node.keys is the same as the key that was copied up to the parent node.
                }
                else
                {
                    node.children.RemoveRange(mid + 1, node.children.Count - mid - 1);
                    node.keys.RemoveRange(mid, node.keys.Count - mid);

                    if (!allDataInLeaves)
                    {
                        node.values.RemoveRange(mid, node.values.Count - mid);
                    }
                }

                for (var i = 0; i < temp.children.Count; ++i)
                {

                    if (temp.children[i] != null)
                    {
                        temp.children[i].selfIdx = i;
                        temp.children[i].parent = temp;
                    }
                }

#if USE_CONSOLE_WRITELINE
                Console.WriteLine("InsertRestore(): Split into {0} and {1}.", node.NumKeys, temp.NumKeys);
#endif

                node = parent;
            }
        }

        // Attempt to insert the given key into the tree, and then re-balance the tree, if necessary.

        protected override void Insert(TKey key, TValue value)
        {
            var node = AddKeyAndValue(key, value);

            if (node == null)
            {
#if USE_CONSOLE_WRITELINE
                Console.WriteLine("Cannot insert; key {0} already exists in the tree.", key);
#endif
            }
            else
            {
                InsertRestore(node);
#if VERIFY_TREE
                VerifyTree();
#endif
            }
        }

        // Re-balance a tree, starting at the given node, that has just had a key deleted from it.

        private void DeleteRestore(BTreeNode<TKey, TValue> node)
        {

            if (node.NumKeys >= treeOrder / 2)
            {
#if USE_CONSOLE_WRITELINE
                Console.WriteLine("DeleteRestore(): Node is OK; it has {0} keys.", node.NumKeys);
#endif
                return;
            }

            var parent = node.parent;

            if (parent == null) // node is the root.
            {
#if USE_CONSOLE_WRITELINE
                Console.WriteLine("DeleteRestore(): Node has no parent.");
#endif

                if (node.NumKeys == 0 && node.children[0] != null)
                {
                    root = node.children[0];
                    root.selfIdx = -1;
                    root.parent = null;
                }

                return;
            }

            BTreeNode<TKey, TValue> rightSib = null;
            BTreeNode<TKey, TValue> leftSib = null;

            if (node.selfIdx < parent.NumKeys)
            {
                rightSib = parent.children[node.selfIdx + 1];
            }

            if (node.selfIdx > 0)
            {
                leftSib = parent.children[node.selfIdx - 1];
            }

#if EXTRA_VERIFY_TREE
            root.AssertSelfIdxValidity();
            root.AssertKeyUniqueness(new HashSet<TKey>());
#endif

            if (rightSib != null && rightSib.NumKeys > treeOrder / 2)
            {
#if USE_CONSOLE_WRITELINE
                Console.WriteLine("DeleteRestore(): Borrowing from right sibling.");
#endif

                var childToMove = rightSib.children[0];

#if EXTRA_VERIFY_TREE
                node.AssertBasicNodeProperties();
                rightSib.AssertBasicNodeProperties();
                node.AssertSelfIdxValidity();
                rightSib.AssertSelfIdxValidity();
                parent.AssertSelfIdxValidity();
                root.AssertSelfIdxValidity();
#endif

                if (!node.isLeaf || !allDataInLeaves)
                {
                    node.keys.Add(parent.keys[node.selfIdx]);
                    parent.keys[node.selfIdx] = rightSib.keys[0];

                    if (!allDataInLeaves)
                    {
                        node.values.Add(parent.values[node.selfIdx]);
                        parent.values[node.selfIdx] = rightSib.values[0];
                    }
                }
                else
                {
                    node.keys.Add(rightSib.keys[0]);
                    node.values.Add(rightSib.values[0]);
                    parent.keys[node.selfIdx] = rightSib.keys[0];
                }

                node.children.Add(childToMove);
                rightSib.ShiftKeysDown(1);

                if (childToMove != null)
                {
                    childToMove.parent = node;
                    childToMove.selfIdx = node.children.Count - 1;
                }

#if EXTRA_VERIFY_TREE
                node.AssertBasicNodeProperties();
                rightSib.AssertBasicNodeProperties();
                node.AssertSelfIdxValidity();
                rightSib.AssertSelfIdxValidity();
                parent.AssertSelfIdxValidity();
                root.AssertSelfIdxValidity();
#endif
            }
            else if (leftSib != null && leftSib.NumKeys > treeOrder / 2)
            {
#if USE_CONSOLE_WRITELINE
                Console.WriteLine("DeleteRestore(): Borrowing from left sibling.");
#endif

                var childToMove = leftSib.children[leftSib.NumKeys];

#if EXTRA_VERIFY_TREE
                node.AssertBasicNodeProperties();
                leftSib.AssertBasicNodeProperties();
                node.AssertSelfIdxValidity();
                leftSib.AssertSelfIdxValidity();
                parent.AssertSelfIdxValidity();
                root.AssertSelfIdxValidity();
#endif
                node.ShiftKeysUp(0);

                if (!node.isLeaf || !allDataInLeaves)
                {
                    node.keys[0] = parent.keys[node.selfIdx - 1];
                    parent.keys[node.selfIdx - 1] = leftSib.keys[leftSib.NumKeys - 1];

                    if (!allDataInLeaves)
                    {
                        node.values[0] = parent.values[node.selfIdx - 1];
                        parent.values[node.selfIdx - 1] = leftSib.values[leftSib.NumKeys - 1];
                    }
                }
                else
                {
                    node.keys[0] = leftSib.keys[leftSib.NumKeys - 1];
                    node.values[0] = leftSib.values[leftSib.NumKeys - 1];
                }

                node.children[0] = childToMove;

                if (leftSib.isLeaf || !allDataInLeaves)
                {
                    leftSib.values.RemoveAt(leftSib.NumKeys - 1);
                }

                leftSib.keys.RemoveAt(leftSib.NumKeys - 1);

                if (leftSib.isLeaf && allDataInLeaves)
                {
                    parent.keys[node.selfIdx - 1] = leftSib.keys[leftSib.NumKeys - 1];
                }

                leftSib.children.RemoveAt(leftSib.children.Count - 1);  // Was .RemoveAt(leftSib.NumKeys), which was a bug.

                if (childToMove != null)
                {
                    childToMove.parent = node;
                    childToMove.selfIdx = 0;
                }

#if EXTRA_VERIFY_TREE
                node.AssertBasicNodeProperties();
                leftSib.AssertBasicNodeProperties();
                node.AssertSelfIdxValidity();
                leftSib.AssertSelfIdxValidity();
                parent.AssertSelfIdxValidity();
                root.AssertSelfIdxValidity();
#endif
            }
            else if (rightSib != null)
            {
#if USE_CONSOLE_WRITELINE
                Console.WriteLine("DeleteRestore(): Combining with right sibling.");
#endif
                node.CombineNodes();
#if EXTRA_VERIFY_TREE
                root.AssertSelfIdxValidity();
#endif
            }
            else
            {
#if USE_CONSOLE_WRITELINE
                Console.WriteLine("DeleteRestore(): Combining with left sibling.");
#endif
                leftSib.CombineNodes();
#if EXTRA_VERIFY_TREE
                root.AssertSelfIdxValidity();
#endif
            }

            DeleteRestore(parent);
        }

        // Attempt to delete the given key from the tree, and then re-balance the tree.

        protected override bool Delete(TKey key)
        {
            BTreeNode<TKey, TValue> node;
            int idx;

            root.FindKey(key, out node, out idx);

            if (node == null)
            {
#if USE_CONSOLE_WRITELINE
                Console.WriteLine("Cannot delete; key {0} does not exist in the tree.", key);
#endif
                return false;
            }
            else if (node.children[idx] == null)    // This will always be true if the data is only in the leaves.
            {
                node.ShiftKeysDown(idx + 1);
#if EXTRA_VERIFY_TREE
                root.AssertSelfIdxValidity();
#endif
                DeleteRestore(node);
            }
            else
            {
                BTreeNode<TKey, TValue> leaf;
                TKey returnedKey;
                TValue returnedValue;

                node.children[idx].LastKey(out leaf, out returnedKey, out returnedValue);
                node.keys[idx] = returnedKey;
                node.values[idx] = returnedValue;
#if EXTRA_VERIFY_TREE
                root.AssertSelfIdxValidity();
#endif
                DeleteRestore(leaf);
            }

#if VERIFY_TREE
            VerifyTree();
#endif
            return true;
        }

        public bool Find(BTreeNode<TKey, TValue> node, TKey keyToFind, out TValue valueThatWasFound)
        {

            if (node != null)
            {
                return node.Find(keyToFind, out valueThatWasFound);
            }
            else
            {
                valueThatWasFound = default(TValue);
                return false;
            }
        }

        protected override bool Find(TKey keyToFind, out TValue valueThatWasFound)
        {
            return Find(root, keyToFind, out valueThatWasFound);
        }

        protected override List<KeyValuePair<TKey, TValue>> InOrderTraversal()
        {
            var result = new List<KeyValuePair<TKey, TValue>>();

            root.InOrderTraversal(result);
            return result;
        }

#if VERIFY_TREE
        private void VerifyKeyOrder()
        {
            var traversal = InOrderTraversal();

            for (var i = 1; i < traversal.Count; ++i)
            {

                if (comparer.Compare(traversal[i - 1].Key, traversal[i].Key) >= 0)
                {
                    throw new Exception(string.Format("Key order error: {0} occurs before {1} in the tree.", traversal[i - 1].Key, traversal[i].Key));
                }
            }
        }

        private void VerifyTree()
        {

            if (root == null)
            {
                throw new Exception("VerifyTree(): root is null.");
            }

            root.AssertBasicSubtreeProperties();
            root.AssertSelfIdxValidity();
            root.AssertKeyUniqueness(new HashSet<TKey>());
            root.CalculateHeightsAndVerifyBalance();
            VerifyKeyOrder();
        }
#endif
    }
}
