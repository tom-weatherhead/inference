﻿#define VERIFY_TREE
//#define EXTRA_VERIFY_TREE

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Inference.Utilities
{
    public class RedBlackTreeNode<TKey, TValue>
    {
        private readonly RedBlackTree<TKey, TValue> tree;
        public TKey key;
        public TValue value;
        public bool isRed;
        public RedBlackTreeNode<TKey, TValue> parent;
        public RedBlackTreeNode<TKey, TValue> leftChild;
        public RedBlackTreeNode<TKey, TValue> rightChild;

        public RedBlackTreeNode(RedBlackTree<TKey, TValue> tree, TKey key, TValue value, bool isRed,
            RedBlackTreeNode<TKey, TValue> parent, RedBlackTreeNode<TKey, TValue> leftChild, RedBlackTreeNode<TKey, TValue> rightChild)
        {
            this.tree = tree;
            this.key = key;
            this.value = value;
            this.isRed = isRed;
            this.parent = parent;
            this.leftChild = leftChild;
            this.rightChild = rightChild;
        }

        public void ReplaceParentsReferenceToMe(RedBlackTreeNode<TKey, TValue> replacement)
        {

            if (this == tree.nil)
            {
                // tree.nil.parent is not reliable; it is used as temporary storage by the Delete() algorithm.
                throw new Exception("ReplaceParentsReferenceToMe(): this == tree.nil");
            }

            if (parent == null)
            {
                tree.root = replacement;
            }
            else if (parent.leftChild == this)
            {
                parent.leftChild = replacement;
            }
            else
            {
                parent.rightChild = replacement;
            }
        }

        /*   A            B
         *  / \          / \
         * a   B   ->   A   c
         *    / \      / \
         *   b   c    a   b
         *   
		fun RotateLeft (Br((bRed1,bRoot1,k1,x1),ltree,(Br((bRed2,_,k2,x2),rltree,rrtree)))) =
				NewNode(bRed1,bRoot1,k2,x2,NewNode(bRed2,false,k1,x1,ltree,rltree),rrtree)
			| RotateLeft _ = raise RBRotateException;
         */

        public RedBlackTreeNode<TKey, TValue> RotateLeft()
        {

            if (rightChild == tree.nil)
            {
                throw new Exception("RotateLeft() : rightChild is nil.");
            }

            var a = leftChild;
            // A == this
            var B = rightChild;
            var b = B.leftChild;
            var c = B.rightChild;

            ReplaceParentsReferenceToMe(B);
            B.parent = parent;

            B.leftChild = this;
            parent = B;

            rightChild = b;

            if (b != tree.nil)
            {
                b.parent = this;
            }

            return B;
        }

        /*     B        A
         *    / \      / \
         *   A   c -> a   B
         *  / \          / \
         * a   b        b   c
         * 
		fun RotateRight (Br((bRed1,bRoot1,k1,x1),(Br((bRed2,_,k2,x2),lltree,lrtree)),rtree)) =
				NewNode(bRed1,bRoot1,k2,x2,lltree,NewNode(bRed2,false,k1,x1,lrtree,rtree))
			| RotateRight _ = raise RBRotateException;
         */

        public RedBlackTreeNode<TKey, TValue> RotateRight()
        {

            if (leftChild == tree.nil)
            {
                throw new Exception("RotateRight() : leftChild is nil.");
            }

            var A = leftChild;
            // B == this
            var a = A.leftChild;
            var b = A.rightChild;
            var c = rightChild;

            ReplaceParentsReferenceToMe(A);
            A.parent = parent;

            A.rightChild = this;
            parent = A;

            leftChild = b;

            if (b != tree.nil)
            {
                b.parent = this;
            }

            return A;
        }

        /*
		fun RotateRightLeft (Br((bRed,bRoot,k,x),ltree,rtree)) =
				RotateLeft(NewNode(bRed,bRoot,k,x,ltree,RotateRight(rtree)))
			| RotateRightLeft _ = raise RBRotateException;
         */

        public RedBlackTreeNode<TKey, TValue> RotateRightLeft()
        {

            if (rightChild == tree.nil)
            {
                throw new Exception("RotateRightLeft() : rightChild == tree.nil");
            }

            rightChild.RotateRight();
            return RotateLeft();
        }

        /*
		fun RotateLeftRight (Br((bRed,bRoot,k,x),ltree,rtree)) =
				RotateRight(NewNode(bRed,bRoot,k,x,RotateLeft(ltree),rtree))
			| RotateLeftRight _ = raise RBRotateException;
         */

        public RedBlackTreeNode<TKey, TValue> RotateLeftRight()
        {

            if (leftChild == tree.nil)
            {
                throw new Exception("RotateLeftRight() : leftChild == tree.nil");
            }

            leftChild.RotateLeft();
            return RotateRight();
        }

#if DEAD_CODE
        /*
		fun SmartRotate (tree as Br(_,Br((true,_,_,_),Br((true,_,_,_),_,_),_),_)) =
				RotateRight(tree)
			| SmartRotate (tree as Br(_,Br((true,_,_,_),_,Br((true,_,_,_),_,_)),_)) =
				RotateLeftRight(tree)
			| SmartRotate (tree as Br(_,_,Br((true,_,_,_),Br((true,_,_,_),_,_),_))) =
				RotateRightLeft(tree)
			| SmartRotate (tree as Br(_,_,Br((true,_,_,_),_,Br((true,_,_,_),_,_)))) =
				RotateLeft(tree)
			| SmartRotate tree = tree;
         */

        public void SmartRotate(ref RedBlackTreeNode<TKey, TValue> parentsReferenceToThisNode)
        {
            var leftLeftChild = (leftChild != null) ? leftChild.leftChild : null;
            var leftRightChild = (leftChild != null) ? leftChild.rightChild : null;
            var rightLeftChild = (rightChild != null) ? rightChild.leftChild : null;
            var rightRightChild = (rightChild != null) ? rightChild.rightChild : null;

            if (leftLeftChild != null && leftChild.isRed && leftLeftChild.isRed)
            {
                leftLeftChild.isRed = false;
                RotateRight(ref parentsReferenceToThisNode);
            }
            else if (leftRightChild != null && leftChild.isRed && leftRightChild.isRed)
            {
                leftChild.isRed = false;
                RotateLeftRight(ref parentsReferenceToThisNode);
            }
            else if (rightLeftChild != null && rightChild.isRed && rightLeftChild.isRed)
            {
                rightChild.isRed = false;
                RotateRightLeft(ref parentsReferenceToThisNode);
            }
            else if (rightRightChild != null && rightChild.isRed && rightRightChild.isRed)
            {
                rightRightChild.isRed = false;
                RotateLeft(ref parentsReferenceToThisNode);
            }
        }

        /*
		fun HandleRedKids (Br((_,bRoot,k,x),Br((true,_,kl,xl),ll,lr),Br((true,_,kr,xr),rl,rr))) =
				Br((not bRoot,bRoot,k,x),
					Br((false,false,kl,xl),ll,lr),Br((false,false,kr,xr),rl,rr))
			| HandleRedKids tree = tree;
         */

        public void HandleRedChildren()
        {

            if (leftChild != null && rightChild != null && leftChild.isRed && rightChild.isRed)
            {
                isRed = true;
                leftChild.isRed = false;
                rightChild.isRed = false;
            }
        }

        /*
		fun Insert ( k, x, Br((bRed,bRoot,k1,x1),ltree,rtree) ) =
				if Order.less(k,k1) then
					Br((bRed,bRoot,k1,x1),RBInsert(k,x,ltree,false),rtree)
				else if Order.less(k1,k) then
					Br((bRed,bRoot,k1,x1),ltree,RBInsert(k,x,rtree,false))
				else
					Br((bRed,bRoot,k1,x),ltree,rtree);
         */

        public bool Insert(ref RedBlackTreeNode<TKey, TValue> parentsReferenceToThisNode, TKey keyToInsert, TValue valueToInsert)
        {
#if DEAD_CODE
            HandleRedChildren();
            SmartRotate(ref parentsReferenceToThisNode);

            var keyComparison = tree.comparer.Compare(keyToInsert, parentsReferenceToThisNode.key);

            if (keyComparison < 0)
            {
                // Insert the new key into the left subtree.
                tree.Insert(parentsReferenceToThisNode, ref parentsReferenceToThisNode.leftChild, keyToInsert, valueToInsert);
            }
            else if (keyComparison > 0)
            {
                // Insert the new key into the right subtree.
                tree.Insert(parentsReferenceToThisNode, ref parentsReferenceToThisNode.rightChild, keyToInsert, valueToInsert);
            }
            else
            {
                parentsReferenceToThisNode.key = keyToInsert;
                parentsReferenceToThisNode.value = valueToInsert;
            }

            parentsReferenceToThisNode.SmartRotate(ref parentsReferenceToThisNode);
#else
            var keyComparison = tree.comparer.Compare(keyToInsert, parentsReferenceToThisNode.key);
            bool rotate;

            if (keyComparison < 0)
            {
                // Insert the new key into the left subtree.
                rotate = tree.Insert(parentsReferenceToThisNode, ref leftChild, keyToInsert, valueToInsert);
            }
            else if (keyComparison > 0)
            {
                // Insert the new key into the right subtree.
                rotate = tree.Insert(parentsReferenceToThisNode, ref rightChild, keyToInsert, valueToInsert);
            }
            else
            {
                key = keyToInsert;
                value = valueToInsert;
                return false;
            }

            if (rotate)
            {
                SmartRotate(ref parentsReferenceToThisNode);
            }
            else if (isRed)
            {
                return true;
            }
            else
            {
                HandleRedChildren();
            }

            return false;
#endif
        }
#endif

        public RedBlackTreeNode<TKey, TValue> FindNode(TKey keyToFind)
        {

            if (this == tree.nil)
            {
                return this;
            }

            var keyComparison = tree.comparer.Compare(keyToFind, key);

            if (keyComparison < 0)
            {
                return leftChild.FindNode(keyToFind);
            }
            else if (keyComparison > 0)
            {
                return rightChild.FindNode(keyToFind);
            }
            else
            {
                return this;
            }
        }

        public void InOrderTraversal(List<KeyValuePair<TKey, TValue>> result)
        {

            if (leftChild != tree.nil)
            {
                leftChild.InOrderTraversal(result);
            }

            result.Add(new KeyValuePair<TKey, TValue>(key, value));

            if (rightChild != tree.nil)
            {
                rightChild.InOrderTraversal(result);
            }
        }

        public void InOrderTraversalOfKeysAndColours(StringBuilder sb)
        {

            if (leftChild != tree.nil)
            {
                leftChild.InOrderTraversalOfKeysAndColours(sb);
            }

            sb.AppendFormat("; key {0} is {1}", key, isRed ? "red" : "black");

            if (rightChild != tree.nil)
            {
                rightChild.InOrderTraversalOfKeysAndColours(sb);
            }
        }
    }

    public class RedBlackTree<TKey, TValue> : DictionaryTreeBase<TKey, TValue>
    {
        public RedBlackTreeNode<TKey, TValue> root;
        public readonly IComparer<TKey> comparer;
        public RedBlackTreeNode<TKey, TValue> nil; // nil is needed by our Delete() algorithm.

        public RedBlackTree(IComparer<TKey> comparer = null, IEnumerable<KeyValuePair<TKey, TValue>> source = null)
        {
            this.comparer = comparer ?? Comparer<TKey>.Default;
            this.nil = new RedBlackTreeNode<TKey, TValue>(this, default(TKey), default(TValue), false, null, null, null);    // nil is black.
            Clear();

            if (source != null)
            {
                this.AddItems(source);
            }
        }

        public RedBlackTree(IEnumerable<KeyValuePair<TKey, TValue>> source)
            : this(null, source)
        {
        }

        public override void Clear()
        {
            root = nil;
        }

#if VERIFY_TREE
        private int CalculateAndVerifyHeightsAndBalance(RedBlackTreeNode<TKey, TValue> node)
        {

            if (node == nil)
            {
                return 0;
            }

            var leftHeight = CalculateAndVerifyHeightsAndBalance(node.leftChild);
            var rightHeight = CalculateAndVerifyHeightsAndBalance(node.rightChild);

            if (leftHeight > 2 * rightHeight + 1 || rightHeight > 2 * leftHeight + 1)
            {
                throw new Exception(string.Format("Subtree imbalance: Left height = {0}; right height = {1}.", leftHeight, rightHeight));
            }

            return Math.Max(leftHeight, rightHeight) + 1;
        }

        public int CalculateAndVerifyBlackHeight(RedBlackTreeNode<TKey, TValue> node)
        {

            if (node == nil)
            {
                return 0;
            }

            var leftHeight = CalculateAndVerifyBlackHeight(node.leftChild);
            var rightHeight = CalculateAndVerifyBlackHeight(node.rightChild);

            if (leftHeight != rightHeight)
            {
                var sb = new StringBuilder();

                node.InOrderTraversalOfKeysAndColours(sb);
                throw new Exception(string.Format("Black height imbalance: Left height = {0}; right height = {1}; key = {2}{3}.",
                    leftHeight, rightHeight, node.key, sb));
            }

            return leftHeight + (node.isRed ? 0 : 1);
        }

        private void VerifyNodeColours(RedBlackTreeNode<TKey, TValue> node)
        {

            if (node == nil)
            {
                return;
            }

            VerifyNodeColours(node.leftChild);
            VerifyNodeColours(node.rightChild);

            if (node.isRed && (node.leftChild.isRed || node.rightChild.isRed))
            {
                throw new Exception("VerifyNodeColours(): A red node has at least one red child.");
            }
        }

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
            CalculateAndVerifyHeightsAndBalance(root);
            CalculateAndVerifyBlackHeight(root);
            VerifyNodeColours(root);
            VerifyKeyOrder();
        }
#endif

#if DEAD_CODE
        public bool Insert(RedBlackTreeNode<TKey, TValue> parent, ref RedBlackTreeNode<TKey, TValue> parentsReferenceToThisNode, TKey key, TValue value)
        {

            if (parentsReferenceToThisNode == null)
            {
                var newNodeIsRoot = root == null;

                parentsReferenceToThisNode = new RedBlackTreeNode<TKey, TValue>(this, key, value, true, newNodeIsRoot, parent, null, null);
                return false;
            }
            else
            {
                return parentsReferenceToThisNode.Insert(ref parentsReferenceToThisNode, key, value);
            }
        }
#else
        // See "Data Structures & Their Algorithms", page 237.

        public void InsertHelper(TKey key, TValue value)
        {
            var P = root;
            var S = new Stack<object>();
            RedBlackTreeNode<TKey, TValue> parentOfNewNode = null;
            var lastD = 0;

            while (P != nil)
            {
                var keyComparison = comparer.Compare(key, P.key);

                if (keyComparison == 0)
                {
                    P.value = value;
                    return;
                }

                lastD = (keyComparison < 0) ? -1 : 1;
                S.Push(P);
                S.Push(lastD);
                parentOfNewNode = P;

                if (keyComparison < 0)
                {
                    P = P.leftChild;
                }
                else
                {
                    P = P.rightChild;
                }
            }

            P = new RedBlackTreeNode<TKey, TValue>(this, key, value, true, parentOfNewNode, nil, nil);

            if (parentOfNewNode == null)
            {
                root = P;
            }
            else if (lastD < 0)
            {
                parentOfNewNode.leftChild = P;
            }
            else
            {
                parentOfNewNode.rightChild = P;
            }

            for (; ; )
            {

                if (S.Count == 0)
                {
                    P.isRed = false;
                    return;
                }

                // P is red.
                // Q is P's parent; R is P's grandparent.
                int d = (int)S.Pop();
                RedBlackTreeNode<TKey, TValue> Q = (RedBlackTreeNode<TKey, TValue>)S.Pop();

                if (Q.isRed)
                {
                    // Q is red, so it is not the root and the stack is not empty.
                    int dPrime = (int)S.Pop();
                    RedBlackTreeNode<TKey, TValue> R = (RedBlackTreeNode<TKey, TValue>)S.Pop();
                    var RIsRoot = R.parent == null;
                    var RParent = R.parent;

                    if (d == dPrime)
                    {
                        // Single rotation.
                        P.isRed = false;

                        if (d < 0)
                        {
                            R = R.RotateRight();
#if EXTRA_VERIFY_TREE
                            CalculateAndVerifyBlackHeight(R);
#endif
                        }
                        else
                        {
                            R = R.RotateLeft();
#if EXTRA_VERIFY_TREE
                            CalculateAndVerifyBlackHeight(R);
#endif
                        }
                    }
                    else
                    {
                        // Double rotation.
                        Q.isRed = false;

                        if (d > 0)
                        {
                            R = R.RotateLeftRight();
#if EXTRA_VERIFY_TREE
                            CalculateAndVerifyBlackHeight(R);
#endif
                        }
                        else
                        {
                            R = R.RotateRightLeft();
#if EXTRA_VERIFY_TREE
                            CalculateAndVerifyBlackHeight(R);
#endif
                        }
                    }

                    if (RIsRoot)
                    {
                        root = R;
                    }
                    else if ((int)S.Peek() < 0)
                    {
                        RParent.leftChild = R;
                    }
                    else
                    {
                        RParent.rightChild = R;
                    }

                    P = R;
                }
                else
                {
                    var C = (d > 0) ? Q.leftChild : Q.rightChild;

                    if (!C.isRed)
                    {
                        return;
                    }

                    C.isRed = false;
                    P.isRed = false;
                    Q.isRed = true;
                    P = Q;
                }
            }
        }
#endif

        protected override void Insert(TKey key, TValue value)
        {
#if DEAD_CODE
            Insert(null, ref root, key, value);
            root.isRed = false;
#else
            InsertHelper(key, value);
#endif
#if VERIFY_TREE
            VerifyTree();
#endif
        }

        // See "Introduction to Algorithms", page 248.

        private RedBlackTreeNode<TKey, TValue> TreeMinimum(RedBlackTreeNode<TKey, TValue> x)
        {

            while (x.leftChild != nil)
            {
                x = x.leftChild;
            }

            return x;
        }

        // See "Introduction to Algorithms", page 249.

        private RedBlackTreeNode<TKey, TValue> TreeSuccessor(RedBlackTreeNode<TKey, TValue> x)
        {

            if (x.rightChild != nil)
            {
                return TreeMinimum(x.rightChild);
            }

            var y = x.parent;

            while (y != null && x == y.rightChild)
            {
                x = y;
                y = y.parent;
            }

            if (y == null)
            {
                throw new Exception("TreeSuccessor(): No successor found.");
            }

            return y;
        }

        // See "Introduction to Algorithms", page 274.

        private void DeleteFixUp(RedBlackTreeNode<TKey, TValue> x)
        {

            while (x != root && !x.isRed)
            {

                if (x == x.parent.leftChild)
                {
                    var w = x.parent.rightChild;

                    if (w.isRed)
                    {
                        w.isRed = false;
                        x.parent.isRed = true;
                        x.parent.RotateLeft();
                        w = x.parent.rightChild;
                    }

                    if (!w.leftChild.isRed && !w.rightChild.isRed)
                    {
                        w.isRed = true;
                        x = x.parent;
                    }
                    else
                    {

                        if (!w.rightChild.isRed)
                        {
                            w.leftChild.isRed = false;
                            w.isRed = true;
                            w.RotateRight();
                            w = x.parent.rightChild;
                        }

                        w.isRed = x.parent.isRed;
                        x.parent.isRed = false;
                        w.rightChild.isRed = false;
                        x.parent.RotateLeft();
                        x = root;
                    }
                }
                else
                {
                    var w = x.parent.leftChild;

                    if (w.isRed)
                    {
                        w.isRed = false;
                        x.parent.isRed = true;
                        x.parent.RotateRight();
                        w = x.parent.leftChild;
                    }

                    if (!w.leftChild.isRed && !w.rightChild.isRed)
                    {
                        w.isRed = true;
                        x = x.parent;
                    }
                    else
                    {

                        if (!w.leftChild.isRed)
                        {
                            w.rightChild.isRed = false;
                            w.isRed = true;
                            w.RotateLeft();
                            w = x.parent.leftChild;
                        }

                        w.isRed = x.parent.isRed;
                        x.parent.isRed = false;
                        w.leftChild.isRed = false;
                        x.parent.RotateRight();
                        x = root;
                    }
                }
#if EXTRA_VERIFY_TREE
                CalculateAndVerifyBlackHeight(x);
#endif
            }

            x.isRed = false;
        }

        // See "Introduction to Algorithms", page 273.

        protected override bool Delete(TKey keyToDelete)
        {
            var z = root.FindNode(keyToDelete);

            if (z == nil)
            {
                return false;
            }

            RedBlackTreeNode<TKey, TValue> y;

            if (z.leftChild == nil || z.rightChild == nil)
            {
                y = z;
            }
            else
            {
                y = TreeSuccessor(z);
            }

            RedBlackTreeNode<TKey, TValue> x;

            if (y.leftChild != nil)
            {
                x = y.leftChild;
            }
            else
            {
                x = y.rightChild;
            }

            x.parent = y.parent;

            // TODO: Try y.ReplaceParentsReferenceToMe(x); instead of the below.
            if (y.parent == null)
            {
                root = x;
            }
            else if (y == y.parent.leftChild)
            {
                y.parent.leftChild = x;
            }
            else
            {
                y.parent.rightChild = x;
            }

            if (y != z)
            {
                z.key = y.key;
                z.value = y.value;
            }

            if (!y.isRed)
            {
                DeleteFixUp(x);
            }

#if VERIFY_TREE
            VerifyTree();
#endif
            return true;
        }

        protected override bool Find(TKey keyToFind, out TValue valueThatWasFound)
        {
            var node = root.FindNode(keyToFind);

            if (node != nil)
            {
                valueThatWasFound = node.value;
                return true;
            }
            else
            {
                valueThatWasFound = default(TValue);
                return false;
            }
        }

        protected override List<KeyValuePair<TKey, TValue>> InOrderTraversal()
        {
            var result = new List<KeyValuePair<TKey, TValue>>();

            if (root != nil)
            {
                root.InOrderTraversal(result);
            }

            return result;
        }
    }
}
