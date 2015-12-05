﻿#define VERIFY_TREE
//#define EXTRA_VERIFY_TREE
#define SIMPLE_BALANCE_DOUBLE_ROTATE_TEST
//#define RECURSIVE_SIMPLE_BALANCE
//#define JOIN_PARENT_TO_CHILD
//#define EXHAUSTIVE_CALCULATE_HEIGHT
#define SUPPRESS_TEST_2
#define DELETE_USING_IN_ORDER_PREDECESSOR

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Inference.Utilities
{
    /*
    public interface IDictionaryTree<TKey, TValue>
    {
        void Clear();
        void Insert(TKey key, TValue value);
        bool Delete(TKey keyToDelete);
        bool Find(TKey keyToFind, out TValue valueThatWasFound);
        List<KeyValuePair<TKey, TValue>> InOrderTraversal();
    }
     */

    public class AVLTreeNode<TKey, TValue>
    {
        private readonly AVLTree<TKey, TValue> tree;
        private TKey key;
        private TValue value;
        private AVLTreeNode<TKey, TValue> parent;
        private AVLTreeNode<TKey, TValue> leftChild;
        private AVLTreeNode<TKey, TValue> rightChild;
        public int height;

        public AVLTreeNode(AVLTree<TKey, TValue> tree, TKey key, TValue value,
            AVLTreeNode<TKey, TValue> parent, AVLTreeNode<TKey, TValue> leftChild, AVLTreeNode<TKey, TValue> rightChild, int height)
        {
            this.tree = tree;
            this.key = key;
            this.value = value;
            this.parent = parent;
            this.leftChild = leftChild;
            this.rightChild = rightChild;
            this.height = height;
        }

        public void CalculateHeight()
        {
            var oldHeight = height;

            height = Math.Max(tree.Height(leftChild), tree.Height(rightChild)) + 1;

            if (height != oldHeight && parent != null)
            {
                parent.CalculateHeight();
            }
        }

#if EXHAUSTIVE_CALCULATE_HEIGHT
        public void ExhaustiveCalculateHeight()
        {

            if (leftChild != null)
            {
                leftChild.ExhaustiveCalculateHeight();
            }

            if (rightChild != null)
            {
                rightChild.ExhaustiveCalculateHeight();
            }

            height = Math.Max(tree.Height(leftChild), tree.Height(rightChild)) + 1;
        }
#endif

        public void ReplaceParentsReferenceToMe(AVLTreeNode<TKey, TValue> replacement)
        {

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
		fun RotateLeft Lf = Lf
			| RotateLeft (Br(_,_,Lf)) = Lf  (* Error *)
			| RotateLeft (Br((k1,_,x1),ltree,(Br((k2,_,x2),rltree,rrtree)))) =
				NewNode(k2,x2,NewNode(k1,x1,ltree,rltree),rrtree);
         */

        public AVLTreeNode<TKey, TValue> RotateLeft()
        {

            if (rightChild == null)
            {
                throw new Exception("RotateLeft() : rightChild is null.");
            }

#if EXTRA_VERIFY_TREE
            tree.VerifyTreeHeights();
#endif

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

            if (b != null)
            {
                b.parent = this;
            }

            CalculateHeight();
            B.CalculateHeight();
            tree.CalculateHeight(B.parent);   // This fixed a bug.

#if EXTRA_VERIFY_TREE
            tree.VerifyTreeHeights();
#endif

            return B;
        }

        /*     B        A
         *    / \      / \
         *   A   c -> a   B
         *  / \          / \
         * a   b        b   c
         * 
		fun RotateRight Lf = Lf
			| RotateRight (Br(_,Lf,_)) = Lf  (* Error *)
			| RotateRight (Br((k1,_,x1),(Br((k2,_,x2),lltree,lrtree)),rtree)) =
				NewNode(k2,x2,lltree,NewNode(k1,x1,lrtree,rtree));
         */

        public AVLTreeNode<TKey, TValue> RotateRight()
        {

            if (leftChild == null)
            {
                throw new Exception("RotateRight() : leftChild is null.");
            }

#if EXTRA_VERIFY_TREE
            tree.VerifyTreeHeights();
#endif

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

            if (b != null)
            {
                b.parent = this;
            }

            CalculateHeight();
            A.CalculateHeight();
            tree.CalculateHeight(A.parent);   // This fixed a bug.

#if EXTRA_VERIFY_TREE
            tree.VerifyTreeHeights();
#endif

            return A;
        }

        /*
		fun RotateRightLeft Lf = Lf
			| RotateRightLeft (Br((k,_,x),ltree,rtree)) =
				RotateLeft(NewNode(k,x,ltree,RotateRight(rtree)));
         */

        public AVLTreeNode<TKey, TValue> RotateRightLeft()
        {

            if (rightChild == null)
            {
                throw new Exception("RotateRightLeft() : rightChild == tree.nil");
            }

            rightChild.RotateRight();
            return RotateLeft();
        }

        /*
		fun RotateLeftRight Lf = Lf
			| RotateLeftRight (Br((k,_,x),ltree,rtree)) =
				RotateRight(NewNode(k,x,RotateLeft(ltree),rtree));
         */

        public AVLTreeNode<TKey, TValue> RotateLeftRight()
        {

            if (leftChild == null)
            {
                throw new Exception("RotateLeftRight() : leftChild == tree.nil");
            }

            leftChild.RotateLeft();
            return RotateRight();
        }

        /*
		fun AVLInsert (k, x, Lf) = Br( (k, 1, x), Lf, Lf )
		  | AVLInsert (k, x, Br((k1, h1, x1), ltree, rtree)) =
				if Order.less(k,k1) then  (* x goes into ltree *)
		      let
				    val newltree = AVLInsert(k,x,ltree)
						val newtree = NewNode(k1,x1,newltree,rtree)
		      in
				    if TreeHeight(newltree) <= TreeHeight(rtree) + 1 then
						  newtree
		        else if Order.less(k,NodeKey(newltree)) then
				      RotateRight(newtree)
						else
		          RotateLeftRight(newtree)
				  end
		    else if Order.less(k1,k) then
				  let
						val newrtree = AVLInsert(k,x,rtree)
		        val newtree = NewNode(k1,x1,ltree,newrtree)
				  in
						if TreeHeight(newrtree) <= TreeHeight(ltree) + 1 then
		          newtree
				    else if Order.less(NodeKey(newrtree),k) then
						  RotateLeft(newtree)
		        else
				      RotateRightLeft(newtree)
		      end
				else
					Br((k1, h1, x), ltree, rtree); (* Just replace the contents *)
         */

        public void Insert(TKey keyToInsert, TValue valueToInsert)
        {
            var keyComparison = tree.comparer.Compare(keyToInsert, key);

            if (keyComparison < 0)
            {
                // Insert the new key into the left subtree.
                tree.Insert(this, ref leftChild, keyToInsert, valueToInsert);
                CalculateHeight();

                if (tree.Height(leftChild) > tree.Height(rightChild) + 1)
                {

                    if (tree.comparer.Compare(keyToInsert, leftChild.key) < 0)
                    {
                        RotateRight();
                    }
                    else
                    {
                        RotateLeftRight();
                    }
                }
            }
            else if (keyComparison > 0)
            {
                // Insert the new key into the right subtree.
                tree.Insert(this, ref rightChild, keyToInsert, valueToInsert);
                CalculateHeight();

                if (tree.Height(rightChild) > tree.Height(leftChild) + 1)
                {

                    if (tree.comparer.Compare(keyToInsert, rightChild.key) > 0)
                    {
                        RotateLeft();
                    }
                    else
                    {
                        RotateRightLeft();
                    }
                }
            }
            else
            {
                key = keyToInsert;
                value = valueToInsert;
            }
        }

        /*
		fun SimpleBalance Lf = Lf
		  | SimpleBalance (Br((k,h,x),ltree,rtree)) =
				let
		      val tree = Br((k,h,x),ltree,rtree)
				  val lh = TreeHeight( ltree )
		      val rh = TreeHeight( rtree )
				in
		      if lh > rh + 1 then
				    RotateRight( tree )
		      else if rh > lh + 1 then
				    RotateLeft( tree )
		      else
				    tree
				end
         */

        public void SimpleBalance()
        {
#if SUPPRESS_TEST_1
#if EXHAUSTIVE_CALCULATE_HEIGHT
            tree.ExhaustiveCalculateHeight();   // Temporary test.
#endif
#endif

            var leftHeight = tree.Height(leftChild);
            var rightHeight = tree.Height(rightChild);

            if (leftHeight > rightHeight + 1)
            {
                /*
                if (leftHeight > rightHeight + 2)
                {
                    throw new Exception(string.Format("SimpleBalance(): leftHeight = {0}; rightHeight = {1}.", leftHeight, rightHeight));
                }
                 */

#if SIMPLE_BALANCE_DOUBLE_ROTATE_TEST
                if (tree.Height(leftChild.leftChild) < tree.Height(leftChild.rightChild))
                {
                    RotateLeftRight();
                }
                else
                {
                    RotateRight();
                }
#else
                RotateRight(ref parentsReferenceToThisNode);
#endif
#if RECURSIVE_SIMPLE_BALANCE
                tree.SimpleBalance(ref parentsReferenceToThisNode); // Temporary test.
#endif
#if EXTRA_VERIFY_TREE
                tree.VerifySubtree(parentsReferenceToThisNode);
#endif
            }
            else if (rightHeight > leftHeight + 1)
            {
                /*
                if (rightHeight > leftHeight + 2)
                {
                    throw new Exception(string.Format("SimpleBalance(): leftHeight = {0}; rightHeight = {1}.", leftHeight, rightHeight));
                }
                 */

#if SIMPLE_BALANCE_DOUBLE_ROTATE_TEST
                if (tree.Height(rightChild.leftChild) > tree.Height(rightChild.rightChild))
                {
                    RotateRightLeft();
                }
                else
                {
                    RotateLeft();
                }
#else
                RotateLeft(ref parentsReferenceToThisNode);
#endif
#if RECURSIVE_SIMPLE_BALANCE
                tree.SimpleBalance(ref parentsReferenceToThisNode); // Temporary test.
#endif
#if EXTRA_VERIFY_TREE
                tree.VerifySubtree(parentsReferenceToThisNode);
#endif
            }
        }

#if JOIN_PARENT_TO_CHILD
        private static void JoinParentToChild(ref AVLTreeNode<T> parentsReferenceToThisNode, AVLTreeNode<T> child, AVLTree<T> tree)
        {
            var parentWhoseHeightToCalculate = parentsReferenceToThisNode.parent;

            if (child != null)
            {
                child.parent = parentsReferenceToThisNode.parent;
            }

            parentsReferenceToThisNode = child;
            //tree.CalculateHeight(parentWhoseHeightToCalculate);
            tree.ExhaustiveCalculateHeight();
#if EXTRA_VERIFY_TREE     // The tree is not balanced at this point.
            tree.VerifySubtree(parentsReferenceToThisNode);
#endif
        }
#endif

        /*
		fun AVLDelete (_, Lf) = Lf
		  | AVLDelete (k, (Br((k1,h1,x1),ltree,rtree))) =
				if Order.less(k,k1) then
		      SimpleBalance( NewNode( k1, x1, AVLDelete( k, ltree ), rtree ) )
				else if Order.less(k1,k) then
		      SimpleBalance( NewNode( k1, x1, ltree, AVLDelete( k, rtree ) ) )
				else if IsEmpty(ltree) then
		      rtree
				else if IsEmpty(rtree) then
		      ltree
				else if TreeHeight(ltree) < TreeHeight(rtree) then
		      AVLDelete( k, RotateLeft( Br((k1,h1,x1),ltree,rtree) ) )
				else
		      AVLDelete( k, RotateRight( Br((k1,h1,x1),ltree,rtree) ) );
         */

        public bool Delete(TKey keyToDelete)
        {
            var result = true;
            var keyComparison = tree.comparer.Compare(keyToDelete, key);

            if (keyComparison < 0)
            {
                result = tree.Delete(leftChild, keyToDelete);
#if !SUPPRESS_TEST_2
                tree.CalculateHeight(parentsReferenceToThisNode);
#endif
#if EXTRA_VERIFY_TREE
                tree.VerifySubtreeHeights(parentsReferenceToThisNode);
#endif
#if !SUPPRESS_TEST_1
#if EXHAUSTIVE_CALCULATE_HEIGHT
                tree.ExhaustiveCalculateHeight();   // Temporary test.
#endif
#endif
                SimpleBalance();
#if EXTRA_VERIFY_TREE
                tree.VerifySubtree(parentsReferenceToThisNode);
                tree.VerifyTreeHeights();
#endif
            }
            else if (keyComparison > 0)
            {
                result = tree.Delete(rightChild, keyToDelete);
#if !SUPPRESS_TEST_2
                tree.CalculateHeight(parentsReferenceToThisNode);
#endif
#if EXTRA_VERIFY_TREE
                tree.VerifySubtreeHeights(parentsReferenceToThisNode);
#endif
#if !SUPPRESS_TEST_1
#if EXHAUSTIVE_CALCULATE_HEIGHT
                tree.ExhaustiveCalculateHeight();   // Temporary test.
#endif
#endif
                SimpleBalance();
#if EXTRA_VERIFY_TREE
                tree.VerifySubtree(parentsReferenceToThisNode);
                tree.VerifyTreeHeights();
#endif
            }
            else if (leftChild == null)
            {
#if !JOIN_PARENT_TO_CHILD

                if (rightChild != null)
                {
                    rightChild.parent = parent;
                }

                ReplaceParentsReferenceToMe(rightChild);
                tree.CalculateHeight(parent);
#if EXTRA_VERIFY_TREE     // The tree is not balanced at this point.
                tree.VerifySubtree(parentsReferenceToThisNode);
                tree.VerifyTreeHeights();
#endif
#else
                JoinParentToChild(ref parentsReferenceToThisNode, rightChild, tree);
#endif
            }
            else if (rightChild == null)
            {
#if !JOIN_PARENT_TO_CHILD
                leftChild.parent = parent;
                ReplaceParentsReferenceToMe(leftChild);
                tree.CalculateHeight(parent);
#if EXTRA_VERIFY_TREE     // The tree is not balanced at this point.
                tree.VerifySubtree(parentsReferenceToThisNode);
                tree.VerifyTreeHeights();
#endif
#else
                JoinParentToChild(ref parentsReferenceToThisNode, leftChild, tree);
#endif
            }
#if DELETE_USING_IN_ORDER_PREDECESSOR
            else
            {
                leftChild.RemoveRightmostNodeAndReturnKeyAndValue(out key, out value);
                SimpleBalance();
            }
#else
            else if (tree.Height(leftChild) < tree.Height(rightChild))
            {
                RotateLeft(ref parentsReferenceToThisNode);
                tree.Delete(ref parentsReferenceToThisNode, keyToDelete);   // Use "tree." because parentsReferenceToThisNode might no longer be = to this.
                //tree.SimpleBalance(ref parentsReferenceToThisNode);     // Temporary test.
#if EXTRA_VERIFY_TREE
                tree.VerifySubtree(parentsReferenceToThisNode);
                tree.VerifyTreeHeights();
#endif
            }
            else
            {
                RotateRight(ref parentsReferenceToThisNode);
                tree.Delete(ref parentsReferenceToThisNode, keyToDelete);   // Use "tree." because parentsReferenceToThisNode might no longer be = to this.
                //tree.SimpleBalance(ref parentsReferenceToThisNode);     // Temporary test.
#if EXTRA_VERIFY_TREE
                tree.VerifySubtree(parentsReferenceToThisNode);
                tree.VerifyTreeHeights();
#endif
            }
#endif

            return result;
        }

#if DELETE_USING_IN_ORDER_PREDECESSOR
        private void RemoveRightmostNodeAndReturnKeyAndValue(out TKey keyToReturn, out TValue valueToReturn)
        {

            if (rightChild == null)
            {

                if (leftChild != null)
                {
                    leftChild.parent = parent;
                }

                ReplaceParentsReferenceToMe(leftChild);
                tree.CalculateHeight(parent);
                keyToReturn = key;
                valueToReturn = value;
            }
            else
            {
                rightChild.RemoveRightmostNodeAndReturnKeyAndValue(out keyToReturn, out valueToReturn);
                SimpleBalance();
            }
        }
#endif

        public bool Find(TKey keyToFind, out TValue valueThatWasFound)
        {
            var keyComparison = tree.comparer.Compare(keyToFind, key);

            if (keyComparison < 0)
            {
                return tree.Find(leftChild, keyToFind, out valueThatWasFound);
            }
            else if (keyComparison > 0)
            {
                return tree.Find(rightChild, keyToFind, out valueThatWasFound);
            }
            else
            {
                valueThatWasFound = value;
                return true;
            }
        }

#if VERIFY_TREE
        /*
		fun AVLVerifyTree( Lf ) = true
		  | AVLVerifyTree( Br( (k, h, _), ltree, rtree ) ) =
				let
		      val lh = TreeHeight( ltree )
				  val rh = TreeHeight( rtree )
		    in
				  h >= lh + 1 andalso h >= rh + 1 andalso
		      ( (h = lh + 1 andalso rh + 1 >= lh) orelse
				    (h = rh + 1 andalso lh + 1 >= rh) ) andalso
					( IsEmpty(ltree) orelse Order.less(NodeKey(ltree),k) ) andalso
					( IsEmpty(rtree) orelse Order.less(k,NodeKey(rtree)) ) andalso
		      AVLVerifyTree( ltree ) andalso AVLVerifyTree( rtree )
				end
         */

        public void VerifySubtree()
        {
            tree.VerifySubtree(leftChild);
            tree.VerifySubtree(rightChild);

            var leftHeight = tree.Height(leftChild);
            var rightHeight = tree.Height(rightChild);

            if (height < leftHeight + 1)
            {
                throw new Exception(string.Format("VerifySubtree(): height = {0}; leftHeight = {1}.", height, leftHeight));
            }
            else if (height < rightHeight + 1)
            {
                throw new Exception(string.Format("VerifySubtree(): height = {0}; rightHeight = {1}.", height, rightHeight));
            }
            else if (!(height == leftHeight + 1 && rightHeight + 1 >= leftHeight) &&
                     !(height == rightHeight + 1 && leftHeight + 1 >= rightHeight))
            {
                throw new Exception(string.Format("VerifySubtree(): Height error: height = {0}; leftHeight = {1}; rightHeight = {2}; key = {3}.",
                    height, leftHeight, rightHeight, key));
            }
            else if (leftChild != null && tree.comparer.Compare(leftChild.key, key) >= 0)
            {
                throw new Exception(string.Format("VerifySubtree(): Key error: leftChild.key = {0}; key = {1}.", leftChild.key, key));
            }
            else if (rightChild != null && tree.comparer.Compare(key, rightChild.key) >= 0)
            {
                throw new Exception(string.Format("VerifySubtree(): Key error: key = {0}; rightChild.key = {1}.", key, rightChild.key));
            }
        }
#endif

#if EXTRA_VERIFY_TREE
        public void VerifySubtreeHeights()
        {
            tree.VerifySubtreeHeights(leftChild);
            tree.VerifySubtreeHeights(rightChild);

            var leftHeight = tree.Height(leftChild);
            var rightHeight = tree.Height(rightChild);

            if (height != Math.Max(leftHeight, rightHeight) + 1)
            {
                throw new Exception(string.Format("VerifySubtreeHeights(): height = {0}; leftHeight = {1}; rightHeight = {2}.",
                    height, leftHeight, rightHeight));
            }
        }
#endif

        public void InOrderTraversal(List<KeyValuePair<TKey, TValue>> result)
        {

            if (leftChild != null)
            {
                leftChild.InOrderTraversal(result);
            }

            result.Add(new KeyValuePair<TKey, TValue>(key, value));

            if (rightChild != null)
            {
                rightChild.InOrderTraversal(result);
            }
        }
    }

    public class AVLTree<TKey, TValue> : DictionaryTreeBase<TKey, TValue>
    {
        public AVLTreeNode<TKey, TValue> root;
        public readonly IComparer<TKey> comparer;

        public AVLTree(IComparer<TKey> comparer = null, IEnumerable<KeyValuePair<TKey, TValue>> source = null)
        {
            this.comparer = comparer ?? Comparer<TKey>.Default;
            Clear();

            if (source != null)
            {
                this.AddItems(source);
            }
        }

        public AVLTree(IEnumerable<KeyValuePair<TKey, TValue>> source)
            : this(null, source)
        {
        }

        public override void Clear()
        {
            root = null;
        }

        public int Height(AVLTreeNode<TKey, TValue> node)
        {

            if (node == null)
            {
                return 0;
            }
            else
            {
                return node.height;
            }
        }

        public void CalculateHeight(AVLTreeNode<TKey, TValue> node)
        {

            if (node != null)
            {
                node.CalculateHeight();
            }
        }

#if EXHAUSTIVE_CALCULATE_HEIGHT
        public void ExhaustiveCalculateHeight()
        {

            if (root != null)
            {
                root.ExhaustiveCalculateHeight();
            }
        }
#endif

        public void Insert(AVLTreeNode<TKey, TValue> parent, ref AVLTreeNode<TKey, TValue> parentsReferenceToThisNode, TKey key, TValue value)
        {

            if (parentsReferenceToThisNode == null)
            {
                parentsReferenceToThisNode = new AVLTreeNode<TKey, TValue>(this, key, value, parent, null, null, 1);
            }
            else
            {
                parentsReferenceToThisNode.Insert(key, value);
            }
        }

        protected override void Insert(TKey key, TValue value)
        {
            Insert(null, ref root, key, value);
#if VERIFY_TREE
            VerifyTree();
#endif
        }

        public bool Delete(AVLTreeNode<TKey, TValue> node, TKey keyToDelete)
        {

            if (node != null)
            {
                return node.Delete(keyToDelete);
            }
            else
            {
                return false;
            }
        }

        protected override bool Delete(TKey keyToDelete)
        {
            var result = Delete(root, keyToDelete);

#if VERIFY_TREE
            VerifyTree();
#endif
            return result;
        }

        public bool Find(AVLTreeNode<TKey, TValue> node, TKey keyToFind, out TValue valueThatWasFound)
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

#if VERIFY_TREE
        public void VerifySubtree(AVLTreeNode<TKey, TValue> node)
        {

            if (node != null)
            {
                node.VerifySubtree();
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

        public void VerifyTree()
        {
            VerifySubtree(root);
            VerifyKeyOrder();
        }
#endif

#if EXTRA_VERIFY_TREE
        public void VerifySubtreeHeights(AVLTreeNode<T> node)
        {

            if (node != null)
            {
                node.VerifySubtreeHeights();
            }
        }

        public void VerifyTreeHeights()
        {
            VerifySubtreeHeights(root);
        }
#endif

        protected override List<KeyValuePair<TKey, TValue>> InOrderTraversal()
        {
            var result = new List<KeyValuePair<TKey, TValue>>();

            if (root != null)
            {
                root.InOrderTraversal(result);
            }

            return result;
        }
    }
}
