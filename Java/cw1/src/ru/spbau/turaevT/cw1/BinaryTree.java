package ru.spbau.turaevT.cw1;

import java.text.MessageFormat;

/**
 * Represents BST
 *
 * @param <T> type of elements in the BST
 * @author Turaev Timur
 * @version 1.0
 */
public class BinaryTree<T extends Comparable<? super T>> {
    private BinaryTree<T> left = null;
    private BinaryTree<T> right = null;
    private BinaryTree<T> parent = null;
    private T value = null;

    public void insert(T value) throws BinaryTreeInsertionException {
        if (this.value == null) {
            this.value = value;
            return;
        }
        if (this.value.compareTo(value) == 0)
            throw new BinaryTreeInsertionException(MessageFormat.format("duplicate value: {0}", value));

        if (value.compareTo(this.value) < 0) {
            if (left == null) {
                left = new BinaryTree<>();
                left.parent = this;
            }
            left.insert(value);
        } else {
            if (right == null) {
                right = new BinaryTree<>();
                right.parent = this;
            }
            right.insert(value);

        }
    }

    public boolean find(T value) {
        if (this.value == null) {
            return false;
        }
        if (this.value.compareTo(value) == 0) {
            return true;
        }
        if (value.compareTo(this.value) < 0) {
            return left != null && left.find(value);
        } else {
            return right != null && right.find(value);
        }
    }

    public int height() {
        if (this.value == null) {
            return 0;
        }

        int leftHeight = left == null ? 0 : left.height();
        int rightHeight = right == null ? 0 : right.height();

        return Math.max(leftHeight, rightHeight) + 1;
    }

    Selector<T> inOrderWalk() {
        return new Selector<T>() {
            public BinaryTree<T> current = BinaryTree.this.leftMost();
            public final BinaryTree<T> end = BinaryTree.this.rightMost();

            @Override
            public T current() {
                return current.value;
            }

            @Override
            public boolean hasNext() {
                return end.value != null && current.value.compareTo(end.value) != 0;
            }

            @Override
            public void next() {
                if (current.right != null) {
                    current = current.right.leftMost();
                } else {
                    while (current.parent.left != current)
                        current = current.parent;
                    current = current.parent;
                }
            }
        };
    }

    Selector<T> reverseInOrderWalk() {
        return new Selector<T>() {
            public BinaryTree<T> current = BinaryTree.this.rightMost();
            public final BinaryTree<T> end = BinaryTree.this.leftMost();

            @Override
            public T current() {
                return current.value;
            }

            @Override
            public boolean hasNext() {
                return end.value != null && current.value.compareTo(end.value) != 0;
            }

            @Override
            public void next() {
                if (current.left != null) {
                    current = current.left.rightMost();
                } else {
                    while (current.parent.right != current)
                        current = current.parent;
                    current = current.parent;
                }
            }
        };
    }

    private BinaryTree<T> leftMost() {
        return left != null ? left.leftMost() : this;
    }

    private BinaryTree<T> rightMost() {
        return right != null ? right.rightMost() : this;
    }
}
