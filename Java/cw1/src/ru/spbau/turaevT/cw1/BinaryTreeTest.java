package ru.spbau.turaevT.cw1;

import org.junit.Assert;

public class BinaryTreeTest {

    private BinaryTree<Integer> tree;

    @org.junit.Before
    public void setUp() throws Exception {
        tree = new BinaryTree<>();
        tree.insert(2);
        tree.insert(3);
        tree.insert(1);
        tree.insert(0);
        tree.insert(-10);
        tree.insert(100500);
    }

    @org.junit.Test
    public void testInsert() throws Exception {
        try {
            tree.insert(2);
            tree.insert(3);
            tree.insert(1);
            tree.insert(0);
            tree.insert(-10);
            tree.insert(100500);
        } catch (BinaryTreeInsertionException e) {
            Assert.assertTrue(true);
            return;
        }
        Assert.fail("Should be exception here!");
    }

    @org.junit.Test
    public void testFind() throws Exception {
        Assert.assertTrue(tree.find(2));
        Assert.assertTrue(tree.find(-10));
        Assert.assertTrue(tree.find(0));
        Assert.assertTrue(tree.find(1));
        Assert.assertTrue(tree.find(3));
        Assert.assertTrue(tree.find(100500));
        Assert.assertFalse(tree.find(-100500));
    }

    @org.junit.Test
    public void testHeight() throws Exception {
        Assert.assertTrue(tree.height() == 4);
    }

    @org.junit.Test
    public void testWalk() throws Exception {
        Walker.walk(tree.inOrderWalk());
        Walker.walk(tree.reverseInOrderWalk());
    }
}
