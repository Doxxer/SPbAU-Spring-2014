package ru.spbau.turaevT.cw1;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("usage: Main inputFileName");
            return;
        }
        String filename = args[0];

        if (filename == null) {
            System.err.println("usage: Main inputFileName");
            return;
        }

        try {
            processFile(new File(filename));
        } catch (FileNotFoundException e) {
            System.err.println(MessageFormat.format("File {0} not found!", filename));
        } catch (IOException e) {
            System.err.println(MessageFormat.format("IOException occurred: {0}", e.getMessage()));
        }
    }

    private static void processFile(File file) throws IOException {
        BinaryTree<Integer> tree = new BinaryTree<>();
        try (Scanner s = new Scanner(file)) {
            int value = 0;
            while (s.hasNextInt()) {
                try {
                    value = s.nextInt();
                    tree.insert(value);
                } catch (BinaryTreeInsertionException e) {
                    System.err.println(MessageFormat.format("Value {0} is already in the tree", value));
                }
            }
            Walker.walk(tree.inOrderWalk());
            Walker.walk(tree.reverseInOrderWalk());
        }
    }
}
