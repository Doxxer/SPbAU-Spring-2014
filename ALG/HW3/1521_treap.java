import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.Writer;
import java.util.Random;

class Node {
    private static final Random random = new Random();
    private final int priority;
    private final int data;
    public Node left, right;
    private int size;

    Node(int data) {
        this.data = data;
        size = 1;
        priority = random.nextInt();
    }

    public static int getSize(Node n) {
        return n == null ? 0 : n.size;
    }

    public void updateSize() {
        this.size = getSize(left) + getSize(right) + 1;
    }

    public int getData() {
        return data;
    }

    public int getPriority() {
        return priority;
    }
}

class Pair<A, B> {
    public A first;
    public B second;

    Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }
}

class Treap {
    private static final Pair<Node, Node> emptyNode = new Pair<>(null, null);
    private static Pair<Node, Node> splitResult = new Pair<>(null, null);
    private Node root = null;

    private static Node merge(Node left, Node right) {
        if (right == null) return left;
        if (left == null) return right;

        if (left.getPriority() < right.getPriority()) {
            right.left = merge(left, right.left);
            right.updateSize();
            return right;
        } else {
            left.right = merge(left.right, right);
            left.updateSize();
            return left;
        }
    }

    private static Pair<Node, Node> split(Node node, int splitPosition) {
        if (node == null) {
            return emptyNode;
        }

        int currentPosition = Node.getSize(node.left) + 1;
        if (currentPosition <= splitPosition) {
            Pair<Node, Node> splittedNodes = split(node.right, splitPosition - currentPosition);
            node.right = null;
            splitResult.first = merge(node, splittedNodes.first);
            splitResult.second = splittedNodes.second;
        } else {
            Pair<Node, Node> splittedNodes = split(node.left, splitPosition);
            node.left = null;
            splitResult.first = splittedNodes.first;
            splitResult.second = merge(splittedNodes.second, node);
        }
        if (splitResult.first != null) {
            splitResult.first.updateSize();
        }
        if (splitResult.second != null) {
            splitResult.second.updateSize();
        }
        return splitResult;
    }

    public void add(int position, int data) {
        Pair<Node, Node> splittedNodes = split(position);
        root = merge(merge(splittedNodes.first, new Node(data)), splittedNodes.second);
    }

    public Pair<Integer, Integer> remove(int position) {
        Pair<Node, Node> leftSplit = split(position - 1);
        int sizeofLeftBranch = Node.getSize(leftSplit.first);

        Pair<Node, Node> rightSplit = split(leftSplit.second, 1);
        int result = rightSplit.first.getData();

        root = merge(leftSplit.first, rightSplit.second);
        return new Pair<>(result, sizeofLeftBranch);
    }

    private Pair<Node, Node> split(int position) {
        Pair<Node, Node> splitted = split(root, position);
        return new Pair<>(splitted.first, splitted.second);
    }
}

public class Main {
    private static StreamTokenizer in;
    private static PrintWriter out;

    public static void main(String[] args) throws IOException {
        init();

        int soldiersNumber = nextInt(), dropoutNumber = nextInt();
        Treap treap = new Treap();
        for (int i = 0; i < soldiersNumber; i++) {
            treap.add(i, i + 1);
        }

        int nextDropOut = dropoutNumber;
        for (int i = 0; i < soldiersNumber; i++) {
            Pair<Integer, Integer> pair = treap.remove(nextDropOut);
            out.print(pair.first);
            out.print(' ');

            if (i == soldiersNumber - 1) break;
            nextDropOut = (dropoutNumber + pair.second) % (soldiersNumber - i - 1);
            if (nextDropOut == 0) {
                nextDropOut = soldiersNumber - i - 1;
            }
        }
        out.flush();
    }

    private static void init() throws IOException {
        boolean online_judge = System.getProperty("ONLINE_JUDGE") != null;
        Reader reader = online_judge ? new InputStreamReader(System.in) : new FileReader("input.txt");
        Writer writer = online_judge ? new OutputStreamWriter(System.out) : new FileWriter("output.txt");
        in = new StreamTokenizer(new BufferedReader(reader));
        out = new PrintWriter(writer);
    }

    private static int nextInt() throws IOException {
        in.nextToken();
        return (int) in.nval;
    }
}