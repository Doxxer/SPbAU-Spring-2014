import java.io.*;
import java.util.Random;

class Node {
    private static Random random = new Random();

    int prior;
    int data = 0;
    Node left, right;
    private int size = 1;

    Node(int data) {
        this.data = data;
        this.prior = random.nextInt();
    }

    private static void print(StringBuffer sb, Node node) {
        if (node == null) {
            return;
        }
        print(sb, node.left);
        sb.append(String.format("[%d; size = %d]", node.data, getSize(node)));
        sb.append(",");
        print(sb, node.right);
    }

    public static int getSize(Node n) {
        return n == null ? 0 : n.size;
    }

    public void update() {
        this.size = getSize(left) + getSize(right) + 1;
    }

    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append('{');
        print(sb, this);
        if (sb.length() > 1) {
            sb.deleteCharAt(sb.length() - 1);
        }
        sb.append('}');
        return sb.toString();
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
    Node root = null;

    Treap() {
    }

    public static Node merge(Node left, Node right) {
        if (right == null) return left;
        if (left == null) return right;

        if (left.prior < right.prior) {
            right.left = merge(left, right.left);
            right.update();
            return right;
        } else {
            left.right = merge(left.right, right);
            left.update();
            return left;
        }
    }

    Pair<Node, Node> split(Node node, Integer key) {
        if (node == null) {
            return new Pair<Node, Node>(null, null);
        }

        int curIndex = Node.getSize(node.left) + 1;
        Pair<Node, Node> res;
        if (curIndex <= key) {
            Pair<Node, Node> pair = split(node.right, key - curIndex);
            node.right = null;
            res = new Pair<Node, Node>(merge(node, pair.first), pair.second);
        } else {
            Pair<Node, Node> pair = split(node.left, key);
            node.left = null;
            res = new Pair<Node, Node>(pair.first, merge(pair.second, node));

        }
        if (res.first != null) {
            res.first.update();
        }
        if (res.second != null) {
            res.second.update();
        }
        return res;
    }

    public void add(int pos, int data) {
        Pair<Node, Node> pair = split(pos);
        root = merge(merge(pair.first, new Node(data)), pair.second);
    }

    public String toString() {
        return root.toString();
    }

    public Pair<Node, Node> split(int key) {
        return split(root, key);
    }

    public Pair<Integer, Integer> remove(int x) {
        Pair<Node, Node> split = split(x - 1);
        int a = Node.getSize(split.first);
        Pair<Node, Node> split1 = split(split.second, 1);
        int res = split1.first.data;
        root = merge(split.first, split1.second);
        return new Pair<Integer, Integer>(res, a);
    }
}

public class Main {
    private static StreamTokenizer in;
    private static PrintWriter out;

    public static void main(String[] args) throws IOException {
        init();

        int n = nextInt(), k = nextInt();
        Treap t = new Treap();
        for (int i = 0; i < n; i++) {
            t.add(i, i + 1);
        }

        int num = k;
        for (int i = 0; i < n; i++) {
            Pair<Integer, Integer> pair = t.remove(num);
            out.print(pair.first);
            out.print(' ');
            int a = pair.second;

            if (i == n - 1) break;
            num = (k + a) % (n - i - 1);
            if (num == 0) {
                num = n - i - 1;
            }
        }

        out.flush();
    }

    private static void init() throws IOException {
        boolean oj = System.getProperty("ONLINE_JUDGE") != null;
        Reader reader = oj ? new InputStreamReader(System.in) : new FileReader("input.txt");
        Writer writer = oj ? new OutputStreamWriter(System.out) : new FileWriter("output.txt");
        in = new StreamTokenizer(new BufferedReader(reader));
        out = new PrintWriter(writer);
    }

    private static int nextInt() throws IOException {
        in.nextToken();
        return (int) in.nval;
    }
}