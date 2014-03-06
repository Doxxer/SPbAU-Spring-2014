import java.io.*;

class Node {
    int data;
    Node left, right, parent;
    private int size = 1;

    public Node(int data, Node left, Node right) {
        this.data = data;
        this.left = left;
        this.right = right;
    }

    public static void setParent(Node child, Node parent) {
        if (child != null) {
            child.parent = parent;
        }
    }

    public static int getSize(Node n) {
        return n == null ? 0 : n.size;
    }

    public void keepParent() {
        setParent(this.left, this);
        setParent(this.right, this);
        update();
    }

    public void update() {
        this.size = getSize(left) + getSize(right) + 1;
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

class SplayTree {
    protected Node root = null;

    public static Node merge(Node left, Node right) {
        if (right == null) return left;
        if (left == null) return right;

        right = find(right, 1);
        right.left = left;
        left.parent = right;
        right.update();
        return right;
    }

    public static Node find(Node node, int pos) {
        if (node == null) return null;

        int curIndex = Node.getSize(node.left) + 1;

        if (curIndex == pos) return splay(node);

        if (pos < curIndex && node.left != null)
            return find(node.left, pos);
        if (pos > curIndex && node.right != null)
            return find(node.right, pos - curIndex);
        return splay(node);
    }

    private static Node splay(Node v) {
        if (v.parent == null)
            return v;
        Node parent = v.parent;
        Node grandparent = parent.parent;

        if (grandparent == null) {
            rotate(parent, v);
            return v;
        } else {
            boolean zigzig = (grandparent.left == parent) == (parent.left == v);
            if (zigzig) {
                rotate(grandparent, parent);
                rotate(parent, v);
            } else {
                rotate(parent, v);
                rotate(grandparent, v);
            }
            return splay(v);
        }
    }

    private static void rotate(Node parent, Node child) {
        Node grandparent = parent.parent;
        if (grandparent != null) {
            if (grandparent.left == parent) {
                grandparent.left = child;
            } else {
                grandparent.right = child;
            }
        }
        if (parent.left == child) {
            parent.left = child.right;
            child.right = parent;
        } else {
            parent.right = child.left;
            child.left = parent;
        }
        parent.keepParent();
        child.keepParent();
        child.parent = grandparent;
        if (grandparent != null) {
            grandparent.update();
        }
    }

    public void insert(int pos, int data) {
        Pair<Node, Node> split = split(root, pos);
        root = new Node(data, split.first, split.second);
        root.keepParent();
    }

    private Pair<Node, Node> split(Node node, int key) {
        if (node == null) {
            return new Pair<Node, Node>(null, null);
        }

        node = find(node, key);
        int curIndex = Node.getSize(node.left) + 1;

        if (curIndex == key) {
            Node.setParent(node.left, null);
            Node.setParent(node.right, null);
            return new Pair<Node, Node>(node.left, node.right);
        }
        if (curIndex < key) {
            Node right = node.right;
            node.right = null;
            Node.setParent(right, null);
            return new Pair<Node, Node>(node, right);
        } else {
            Node left = node.left;
            node.left = null;
            Node.setParent(left, null);
            return new Pair<Node, Node>(left, node);
        }
    }

    public Pair<Integer, Integer> remove(int key) {
        root = find(root, key);

        int leftSize = Node.getSize(root.left);
        int data = root.data;

        Node.setParent(root.left, null);
        Node.setParent(root.right, null);
        root = merge(root.left, root.right);

        return new Pair<Integer, Integer>(data, leftSize);
    }
}


public class Main {
    private static StreamTokenizer in;
    private static PrintWriter out;

    public static void main(String[] args) throws Exception {
        init();
        SplayTree t = new SplayTree();

        int n = nextInt(), k = nextInt();
        for (int i = 1; i <= n; i++) {
            t.insert(i, i);
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
            out.flush();
        }
        out.flush();
    }

    private static void init() throws IOException {
        boolean oj = System.getProperty("ONLINE_JUDGE") != null;
        Reader reader = oj ? new InputStreamReader(System.in) : new FileReader("input.txt");
        //Writer writer = oj ? new OutputStreamWriter(System.out) : new FileWriter("output.txt");
        Writer writer = new OutputStreamWriter(System.out);
        in = new StreamTokenizer(new BufferedReader(reader));
        out = new PrintWriter(writer);
    }

    private static int nextInt() throws IOException {
        in.nextToken();
        return (int) in.nval;
    }
}