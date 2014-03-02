import java.io.*;
import java.util.Random;

class Node {
   private static Random random = new Random();

   int prior;
   long data = 0;
   Node left, right;
   private long sum = 0;
   private int size = 1;

   Node(long data) {
       this.data = data;
       this.sum = this.data;
       this.prior = random.nextInt();
   }

   private static void print(StringBuffer sb, Node node) {
       if (node == null) {
           return;
       }
       print(sb, node.left);
       sb.append(String.format("[%d; sum=%d, size = %d]", node.data, getSum(node), getSize(node)));
       sb.append(",");
       print(sb, node.right);
   }

   public static int getSize(Node n) {
       return n == null ? 0 : n.size;
   }

   public static long getSum(Node n) {
       return n == null ? 0 : n.sum;
   }

   public void update() {
       this.size = getSize(left) + getSize(right) + 1;
       this.sum = getSum(left) + getSum(right) + this.data;
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

   public void add(int pos, long data) {
       Pair<Node, Node> pair = split(pos);
       root = merge(merge(pair.first, new Node(data)), pair.second);
   }

   public String toString() {
       return root.toString();
   }

   public Pair<Node, Node> split(int key) {
       return split(root, key);
   }

   public long sumOn(int a, int b) {
       a--;
       b--;
       Pair<Node, Node> split1 = split(a);
       Pair<Node, Node> split2 = split(split1.second, b + 1 - a);
       long sum = Node.getSum(split2.first);
       root = merge(merge(split1.first, split2.first), split2.second);
       return sum;
   }

   public Node[] splitToRange(int a, int b) {
       a--;
       b--;
       Pair<Node, Node> split1 = split(a);
       Pair<Node, Node> split2 = split(split1.second, b + 1 - a);
       return new Node[]{split1.first, split2.first, split2.second};
   }
}

public class Main {
   private static StreamTokenizer in;
   private static PrintWriter out;

   public static void main(String[] args) throws IOException {
       init();

       int test = 1;
       int n = nextInt(), m = nextInt();
       do {
           if (test != 1) out.println();
           out.printf("Swapper %d:\n", test);
           Treap a = new Treap(), b = new Treap();
           int aPos = 0, bPos = 0;
           for (int i = 1; i <= n; i++) {
               if (i % 2 == 1) {
                   a.add(aPos++, nextInt());
               } else {
                   b.add(bPos++, nextInt());
               }
           }

           for (int i = 0; i < m; i++) {
               int type = nextInt(), l = nextInt(), r = nextInt();

               int al = (l + 2 - l % 2) / 2;
               int bl = (l + l % 2) / 2;
               int ar = (r + r % 2) / 2;
               int br = (r - r % 2) / 2;

               if (type == 2) {
                   long res = 0;
                   if (al <= ar) {
                       res += a.sumOn(al, ar);
                   }
                   if (bl <= br) {
                       res += b.sumOn(bl, br);
                   }
                   out.println(res);
                   out.flush();
               } else {
                   Node[] splitA = a.splitToRange(al, ar);
                   Node[] splitB = b.splitToRange(bl, br);

                   a.root = Treap.merge(Treap.merge(splitA[0], splitB[1]), splitA[2]);
                   b.root = Treap.merge(Treap.merge(splitB[0], splitA[1]), splitB[2]);
               }
           }
           n = nextInt();
           m = nextInt();
           test++;
       } while (n != 0 && m != 0);
       out.flush();
   }

   private static void init() throws FileNotFoundException {
       Reader reader = new InputStreamReader(System.in);
       in = new StreamTokenizer(new BufferedReader(reader));
       out = new PrintWriter(new OutputStreamWriter(System.out));
   }

   private static int nextInt() throws IOException {
       in.nextToken();
       return (int) in.nval;
   }
}