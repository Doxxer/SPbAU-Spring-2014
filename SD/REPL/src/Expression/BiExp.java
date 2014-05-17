package Expression;

public abstract class BiExp implements Exp {
    public final Exp left;
    public final Exp right;
    private final int begin;
    private final int end;

    @Override
    public int getBegin() {
        return begin;
    }

    @Override
    public int getEnd() {
        return end;
    }

    protected BiExp(Exp left, Exp right, int begin, int end) {
        this.left = left;
        this.right = right;
        this.begin = begin;
        this.end = end;
    }
}