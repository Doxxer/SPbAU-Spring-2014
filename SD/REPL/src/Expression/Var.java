package Expression;

public class Var implements Exp {
    public final String name;
    private final int begin;
    private final int end;

    public Var(String name, int begin, int end) {
        this.name = name;
        this.begin = begin;
        this.end = end;
    }

    @Override
    public int getBegin() {
        return begin;
    }

    @Override
    public int getEnd() {
        return end;
    }

    @Override
    public void accept(ExpressionVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public void traverse(ExpressionVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Exp evaluate(EvaluateVisitor visitor) throws EvaluateError {
        visitor.visit(this);
        return visitor.result();
    }
}
