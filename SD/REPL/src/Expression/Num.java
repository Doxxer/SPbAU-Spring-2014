package Expression;

public class Num implements Exp {
    public final Number number;
    public final int begin;
    public final int end;

    public Num(Number number, int begin, int end) {
        this.number = number;
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
    public void accept(ExpressionVisitor prettyPrinter) {
        prettyPrinter.visit(this);
    }

    @Override
    public void traverse(ExpressionVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Exp evaluate(EvaluateVisitor visitor) {
        visitor.visit(this);
        return visitor.result();
    }
}

