package Expression;

public class Assignment implements Exp {
    public final Exp innerExpression;
    public final Var innerVariable;

    private final int begin;
    private final int end;

    public Assignment(Var var, Exp exp, int begin, int end) {
        this.innerExpression = exp;
        this.innerVariable = var;
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
        innerExpression.traverse(visitor);
    }

    @Override
    public Exp evaluate(EvaluateVisitor visitor) throws EvaluateError {
        visitor.visit(this);
        return visitor.result();
    }
}
