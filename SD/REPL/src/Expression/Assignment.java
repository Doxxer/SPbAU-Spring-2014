package Expression;

public class Assignment implements Exp {
    public final Exp innerExpression;
    public final String name;

    public Assignment(String name, Exp exp) {
        this.innerExpression = exp;
        this.name = name;
    }

    @Override
    public void accept(ExpVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public void traverse(ExpVisitor visitor) {
        visitor.visit(this);
        innerExpression.traverse(visitor);
    }

    @Override
    public Exp evaluate(EvaluateVisitor visitor) throws EvaluateError {
        visitor.visit(this);
        return visitor.result();
    }
}
