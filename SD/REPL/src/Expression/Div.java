package Expression;

public class Div extends BiExp {
    public Div(Exp left, Exp right) {
        super(left, right);
    }

    @Override
    public void accept(ExpVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public void traverse(ExpVisitor visitor) {
        left.traverse(visitor);
        visitor.visit(this);
        right.traverse(visitor);
    }

    @Override
    public Exp evaluate(EvaluateVisitor visitor) throws EvaluateError {
        visitor.visit(this);
        return visitor.result();
    }
}
