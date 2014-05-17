package Expression;

public class Div extends BiExp {
    public Div(Exp left, Exp right, int begin, int end) {
        super(left, right, begin, end);
    }

    @Override
    public void accept(ExpressionVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public void traverse(ExpressionVisitor visitor) {
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

