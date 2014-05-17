package Expression;

public interface Exp {

    public int getBegin();

    public int getEnd();

    public void accept(ExpressionVisitor visitor);

    public void traverse(ExpressionVisitor visitor);

    public Exp evaluate(EvaluateVisitor visitor) throws EvaluateError;
}
