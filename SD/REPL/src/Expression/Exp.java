package Expression;

public interface Exp {

    public void accept(ExpVisitor visitor);

    public void traverse(ExpVisitor visitor);

    public Exp evaluate(EvaluateVisitor visitor) throws EvaluateError;
}
