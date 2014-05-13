package Expression;

public interface EvaluateVisitor {
    public Exp result();

    void visit(Num num);

    void visit(Var var) throws EvaluateError;

    void visit(Sum sum) throws EvaluateError;

    void visit(Mul mul) throws EvaluateError;

    void visit(Div div) throws EvaluateError;

    void visit(Assignment assignment) throws EvaluateError;
}
