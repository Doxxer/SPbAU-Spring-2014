package Expression;

public interface ExpressionVisitor {
    void visit(Num num);

    void visit(Var var);

    void visit(Sum sum);

    void visit(Mul mul);

    void visit(Div div);

    void visit(Assignment assignment);

    void visit(Sub sub);
}
