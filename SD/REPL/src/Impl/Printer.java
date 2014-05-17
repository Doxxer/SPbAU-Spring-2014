package Impl;

import Expression.*;

public class Printer implements ExpressionVisitor {

    private final StringBuilder stringBuilder;

    public Printer(StringBuilder stringBuilder) {
        this.stringBuilder = stringBuilder;
    }

    public void visit(Num exp) {
        stringBuilder.append(exp.number);
    }

    @Override
    public void visit(Var var) {
        stringBuilder.append(var.name);
    }

    public void visit(Div exp) {
        Div div = exp;
        div.left.accept(this);
        stringBuilder.append(" / ");
        div.right.accept(this);
    }

    @Override
    public void visit(Assignment assignment) {
        System.out.println(assignment.name);
        stringBuilder.append(" = (");
        assignment.innerExpression.accept(this);
        stringBuilder.append(")");
    }

    @Override
    public void visit(Sub sub) {
        stringBuilder.append("(");
        sub.left.accept(this);
        stringBuilder.append(" - ");
        sub.right.accept(this);
        stringBuilder.append(")");
    }

    public void visit(Mul exp) {
        Mul mul = exp;
        mul.left.accept(this);
        stringBuilder.append(" * ");
        mul.right.accept(this);
    }

    public void visit(Sum exp) {
        Sum sum = exp;
        stringBuilder.append("(");
        sum.left.accept(this);
        stringBuilder.append(" + ");
        sum.right.accept(this);
        stringBuilder.append(")");
    }
}
