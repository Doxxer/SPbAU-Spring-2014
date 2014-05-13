package Impl;

import Expression.*;

public class Printer implements ExpVisitor {

    public void visit(Num exp) {
        System.out.print(exp.number);
    }

    @Override
    public void visit(Var var) {
        System.out.println(var.name);
    }

    public void visit(Div exp) {
        Div div = exp;
        div.left.accept(this);
        System.out.print(" / ");
        div.right.accept(this);
    }

    @Override
    public void visit(Assignment assignment) {
        System.out.println(assignment.name);
        System.out.println(" = (");
        assignment.innerExpression.accept(this);
        System.out.println(")");
    }

    public void visit(Mul exp) {
        Mul mul = exp;
        mul.left.accept(this);
        System.out.print(" * ");
        mul.right.accept(this);
    }

    public void visit(Sum exp) {
        Sum sum = exp;
        System.out.print("(");
        sum.left.accept(this);
        System.out.print(" + ");
        sum.right.accept(this);
        System.out.print(")");
    }
}
