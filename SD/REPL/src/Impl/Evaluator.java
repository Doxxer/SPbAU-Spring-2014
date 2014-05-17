package Impl;

import Expression.*;

import java.util.Map;

public class Evaluator implements EvaluateVisitor {
    private final boolean justSimplify;
    private Exp result;
    private Map<String, Exp> context;

    public Evaluator(Map<String, Exp> context, boolean justSimplify) {
        this.context = context;
        this.justSimplify = justSimplify;
    }

    @Override
    public Exp result() {
        return result;
    }

    @Override
    public void visit(Num num) {
        result = num;
    }

    @Override
    public void visit(Var var) throws EvaluateError {
        if (context.containsKey(var.name)) {
            result = context.get(var.name).evaluate(this);
        } else if (justSimplify) {
            result = var;
        } else {
            throw new EvaluateError("Error: " + var.name + " is undefined");
        }
    }

    @Override
    public void visit(Sum sum) throws EvaluateError {
        Exp left = sum.left.evaluate(this);
        Exp right = sum.right.evaluate(this);

        if (left instanceof Num && right instanceof Num) {
            Num l = (Num) left;
            Num r = (Num) right;
            result = new Num(l.number.doubleValue() + r.number.doubleValue(), 0, 0);
        } else {
            result = new Sum(left, right, sum.getBegin(), sum.getEnd());
        }
    }

    @Override
    public void visit(Mul mul) throws EvaluateError {
        Exp left = mul.left.evaluate(this);
        Exp right = mul.right.evaluate(this);

        if (left instanceof Num && right instanceof Num) {
            Num l = (Num) left;
            Num r = (Num) right;
            result = new Num(l.number.doubleValue() * r.number.doubleValue(), 0, 0);
        } else {
            result = new Mul(left, right, mul.getBegin(), mul.getEnd());
        }
    }

    @Override
    public void visit(Div div) throws EvaluateError {
        Exp left = div.left.evaluate(this);
        Exp right = div.right.evaluate(this);

        if (left instanceof Num && right instanceof Num) {
            Num l = (Num) left;
            Num r = (Num) right;

            if (r.number.doubleValue() == 0)
                throw new EvaluateError("Division by zero");

            result = new Num(l.number.doubleValue() / r.number.doubleValue(), 0, 0);
        } else {
            result = new Div(left, right, div.getBegin(), div.getEnd());
        }
    }

    @Override
    public void visit(Assignment assignment) throws EvaluateError {
        result = assignment.innerExpression.evaluate(this);
        context.put(assignment.innerVariable.name, result);
    }

    @Override
    public void visit(Sub sub) throws EvaluateError {
        Exp left = sub.left.evaluate(this);
        Exp right = sub.right.evaluate(this);

        if (left instanceof Num && right instanceof Num) {
            Num l = (Num) left;
            Num r = (Num) right;
            result = new Num(l.number.doubleValue() - r.number.doubleValue(), 0, 0);
        } else {
            result = new Sub(left, right, sub.getBegin(), sub.getEnd());
        }
    }
}
