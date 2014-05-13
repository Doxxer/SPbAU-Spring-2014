package Impl;

import Expression.*;

import java.util.Map;

public class Evaluator implements EvaluateVisitor {
    private Exp result;
    private Map<String, Exp> context;

    public Evaluator(Map<String, Exp> context) {
        this.context = context;
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
            result = new Num(l.number.doubleValue() + r.number.doubleValue());
        } else {
            result = new Sum(left, right);
        }
    }

    @Override
    public void visit(Mul mul) throws EvaluateError {
        Exp left = mul.left.evaluate(this);
        Exp right = mul.right.evaluate(this);

        if (left instanceof Num && right instanceof Num) {
            Num l = (Num) left;
            Num r = (Num) right;
            result = new Num(l.number.doubleValue() * r.number.doubleValue());
        } else {
            result = new Mul(left, right);
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

            result = new Num(l.number.doubleValue() / r.number.doubleValue());
        } else {
            result = new Div(left, right);
        }
    }

    @Override
    public void visit(Assignment assignment) throws EvaluateError {
        result = assignment.innerExpression.evaluate(this);
        context.put(assignment.name, result);
    }
}
