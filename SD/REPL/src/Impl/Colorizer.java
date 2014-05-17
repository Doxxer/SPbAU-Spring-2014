package Impl;

import Expression.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Colorizer implements ExpressionVisitor {
    private final Map<String, Exp> context;
    private final boolean simplifyMode;
    private List<Segment> segmentList = new ArrayList<>();

    public Colorizer(Map<String, Exp> context, boolean simplifyMode) {
        this.context = context;
        this.simplifyMode = simplifyMode;
    }

    @Override
    public void visit(Num num) {
        segmentList.add(new Segment(num.begin, num.end, "operand"));
    }

    public List<Segment> getSegments() {
        return segmentList;
    }

    @Override
    public void visit(Var var) {
        String style = "operand";
        if (!context.containsKey(var.name) && !simplifyMode) {
            style = "error";
        }
        segmentList.add(new Segment(var.getBegin(), var.getEnd(), style));
    }

    @Override
    public void visit(Sum sum) {
        sum.left.accept(this);
        sum.right.accept(this);
    }

    @Override
    public void visit(Mul mul) {
        mul.left.accept(this);
        mul.right.accept(this);
    }

    @Override
    public void visit(Div div) {
        div.left.accept(this);
        div.right.accept(this);
    }

    @Override
    public void visit(Assignment assignment) {
        assignment.innerVariable.accept(this);
        assignment.innerExpression.accept(this);
    }

    @Override
    public void visit(Sub sub) {
        sub.left.accept(this);
        sub.right.accept(this);
    }

    public class Segment {
        private final int begin;
        private final int end;
        private final String name;

        private Segment(int begin, int end, String name) {
            this.begin = begin;
            this.end = end;
            this.name = name;
        }

        public int getBegin() {
            return begin;
        }

        public int getEnd() {
            return end;
        }

        public String getStyle() {
            return name;
        }

        public int length() {
            return end - begin;
        }
    }
}
