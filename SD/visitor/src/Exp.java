import java.util.Iterator;

public interface Exp {
    void accept(ExpVisitor visitor);

    void traverse(ExpVisitor visitor);

    Iterator<Exp> iterator();

    int evaluate();
}


abstract class BiExp implements Exp {
    public final Exp left;
    public final Exp right;

    protected BiExp(Exp left, Exp right) {
        this.left = left;
        this.right = right;
    }

    public Iterator<Exp> iterator() {
        return new Iterator<Exp>() {
            private boolean iterated = false;
            private Iterator<Exp> leftIterator = left.iterator();
            private Iterator<Exp> rightIterator = right.iterator();

            public boolean hasNext() {
                return leftIterator.hasNext() || !iterated || rightIterator.hasNext();
            }

            @Override
            public Exp next() {
                if (!iterated) {
                    iterated = true;
                    return BiExp.this;
                }
                if (leftIterator.hasNext())
                    return leftIterator.next();
                if (rightIterator.hasNext())
                    return rightIterator.next();
                return null;
            }

            @Override
            public void remove() {

            }
        };
    }
}

class Num implements Exp {
    public final Number number;

    public Num(Number number) {
        this.number = number;
    }

    @Override
    public void accept(ExpVisitor prettyPrinter) {
        prettyPrinter.visit(this);
    }

    @Override
    public void traverse(ExpVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Iterator<Exp> iterator() {
        return new Iterator<Exp>() {
            private boolean iterated = false;

            public boolean hasNext() {
                return !iterated;
            }

            @Override
            public Exp next() {
                if (!iterated) {
                    iterated = true;
                    return Num.this;
                }
                return null;
            }

            @Override
            public void remove() {

            }
        };
    }

    @Override
    public int evaluate() {
        return number.intValue();
    }
}

class Sum extends BiExp {

    public Sum(Exp left, Exp right) {
        super(left, right);
    }

    @Override
    public void accept(ExpVisitor prettyPrinter) {
        prettyPrinter.visit(this);
    }

    @Override
    public void traverse(ExpVisitor visitor) {
        left.traverse(visitor);
        visitor.visit(this);
        right.traverse(visitor);
    }

    @Override
    public int evaluate() {
        return left.evaluate() + right.evaluate();
    }
}

class Mul extends BiExp {
    public Mul(Exp left, Exp right) {
        super(left, right);
    }

    @Override
    public void accept(ExpVisitor prettyPrinter) {
        prettyPrinter.visit(this);
    }

    @Override
    public void traverse(ExpVisitor visitor) {
        left.traverse(visitor);
        visitor.visit(this);
        right.traverse(visitor);
    }

    @Override
    public int evaluate() {
        return left.evaluate() * right.evaluate();
    }
}

class Div extends BiExp {
    public Div(Exp left, Exp right) {
        super(left, right);
    }

    @Override
    public void accept(ExpVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public void traverse(ExpVisitor visitor) {
        left.traverse(visitor);
        visitor.visit(this);
        right.traverse(visitor);
    }

    @Override
    public int evaluate() {
        return left.evaluate() / right.evaluate();
    }
}
