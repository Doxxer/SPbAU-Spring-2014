import java.text.MessageFormat;

public class PolishPrinter implements ExpVisitor {

    public void visit(Num exp) {
        System.out.print(MessageFormat.format("{0} ", exp.number));
    }

    public void visit(Div exp) {
        Div div = exp;
        System.out.print("/ ");
        div.left.accept(this);
        div.right.accept(this);
    }

    public void visit(Mul exp) {
        Mul mul = exp;
        System.out.print("* ");
        mul.left.accept(this);
        mul.right.accept(this);
    }

    public void visit(Sum exp) {
        Sum sum = exp;
        System.out.print("+ ");
        sum.left.accept(this);
        sum.right.accept(this);
    }
}
