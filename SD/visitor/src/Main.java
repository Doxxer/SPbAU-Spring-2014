import java.util.Iterator;

public class Main {
    public static void main(String[] args) {
        Exp exp = new Sum(
                new Mul(
                        new Sum(new Num(10.0), new Num(21.0)),
                        new Sum(new Num(22.0), new Num(14.0))
                ),
                new Div(
                        new Sum(new Num(15), new Num(88)),
                        new Mul(new Num(11), new Num(18))
                )
        );

        Exp exp1 = new Mul(
                new Sum(
                        new Num(3),
                        new Num(4)
                ),
                new Num(5)
        );

        exp.traverse(new ExpVisitorAdapter() {
            @Override
            public void visit(Div div) {
                div.right.accept(new ExpVisitorAdapter() {
                    @Override
                    public void visit(Num num) {
                        if (num.number.intValue() == 0) {
                            throw new RuntimeException("StaticCheck failed: Divide by zero");
                        }
                    }
                });
            }
        });

        PrettyPrinter printer = new PrettyPrinter();
        PolishPrinter polishPrinter = new PolishPrinter();

        exp.accept(printer);
        System.out.println();
        exp1.accept(polishPrinter);
        System.out.println();

        Iterator<Exp> i = exp.iterator();
        while (i.hasNext()) {
            Exp e = i.next();
            e.accept(printer);
            System.out.println();
        }
    }
}