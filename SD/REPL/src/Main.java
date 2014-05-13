import Expression.*;
import Impl.Evaluator;
import Impl.Printer;

import java.util.HashMap;
import java.util.Map;

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
                new Var("x")
        );

        Exp assignment = new Assignment("x",
                new Sum(
                        new Num(40),
                        new Num(60)
                )
        );

        Printer prettyPrinter = new Printer();

        exp.accept(prettyPrinter);
        System.out.println();

        Map<String, Exp> context = new HashMap<>();

        try {
            exp.evaluate(new Evaluator(context)).accept(prettyPrinter);
            System.out.println();
        } catch (EvaluateError evaluateError) {
            evaluateError.printStackTrace();
        }

        try {
            assignment.evaluate(new Evaluator(context)).accept(prettyPrinter);
            System.out.println();
            exp1.evaluate(new Evaluator(context)).accept(prettyPrinter);
            System.out.println();
        } catch (EvaluateError evaluateError) {
            evaluateError.printStackTrace();
        }
    }
}
