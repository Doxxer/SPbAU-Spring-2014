package Actions;

import Expression.Assignment;
import Expression.EvaluateError;
import Expression.Exp;
import Impl.Evaluator;
import Impl.Printer;
import Parsing.Parser;

import javax.swing.undo.UndoableEdit;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class UserInputProcessor {
    private final Stack<UndoableEdit> edits = new Stack<>();
    private final Stack<EvaluationAction> evaluations = new Stack<>();

    private boolean simplifyMode;
    private Map<String, Exp> context;
    private String lastResult = null;

    public UserInputProcessor() {
        context = new HashMap<>();
    }

    public Map<String, Exp> getContext() {
        return context;
    }

    public boolean isSimplifyMode() {
        return simplifyMode;
    }

    public void setMode(boolean simplifyMode) {
        this.simplifyMode = simplifyMode;
    }

    public String parseAndEvaluate(String userInput) {
        edits.clear();
        evaluations.push(new EvaluationAction(lastResult, new HashMap<>(context)));
        String result;
        try {
            Parser p = new Parser(userInput);
            StringBuilder b = new StringBuilder();
            Exp expression = p.process();
            if (!simplifyMode && expression instanceof Assignment)
                throw new ParseException("Assignments disallowed in non-simplify mode", 0);
            expression.evaluate(new Evaluator(context, this.simplifyMode)).accept(new Printer(b));
            result = b.toString();
        } catch (ParseException | EvaluateError error) {
            result = error.getMessage();
        }
        lastResult = result;
        return result;
    }

    public void saveUndoableEdit(UndoableEdit edit) {
        if (edit.canUndo())
            edits.push(edit);
    }

    public void undoEdit() {
        if (!edits.empty()) {
            edits.pop().undo();
        }
    }

    public String undoEvaluation() {
        if (!evaluations.empty()) {
            EvaluationAction e = evaluations.pop();
            context = e.getContext();
            lastResult = e.getLastResult();
            return lastResult;
        }
        return null;
    }
}
