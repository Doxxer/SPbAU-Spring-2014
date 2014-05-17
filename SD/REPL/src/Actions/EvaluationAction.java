package Actions;

import Expression.Exp;

import java.util.Map;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class EvaluationAction {

    private final String lastResult;
    private final Map<String, Exp> context;

    public EvaluationAction(String userInput, Map<String, Exp> context) {
        this.lastResult = userInput;
        this.context = context;
    }

    public String getLastResult() {
        return lastResult;
    }

    public Map<String, Exp> getContext() {
        return context;
    }
}
