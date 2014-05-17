import Actions.AllowInputOnlyToLastLineFilter;
import Actions.UserInputProcessor;
import Actions.Utilities;

import javax.swing.*;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class REPLConsole {
    public static final String SIMPLIFY = "Simplify";
    public static final String EVALUATE = "Evaluate";
    public static final String GREETING = System.lineSeparator() + ">";

    private UserInputProcessor userInputProcessor;
    private UndoableEditListener undoableEditListener;

    public static void main(String[] args) {
        REPLConsole replConsole = new REPLConsole();
        replConsole.init();
    }

    private void init() {

        undoableEditListener = e -> userInputProcessor.saveUndoableEdit(e.getEdit());

        JFrame frame = new JFrame();
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosed(WindowEvent e) {
                System.exit(0);
            }
        });
        frame.setLayout(new BorderLayout());

        final JComboBox<String> optionPane = new JComboBox<>();
        optionPane.addItem(SIMPLIFY);
        optionPane.addItem(EVALUATE);
        optionPane.addActionListener(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                userInputProcessor.setMode(SIMPLIFY.equals(optionPane.getSelectedItem()));
            }
        });


        frame.add(optionPane, "North");

        JEditorPane textArea = new JEditorPane();
        AbstractDocument document = (AbstractDocument) textArea.getDocument();
        document.setDocumentFilter(new AllowInputOnlyToLastLineFilter());
        textArea.setText("Welcome to REPL Console! " + System.lineSeparator() + ">");
        textArea.setEditable(true);
        frame.add(textArea, "Center");

        userInputProcessor = new UserInputProcessor();
        optionPane.setSelectedIndex(0);

        // ----------- add listeners ----------

        document.addUndoableEditListener(undoableEditListener);

        textArea.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke("ENTER"), new EvaluateAction());
        textArea.getKeymap().addActionForKeyStroke(KeyStroke.getKeyStroke("control shift Z"), new UndoEvaluationAction());

        textArea.getKeymap().addActionForKeyStroke(
                KeyStroke.getKeyStroke("control Z"),
                new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        userInputProcessor.undoEdit();
                    }
                }
        );

        frame.setVisible(true);
        frame.setSize(500, 300);
    }

    private abstract class PrintToConsoleAction extends AbstractAction {
        protected Document document;

        protected abstract String getStringToPrint() throws BadLocationException;

        @Override
        public void actionPerformed(ActionEvent e) {
            JEditorPane source = (JEditorPane) e.getSource();
            document = source.getDocument();
            try {
                document.removeUndoableEditListener(undoableEditListener);

                String result = getStringToPrint();
                if (result != null) {
                    document.insertString(Utilities.endOffset(document), System.lineSeparator() + result, null);
                    document.insertString(Utilities.endOffset(document), GREETING, null);
                    source.setCaretPosition(Utilities.endOffset(document));
                }
            } catch (BadLocationException e1) {
                e1.printStackTrace();
            } finally {
                document.addUndoableEditListener(undoableEditListener);
            }
        }
    }

    public class UndoEvaluationAction extends PrintToConsoleAction {
        @Override
        protected String getStringToPrint() throws BadLocationException {
            return userInputProcessor.undoEvaluation();
        }
    }

    public class EvaluateAction extends PrintToConsoleAction {
        @Override
        protected String getStringToPrint() throws BadLocationException {
            String userInput = document.getText(0, document.getLength()).substring(Utilities.lastLineIndex(document) + GREETING.length());
            if (userInput.isEmpty())
                return null;
            return userInputProcessor.parseAndEvaluate(userInput);
        }
    }
}

