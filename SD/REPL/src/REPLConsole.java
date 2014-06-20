import Actions.AllowInputOnlyToLastLineFilter;
import Actions.UserInputProcessor;
import Actions.Utilities;
import Expression.Exp;
import Impl.Colorizer;
import Parsing.Parser;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.ParseException;

public class REPLConsole {
    public static final String SIMPLIFY = "Simplify";
    public static final String EVALUATE = "Evaluate";
    public static final String GREETING = System.lineSeparator() + ">";

    private UserInputProcessor userInputProcessor;
    private UndoableEditListener undoableEditListener;

    public static void main(String[] args) {
        Iterable
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

        StyledDocument styledDocument = new DefaultStyledDocument();
        Style base = StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);

        Style def = styledDocument.addStyle("default", base);
        StyleConstants.setForeground(def, Color.BLACK);

        Style operand = styledDocument.addStyle("operand", base);
        StyleConstants.setForeground(operand, Color.BLUE);
        StyleConstants.setBold(operand, true);

        Style error = styledDocument.addStyle("error", base);
        StyleConstants.setForeground(error, Color.RED);
        StyleConstants.setUnderline(error, true);

        JTextPane textArea = new JTextPane(styledDocument);

        textArea.setText("Welcome to REPL Console! " + System.lineSeparator() + ">");
        textArea.setEditable(true);
        frame.add(textArea, "Center");

        userInputProcessor = new UserInputProcessor();
        optionPane.setSelectedIndex(0);

        AbstractDocument document = (AbstractDocument) textArea.getDocument();
        // ----------- add listeners ----------
        document.setDocumentFilter(new AllowInputOnlyToLastLineFilter());
        document.addUndoableEditListener(undoableEditListener);
        document.addDocumentListener(new Coloring());

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
        frame.setSize(500, 800);
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

    private class Coloring implements DocumentListener {
        public void insertUpdate(DocumentEvent e) {
            process(e);
        }

        public void removeUpdate(DocumentEvent e) {
            process(e);
        }

        public void changedUpdate(DocumentEvent e) {
        }

        public void process(DocumentEvent e) {
            final StyledDocument document = (StyledDocument) e.getDocument();
            try {
                String text = document.getText(0, document.getLength());
                boolean isUserInput = '>' == text.charAt(Utilities.lastLineIndex(document) + 1);
                int begin = Utilities.lastLineIndex(document) + GREETING.length();
                final String userInput = text.substring(begin);
                if (!isUserInput || userInput.isEmpty())
                    return;

                try {
                    Exp exp = new Parser(userInput).process();
                    Colorizer c = new Colorizer(userInputProcessor.getContext(), userInputProcessor.isSimplifyMode());
                    exp.accept(c);
                    SwingUtilities.invokeLater(() -> {
                        document.removeUndoableEditListener(undoableEditListener);
                        document.setCharacterAttributes(begin, userInput.length(), document.getStyle("default"), true);
                        for (Colorizer.Segment segment : c.getSegments()) {
                            document.setCharacterAttributes(begin + segment.getBegin(),
                                    segment.length(), document.getStyle(segment.getStyle()), true);
                        }
                        document.addUndoableEditListener(undoableEditListener);
                    });

                } catch (ParseException e1) {
                    SwingUtilities.invokeLater(() -> {
                        document.removeUndoableEditListener(undoableEditListener);
                        Style s = document.getStyle("error");
                        document.setCharacterAttributes(begin, userInput.length(), s, true);
                        document.addUndoableEditListener(undoableEditListener);
                    });
                }
            } catch (BadLocationException ignored) {
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

