package Actions;

import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

public class AllowInputOnlyToLastLineFilter extends DocumentFilter {
    @Override
    public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
        if (Utilities.cursorOnLastLine(offset, fb)) {
            super.insertString(fb, offset, string, attr);
        }
    }

    public void remove(final FilterBypass fb, final int offset, final int length) throws BadLocationException {
        if (offset > Utilities.lastLineIndex(fb.getDocument()) + 1) {
            super.remove(fb, offset, length);
        }
    }

    public void replace(final FilterBypass fb, final int offset, final int length, final String text, final AttributeSet attrs)
            throws BadLocationException {
        if (Utilities.cursorOnLastLine(offset, fb)) {
            super.replace(fb, offset, length, text, attrs);
        }
    }
}
