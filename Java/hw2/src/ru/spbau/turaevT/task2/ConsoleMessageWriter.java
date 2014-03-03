package ru.spbau.turaevT.task2;

import java.util.List;

/**
 * A <tt>ConsoleMessageWriter</tt> is an object for writing <tt>messages</tt> to the console
 *
 * @author Turaev Timur
 * @version 1.0
 * @see ru.spbau.turaevT.task2.Message
 */
public class ConsoleMessageWriter implements MessageWriter {

    private int currentMsgNumber = 1;

    /**
     * Writes the <tt>message</tt> to the console
     *
     * @param message message to be written to the console
     */
    @Override
    public void writeMessage(Message message) {
        List<String> content = message.getLines();
        System.out.println("\"Message " + currentMsgNumber + "\"");
        int currentLineNumber = 1;
        for (String line : content) {
            System.out.printf("\"%d.%d. \"%s\n",
                    currentMsgNumber,
                    currentLineNumber,
                    line);

            currentLineNumber++;
        }
        currentMsgNumber++;
    }

    /**
     * Closes the console writer.
     */
    @Override
    public void close() {
    }
}
