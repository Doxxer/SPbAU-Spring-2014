package ru.spbau.turaevT.task2;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

/**
 * A <tt>FileMessageWriter</tt> is an object for writing <tt>messages</tt> in specific format to the file.
 *
 * <p> File format:
 * <pre>
 *     Count of first message strings
 *     First message strings
 *     Count of second message strings
 *     Second message strings
 *     ...
 * </pre>
 *
 * @author Turaev Timur
 * @version 1.0
 * @see ru.spbau.turaevT.task2.Message
 */
public class FileMessageWriter implements MessageWriter {

    BufferedWriter fileWriter;

    /**
     * Constructs writer, given the name of the file to write to.
     *
     * @param filename name of the file to write to.
     * @throws IOException if an I/O error occurs.
     */
    public FileMessageWriter(String filename) throws IOException {
        fileWriter = new BufferedWriter(new FileWriter(filename));
    }

    /**
     * Writes the <tt>message</tt> to the file
     *
     * @param message message to be written.
     * @throws IOException if an I/O error occurs
     */
    @Override
    public void writeMessage(Message message) throws IOException {
        List<String> content = message.getLines();
        Integer linesNum = content.size();
        fileWriter.write(linesNum.toString());
        fileWriter.newLine();
        for (String line : content) {
            fileWriter.write(line);
            fileWriter.newLine();
        }
        fileWriter.flush();
    }

    /**
     * Closes associated file.
     *
     * @throws IOException if this resource cannot be closed
     */
    @Override
    public void close() throws IOException {
        fileWriter.close();
    }
}
