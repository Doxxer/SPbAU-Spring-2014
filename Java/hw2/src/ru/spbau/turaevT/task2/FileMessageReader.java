package ru.spbau.turaevT.task2;

import java.io.*;
import java.util.ArrayList;

/**
 * Reads one-by-one <code>messages</code> from a specified formatted file
 * <p/>
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
public class FileMessageReader implements Closeable {

    private BufferedReader fileReader;

    /**
     * Constructs message reader, given the name of the file to be read from.
     *
     * @param filename name of the file to be read from.
     * @throws FileNotFoundException if named file doesn't exist
     */
    public FileMessageReader(String filename) throws FileNotFoundException {
        fileReader = new BufferedReader(new FileReader(filename));
    }

    /**
     * Reads a message from a file.
     *
     * @return <tt>message</tt> read from a file
     * @throws IOException                   if an I/O error occurs
     * @throws IllegalMessageFormatException if the specified file is illegal-formatted
     */
    public Message readMessage() throws IOException, IllegalMessageFormatException {
        String linesNumber = fileReader.readLine();
        if (linesNumber == null) {
            return null;
        }

        int lines;
        try {
            lines = Integer.parseInt(linesNumber);
        } catch (NumberFormatException e) {
            throw new IllegalMessageFormatException("Failed to get number of lines");
        }
        if (lines < 0) {
            throw new IllegalMessageFormatException("Negative numbers as line counters are not allowed");
        }
        ArrayList<String> content = new ArrayList<>();
        for (int i = 0; i < lines; i++) {
            String line = fileReader.readLine();
            if (line == null) {
                throw new IllegalMessageFormatException("Unexpected end of file");
            }
            content.add(line);
        }
        return new Message(content);
    }

    /**
     * Closes associated file.
     *
     * @throws IOException if an I/O error occurs
     */
    @Override
    public void close() throws IOException {
        fileReader.close();
    }
}
