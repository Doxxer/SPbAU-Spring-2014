package ru.spbau.turaevT.task2;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Reads one-by-one <code>messages</code> from a specified formatted file
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
public class FileMessageReader implements AutoCloseable {

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
        Message message;
        String linesNumber = fileReader.readLine();
        if (linesNumber == null) {
            return null;
        }

        try {
            int lines = Integer.parseInt(linesNumber);
            if (lines < 0) {
                throw new IllegalMessageFormatException("Negative numbers as line counters are not allowed");
            }
            ArrayList<String> content = new ArrayList<>();
            for (int i = 0; i < lines; i++) {
                String line = fileReader.readLine();
                if (line == null) {
                    throw new IllegalMessageFormatException("Failed to read line");
                }
                content.add(line);
            }
            message = new Message(content);

        } catch (NumberFormatException e) {
            throw new IllegalMessageFormatException("Failed to get number of lines");
        }
        return message;
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
