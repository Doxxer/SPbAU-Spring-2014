package ru.spbau.turaevT.task2;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.MessageFormat;

/**
 * Entry point.
 *
 * First command-line argument is the name of input file. <p>
 * Second (if exists) is the name of output file. <p>
 *
 * Compresses messages in input file and output it to the file or console
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Main {
    /**
     * Compresses messages in input file and output it to the file or console
     *
     * @param args command line arguments
     */
    public static void main(String[] args) {
        String inputFileName;
        String outputFileName = null;

        if (args.length > 0) {
            inputFileName = args[0];
            if (args.length > 1) {
                outputFileName = args[1];
            }
        } else {
            System.err.println("usage: Main inputFileName [outputFileName]");
            return;
        }

        Message message;
        try (FileMessageReader reader = new FileMessageReader(inputFileName);
             MessageWriter outputStream = (outputFileName == null)
                     ? new ConsoleMessageWriter()
                     : new FileMessageWriter(outputFileName);
             CompressingMessageWriter writer = new CompressingMessageWriter(outputStream)) {
            while ((message = reader.readMessage()) != null) {
                writer.writeMessage(message);
            }
            writer.flush();
        } catch (FileNotFoundException e) {
            System.err.println(MessageFormat.format("Input file ''{0}'' not found.", inputFileName));
        } catch (IllegalMessageFormatException e) {
            System.err.println(MessageFormat.format("Input file format has illegal format: {0}.", e.getMessage()));
        } catch (IOException e) {
            System.err.println(MessageFormat.format("An I/O error occurred: {0}", e.getMessage()));
        }
    }
}