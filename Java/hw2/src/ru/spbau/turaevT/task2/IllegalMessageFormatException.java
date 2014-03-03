package ru.spbau.turaevT.task2;

/**
 * Signals that associated file is illegal formatted.
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class IllegalMessageFormatException extends Exception  {

    /**
     * Constructs a new exception with the specified detail message.
     *
     * @param message the detail message. The detail message is saved for
     *                later retrieval by the {@link #getMessage()} method.
     */
    public IllegalMessageFormatException(String message) {
        super(message);
    }
}
