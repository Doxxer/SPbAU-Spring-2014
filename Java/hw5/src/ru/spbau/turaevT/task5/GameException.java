package ru.spbau.turaevT.task5;

/**
 * General game exception
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class GameException extends Exception {
    /**
     * Constructs a new exception with the specified detail message.  The
     * cause is not initialized, and may subsequently be initialized by
     * a call to {@link #initCause}.
     *
     * @param message the detail message. The detail message is saved for
     *                later retrieval by the {@link #getMessage()} method.
     */
    public GameException(String message) {
        super(message);
    }
}
