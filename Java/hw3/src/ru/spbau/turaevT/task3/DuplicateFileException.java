package ru.spbau.turaevT.task3;

import java.io.IOException;

/**
 * Checked exception <tt>DuplicateFileException</tt> thrown when occurs
 * an attempt to add to the archive a file that has already been added
 * to the archive
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class DuplicateFileException extends IOException {
    /**
     * Constructs a new exception with the specified detail message.
     *
     * @param message the detail message. The detail message is saved for
     *                later retrieval by the {@link #getMessage()} method.
     */
    public DuplicateFileException(String message) {
        super(message);
    }
}
