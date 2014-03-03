package ru.spbau.turaevT.task2;

import java.io.IOException;

/**
 * A <tt>MessageWriter</tt> is an object for writing <tt>messages</tt>
 *
 * @author Turaev Timur
 * @version 1.0
 * @see ru.spbau.turaevT.task2.Message
 */
public interface MessageWriter extends AutoCloseable {

    /**
     * Writes the <tt>message</tt>
     *
     * @param message message to be written
     * @throws IOException if an I/O error occurs
     */
    public void writeMessage(Message message) throws IOException;
}
