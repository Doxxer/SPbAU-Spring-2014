package ru.spbau.turaevT.task2;

import java.io.IOException;

/**
 * A <tt>CompressingMessageWriter</tt> is an object for writing <tt>messages</tt>, merging each two on them into one
 *
 * A <tt>CompressingMessageWriter</tt> uses external <tt>MessageWriter</tt> for writing messages
 *
 * @author Turaev Timur
 * @version 1.0
 * @see ru.spbau.turaevT.task2.Message
 * @see ru.spbau.turaevT.task2.MessageWriter
 */
public class CompressingMessageWriter implements MessageWriter {

    private Message bufferMessage = null;
    private MessageWriter writer;

    /**
     * Constructs <tt>CompressingMessageWriter<tt>, wrapping up given message writer
     *
     * @param writer message writer, to be wrapped up
     */
    public CompressingMessageWriter(MessageWriter writer) {
        this.writer = writer;
    }

    /**
     * Writes the <tt>message</tt> to specified message writer
     *
     * @param message message to be written
     * @throws java.io.IOException if an I/O error occurs
     */
    @Override
    public void writeMessage(Message message) throws IOException {
        if (bufferMessage == null) {
            bufferMessage = message;
        } else {
            bufferMessage.append(message);
            writer.writeMessage(bufferMessage);
            bufferMessage = null;
        }
    }

    /**
     * Closes associated message writer
     *
     * @throws Exception if this resource cannot be closed
     */
    @Override
    public void close() throws Exception {
        flush();
        writer.close();
    }

    private void flush() throws IOException {
        if (bufferMessage != null) {
            writer.writeMessage(bufferMessage);
        }
        bufferMessage = null;
    }
}
