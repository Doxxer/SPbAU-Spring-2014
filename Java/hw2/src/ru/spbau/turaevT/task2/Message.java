package ru.spbau.turaevT.task2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The <code>Message</code> class represents text message.
 *
 * <p> Message content is a list of string
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Message {

    private List<String> messageContent = new ArrayList<>();

    /**
     * Constructs the message with content equals to content of given message
     */
    public Message(Message message) {
        this(message.getLines());
    }

    /**
     * Constructs the message containing elements of specified list of strings
     *
     * @param content the initial value of this message content
     */
    public Message(List<String> content) {
        messageContent.addAll(content);
    }

    /**
     * Appends content of the given message to the current message
     *
     * @param message the message, which content is to be appended to the current message
     */
    public void append(Message message) {
        messageContent.addAll(message.messageContent);
    }

    /**
     * Returns an unmodifiable view of the message content.
     *
     * @return an unmodifiable view of the message content.
     */
    public List<String> getLines() {
        return Collections.unmodifiableList(messageContent);
    }
}
