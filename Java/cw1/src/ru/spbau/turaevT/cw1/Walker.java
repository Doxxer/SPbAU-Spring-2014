package ru.spbau.turaevT.cw1;

import java.text.MessageFormat;

/**
 * Walker is the object that prints all elements in given container via its selector
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Walker {

    /**
     * Prints all elements in given container via its selector
     *
     * @param selector selector
     * @param <T>      type of element that selector returned
     */
    public static <T> void walk(Selector<T> selector) {
        System.out.printf(selector.current() + " ");
        while (selector.hasNext()) {
            selector.next();
            System.out.print(MessageFormat.format("{0} ", selector.current()));
        }
        System.out.println();
    }
}
