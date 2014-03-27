package ru.spbau.turaevT.cw1;

/**
 * @author Turaev Timur
 * @version 1.0
 */
public interface Selector<T> {
    T current();

    boolean hasNext();

    void next();
}
