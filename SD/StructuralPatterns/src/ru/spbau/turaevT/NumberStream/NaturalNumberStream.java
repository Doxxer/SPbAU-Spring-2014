package ru.spbau.turaevT.NumberStream;

/**
 * @author Turaev Timur
 * @version 1.0
 */
public class NaturalNumberStream implements IntegerStream {
    private static int current = 0;

    @Override
    public int getNext() {
        return ++current;
    }
}
