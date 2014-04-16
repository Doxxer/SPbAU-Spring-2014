package ru.spbau.turaevT.NumberStream;

/**
 * @author Turaev Timur
 * @version 1.0
 */
public abstract class AbstractFilter implements IntegerStream {

    private final IntegerStream stream;

    public AbstractFilter(IntegerStream stream) {
        this.stream = stream;
    }

    @Override
    public int getNext() {
        return stream.getNext();
    }
}
