package ru.spbau.turaevT.NumberStream;

/**
 * @author Turaev Timur
 * @version 1.0
 */
public class Divisible3 extends AbstractFilter {

    public Divisible3(IntegerStream stream) {
        super(stream);
    }

    @Override
    public int getNext() {
        int current = super.getNext();
        while (current % 3 != 0) {
            current = super.getNext();
        }
        return current;
    }
}
