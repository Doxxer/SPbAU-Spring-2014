package ru.spbau.turaevT.NumberStream;

/**
 * @author Turaev Timur
 * @version 1.0
 */
public class Divisible11 extends AbstractFilter {
    public Divisible11(IntegerStream stream) {
        super(stream);
    }

    @Override
    public int getNext() {
        int current = super.getNext();
        while (current % 11 != 0) {
            current = super.getNext();
        }
        return current;
    }
}
