package ru.spbau.turaevT.NumberStream;

/**
 * @author Turaev Timur
 * @version 1.0
 */
public class FibonacciStream extends AbstractFilter {
    private int prevFibonacci = 0;
    private int currentFibonacci = 1;

    public FibonacciStream(IntegerStream stream) {
        super(stream);
    }

    @Override
    public int getNext() {
        int current = super.getNext();
        while (current != prevFibonacci + currentFibonacci) {
            current = super.getNext();
        }
        prevFibonacci = currentFibonacci;
        currentFibonacci = current;
        return current;
    }
}
