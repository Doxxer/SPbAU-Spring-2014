package ru.spbau.turaevT.NumberStream;

/**
 * Prints first 10 Fibonacci numbers divisible by 3
 */
class Main {

    public static void main(String[] args) {
        IntegerStream fib3 = new Divisible3(new FibonacciStream(new NaturalNumberStream()));

        for (int i = 0; i < 10; i++) {
            System.out.println(fib3.getNext());
        }
    }
}
