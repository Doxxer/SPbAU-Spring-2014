package ru.spbau.turaevT.task4;

/**
 * Represents a function that accepts one argument and produces a result.
 *
 * @param <T> the type of the input to the function
 * @param <R> the type of the result of the function
 * @author Turaev Timur
 * @version 1.0
 */
public abstract class Function<T, R> {
    /**
     * Applies this function to the given argument.
     *
     * @param arg the function argument
     * @return the function result
     */
    public abstract R apply(T arg);


    /**
     * Returns a composed function that applies this function to its input,
     * and then applies the given function to the result.
     *
     * @param <V> the type of output of the given function, and of the composed function
     * @param f   the function to apply after this function is applied
     * @return a composed function that applies this function and then applies the given function
     */
    public <V> Function<T, V> then(final Function<? super R, ? extends V> f) {
        return new Function<T, V>() {
            @Override
            public V apply(T arg) {
                return f.apply(Function.this.apply(arg));
            }
        };
    }
}
