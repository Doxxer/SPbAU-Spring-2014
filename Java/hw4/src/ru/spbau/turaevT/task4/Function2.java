package ru.spbau.turaevT.task4;

/**
 * Represents a function that accepts two arguments and produces a result.
 *
 * @param <T1> the type of the first argument to the function
 * @param <T2> the type of the second argument to the function
 * @param <R>  the type of the result of the function
 * @author Turaev Timur
 * @version 1.0
 */
public abstract class Function2<T1, T2, R> {

    /**
     * Applies this function to the given arguments.
     *
     * @param arg1 the first function argument
     * @param arg2 the second function argument
     * @return the function result
     */
    public abstract R apply(T1 arg1, T2 arg2);


    /**
     * Returns a composed function that applies this function to its input,
     * and then applies the given function to the result.
     *
     * @param <V> the type of output of the given function, and of the composed function
     * @param f   the function to apply after this function is applied
     * @return a composed function that applies this function and then applies the given function
     */
    public <V> Function2<T1, T2, V> then(final Function<? super R, ? extends V> f) {
        return new Function2<T1, T2, V>() {
            @Override
            public V apply(T1 arg1, T2 arg2) {
                return f.apply(Function2.this.apply(arg1, arg2));
            }
        };
    }

    /**
     * Binds the first argument of this function with specified value
     *
     * @param arg1 value of the first argument of this function
     * @return one-argument-function
     */
    public Function<T2, R> bind1(final T1 arg1) {
        return new Function<T2, R>() {
            @Override
            public R apply(T2 arg2) {
                return Function2.this.apply(arg1, arg2);
            }
        };
    }

    /**
     * Binds the second argument of this function with specified value
     *
     * @param arg2 value of the second argument of this function
     * @return one-argument-function
     */
    public Function<T1, R> bind2(final T2 arg2) {
        return new Function<T1, R>() {
            @Override
            public R apply(T1 arg1) {
                return Function2.this.apply(arg1, arg2);
            }
        };
    }
}
