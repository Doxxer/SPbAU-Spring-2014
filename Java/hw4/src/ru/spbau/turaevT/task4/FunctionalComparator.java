package ru.spbau.turaevT.task4;

import java.util.Comparator;


/**
 * FunctionalComparator represents comparator, that can compare arguments as images of specified mapping
 *
 * @param <T> type of comparing values
 * @param <A> type of return value of specified mapping, type if image of input values
 * @author Turaev Timur
 * @version 1.0
 */
public class FunctionalComparator<T, A extends Comparable<? super A>> implements Comparator<T> {

    private final Function<? super T, A> function;

    /**
     * Constructs comparator with specified mapping function
     *
     * @param function mapping function which will be applied to input values
     */
    public FunctionalComparator(Function<? super T, A> function) {
        this.function = function;
    }

    /**
     * Compares to values as images of specified mapping
     * <p/>
     * Returns a negative integer, zero, or a positive integer as the first argument is
     * less than, equal to, or greater than the second.
     *
     * @param o1 the first object to be compared.
     * @param o2 the second object to be compared.
     * @return a negative integer, zero, or a positive integer as the first argument is
     * less than, equal to, or greater than the second.
     */
    @Override
    public int compare(T o1, T o2) {
        return function.apply(o1).compareTo(function.apply(o2));
    }
}
