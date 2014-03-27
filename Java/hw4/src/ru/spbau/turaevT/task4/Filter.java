package ru.spbau.turaevT.task4;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Represent a binary function that accepts two arguments and produces a result.
 * <p/>
 * First argument is a predicate to be applied to each element of the list (second argument)
 * Second argument is a list, for which argument will be applied function (first argument)
 * Returns a list constructed from elements of a list fulfilling the predicate
 *
 * @param <T> the type of elements of list
 * @author Turaev Timur
 * @version 1.0
 */
public class Filter<T> extends Function2<Predicate<? super T>, Iterable<? extends T>, Collection<T>> {
    /**
     * Filters given list via given predicate
     * Returns a list constructed from elements of a list fulfilling the predicate
     *
     * @param func the predicate
     * @param list the list for which element will be applied predicate
     * @return a list constructed from elements of a list fulfilling the predicate
     */
    @Override
    public Collection<T> apply(Predicate<? super T> func, Iterable<? extends T> list) {
        Collection<T> result = new ArrayList<>();
        for (T element : list) {
            if (func.apply(element)) {
                result.add(element);
            }
        }
        return result;
    }
}
