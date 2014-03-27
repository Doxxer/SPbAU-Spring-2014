package ru.spbau.turaevT.task4;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Represents a binary function that accepts two arguments and produces a result.
 * <p/>
 * First argument is a unary function to be applied to each element of the list (second argument)
 * Second argument is a list, for which argument will be applied function (first argument)
 * Result is list obtained by applying function
 *
 * @param <T> the type of elements of list
 * @param <R> the type of elements of list after applying the function
 * @author Turaev Timur
 * @version 1.0
 */
public class Map<T, R> extends Function2<Function<? super T, ? extends R>, Iterable<? extends T>, Collection<R>> {

    /**
     * Applies given function to the each argument of the given list.
     *
     * @param func given function to be applied to the each argument of the list
     * @param list the list for which argument will be applied function
     * @return the list obtained by applying function
     */
    @Override
    public Collection<R> apply(Function<? super T, ? extends R> func, Iterable<? extends T> list) {
        Collection<R> result = new ArrayList<>();
        for (T element : list) {
            result.add(func.apply(element));
        }
        return result;
    }
}
