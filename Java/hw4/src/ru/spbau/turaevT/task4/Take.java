package ru.spbau.turaevT.task4;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;


/**
 * Represent a binary function that accepts two arguments and produces a result.
 * <p/>
 * First argument is length of prefix
 * Second argument is the list, which prefix will be returned
 * Returns the prefix of given list specified length
 *
 * @param <T> the type of elements of list
 * @author Turaev Timur
 * @version 1.0
 */
public class Take<T> extends Function2<Integer, Iterable<? extends T>, Collection<T>> {
    /**
     * Applies this function to the given arguments.
     *
     * @param count the length of prefix
     * @param list  the list, which prefix will be returned
     * @return the prefix of given list
     */
    @Override
    public Collection<T> apply(Integer count, Iterable<? extends T> list) {
        List<T> result = new ArrayList<>();

        if (count <= 0)
            return result;

        int counter = 0;
        for (T elem : list) {
            if (counter++ == count) {
                break;
            }
            result.add(elem);
        }
        return result;
    }
}
