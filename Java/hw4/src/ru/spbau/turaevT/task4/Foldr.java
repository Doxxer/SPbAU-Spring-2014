package ru.spbau.turaevT.task4;

import java.io.BufferedReader;
import java.util.Collections;
import java.util.List;

/**
 * Represent a binary function that accepts two arguments and produces a result.
 * <p/>
 * First argument is the binary operator that will reduce the list
 * Second argument is the initial value
 * Return the function that accepts the list and returns reduced value
 *
 * @param <A> the type of elements of list
 * @param <B> type of the return value of the binary operator, and the type of initial value
 * @author Turaev Timur
 * @version 1.0
 */
public class Foldr<A, B> extends Function2<Function2<? super A, ? super B, B>, B, Function<List<? extends A>, B>> {

    /**
     * Applies this function to the given arguments.
     *
     * @param arg1 the binary operator that will reduce the list
     * @param arg2 the initial value
     * @return the function that accepts the list and returns reduced value
     */
    @Override
    public Function<List<? extends A>, B> apply(final Function2<? super A, ? super B, B> arg1, final B arg2) {
        return new Function<List<? extends A>, B>() {
            @Override
            public B apply(List<? extends A> arg) {
                B acc = arg2;
                Collections.reverse(arg);
                for (A element : arg) {
                    acc = arg1.apply(element, acc);
                }
                return acc;
            }
        };
    }
}
