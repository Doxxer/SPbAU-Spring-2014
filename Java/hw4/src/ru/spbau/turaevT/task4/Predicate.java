package ru.spbau.turaevT.task4;

/**
 * Represents a predicate (boolean-valued function) of one argument.
 *
 * @param <T> the type of the input to the predicate
 * @author Turaev Timur
 * @version 1.0
 */
public abstract class Predicate<T> extends Function<T, Boolean> {

    /**
     * Returns an always true predicate
     *
     * @return an always true predicate
     */
    public static <S> Predicate<S> alwaysTrue() {
        return new Predicate<S>() {
            @Override
            public Boolean apply(S arg) {
                return true;
            }
        };
    }

    /**
     * Returns an always false predicate
     *
     * @return an always false predicate
     */
    public static <S> Predicate<S> alwaysFalse() {
        return new Predicate<S>() {
            @Override
            public Boolean apply(S arg) {
                return false;
            }
        };
    }

    /**
     * Returns a predicate that tests if given object is not null
     *
     * @return a predicate that tests if given object is not null
     */
    public static <S> Predicate<S> notNull() {
        return new Predicate<S>() {
            @Override
            public Boolean apply(S arg) {
                return arg != null;
            }
        };
    }

    /**
     * Returns a predicate that tests if two arguments are equal according to {@link java.lang.Comparable}.
     *
     * @param arg the object with which to compare for equality
     * @param <S> the type of arguments to the predicate
     * @return a predicate that tests if two arguments are equal according to {@link java.lang.Comparable}.
     */
    public static <S extends Comparable<? super S>> Predicate<S> equals(final S arg) {
        return new Predicate<S>() {
            @Override
            public Boolean apply(S s) {
                return arg.compareTo(s) == 0;
            }
        };
    }

    /**
     * Returns a predicate that tests if given argument is less than this argument according to {@link java.lang.Comparable}.
     *
     * @param arg the object with which to compare
     * @param <S> the type of arguments to the predicate
     * @return a predicate that tests if given argument is less than this argument according to {@link java.lang.Comparable}.
     */
    public static <S extends Comparable<? super S>> Predicate<S> less(final S arg) {
        return new Predicate<S>() {
            @Override
            public Boolean apply(S s) {
                return arg.compareTo(s) > 0;
            }
        };
    }

    /**
     * Returns a composed predicate that represents logical AND of this predicate and another.
     *
     * @param other a predicate that will be logically-ANDed with this predicate
     * @return a composed predicate that represents logical AND of this predicate and another.
     */
    public Predicate<T> and(final Predicate<? super T> other) {
        return new Predicate<T>() {
            @Override
            public Boolean apply(T arg) {
                return Predicate.this.apply(arg) && other.apply(arg);
            }
        };
    }

    /**
     * Returns a composed predicate that represents logical OR of this predicate and another.
     *
     * @param other a predicate that will be logically-ORed with this predicate
     * @return a composed predicate that represents logical OR of this predicate and another.
     */
    public Predicate<T> or(final Predicate<? super T> other) {
        return new Predicate<T>() {
            @Override
            public Boolean apply(T arg) {
                return Predicate.this.apply(arg) || other.apply(arg);
            }
        };
    }

    /**
     * Returns a predicate that represents the logical negation of this predicate.
     *
     * @return a predicate that represents the logical negation of this predicate.
     */
    public Predicate<T> not() {
        return new Predicate<T>() {
            @Override
            public Boolean apply(T arg) {
                return !Predicate.this.apply(arg);
            }
        };
    }
}
