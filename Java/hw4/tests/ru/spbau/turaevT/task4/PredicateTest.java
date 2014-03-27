package ru.spbau.turaevT.task4;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.Time;

public class PredicateTest {
    private Predicate<Double> positive;
    private Predicate<Object> notNull;
    private Predicate<Object> isNull;
    private Time t1;
    private Time t2;

    @Before
    public void setUp() throws Exception {
        positive = new Predicate<Double>() {
            @Override
            public Boolean apply(Double arg) {
                return arg != null && arg > 0;
            }
        };
        notNull = new Predicate<Object>() {
            @Override
            public Boolean apply(Object arg) {
                return arg != null;
            }
        };
        isNull = new Predicate<Object>() {
            @Override
            public Boolean apply(Object arg) {
                return arg == null;
            }
        };

        long currentTime = System.currentTimeMillis();
        t1 = new Time(currentTime);
        t2 = new Time(currentTime - 2);
    }

    @Test
    public void testApply() throws Exception {
        Assert.assertTrue(positive.apply(Math.PI));
        Assert.assertTrue(notNull.apply(Math.PI));
        Assert.assertFalse(isNull.apply(2));
    }

    @Test
    public void testAnd() throws Exception {
        Assert.assertTrue(positive.and(notNull).apply(Math.PI));
        Assert.assertFalse(positive.and(notNull).apply(null));
        Assert.assertFalse(positive.and(isNull).apply(null));
    }

    @Test
    public void testOr() throws Exception {
        Assert.assertTrue(positive.or(notNull).apply(Math.PI));
        Assert.assertFalse(positive.or(notNull).apply(null));
        Assert.assertTrue(positive.or(isNull).apply(null));
    }

    @Test
    public void testNot() throws Exception {
        Assert.assertFalse(positive.not().apply(Math.PI));
        Assert.assertTrue(positive.not().apply(null));
        Assert.assertTrue(isNull.not().not().not().apply(Math.PI));
    }

    @Test
    public void testAlwaysTrue() throws Exception {
        Assert.assertTrue(Predicate.alwaysTrue().apply(null));
        Assert.assertTrue(Predicate.alwaysTrue().apply(2));
    }

    @Test
    public void testAlwaysFalse() throws Exception {
        Assert.assertFalse(Predicate.alwaysFalse().apply(null));
        Assert.assertFalse(Predicate.alwaysFalse().apply(2));
    }

    @Test
    public void testNotNull() throws Exception {
        Assert.assertTrue(Predicate.notNull().apply(2));
        Assert.assertFalse(Predicate.notNull().apply(null));
    }

    @Test
    public void testEquals() throws Exception {
        Assert.assertTrue(Predicate.equals(t1).apply(t1));  // without <? super S> got compilation error
        Assert.assertFalse(Predicate.equals(12).apply(11));
    }

    @Test
    public void testLess() throws Exception {
        Assert.assertTrue(Predicate.less(t1).apply(t2));
        Assert.assertTrue(Predicate.less(12).apply(11));
        Assert.assertFalse(Predicate.less(11).apply(11));
    }
}
