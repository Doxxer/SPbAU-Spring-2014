package ru.spbau.turaevT.task4;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.Time;

public class FunctionalComparatorTest {

    private FunctionalComparator<Time, Time> comparator1;
    private Time t1;
    private FunctionalComparator<Integer, String> comparator2;

    @Before
    public void setUp() throws Exception {
        Function<Time, Time> id = new Function<Time, Time>() {
            @Override
            public Time apply(Time arg) {
                return arg;
            }
        };

        Function<Object, String> compareByLength = new Function<Object, String>() {
            @Override
            public String apply(Object arg) {
                return arg.toString();
            }
        };

        comparator1 = new FunctionalComparator<>(id);
        comparator2 = new FunctionalComparator<>(compareByLength);

        long currentTime = System.currentTimeMillis();
        t1 = new Time(currentTime);
    }

    @Test
    public void testCompare() throws Exception {
        Assert.assertEquals(comparator1.compare(t1, t1), 0);
        Assert.assertTrue(comparator2.compare(2, -3) > 0);
    }
}
