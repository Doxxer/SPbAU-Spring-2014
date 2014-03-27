package ru.spbau.turaevT.task4;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collection;

public class FilterTest {
    private ArrayList<Integer> list;
    private ArrayList<Number> expected;
    private Predicate<Object> predicate;
    private Filter<Number> filter;

    @Before
    public void setUp() throws Exception {
        filter = new Filter<>();

        predicate = new Predicate<Object>() {
            @Override
            public Boolean apply(Object arg) {
                return arg.toString().length() > 2;
            }
        };

        list = new ArrayList<>();
        for (int i = 0; i < 110; i++) {
            list.add(i);
        }
        expected = new ArrayList<>();
        for (int i = 100; i < 110; i++) {
            expected.add(i);
        }
    }

    @Test
    public void testApply() throws Exception {
        Collection<Number> result = filter.apply(predicate, list);
        Assert.assertArrayEquals(result.toArray(), expected.toArray());
    }
}
