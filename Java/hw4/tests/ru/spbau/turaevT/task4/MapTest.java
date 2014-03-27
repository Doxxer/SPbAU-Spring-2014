package ru.spbau.turaevT.task4;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collection;

public class MapTest {

    private Function<Number, String> toString;
    private Map<Integer, String> toStringMap;
    private ArrayList<Integer> list;
    private ArrayList<String> expected;

    @Before
    public void setUp() throws Exception {
        toStringMap = new Map<>();
        toString = new Function<Number, String>() {
            @Override
            public String apply(Number arg) {
                return arg.toString();
            }
        };

        list = new ArrayList<>();
        expected = new ArrayList<>();

        for (int i = 0; i < 10; i++) {
            list.add(i);
            expected.add(String.valueOf(i));
        }
    }

    @Test
    public void testApply() throws Exception {
        Collection<String> result = toStringMap.apply(toString, list);
        Assert.assertArrayEquals(result.toArray(), expected.toArray());
    }
}
