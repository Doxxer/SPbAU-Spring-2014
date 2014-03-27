package ru.spbau.turaevT.task4;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class FoldrTest {
    private Foldr<Double, Double> fold;
    private Function2<Double, Double, Double> func;
    private List<Double> list;

    @Before
    public void setUp() throws Exception {
        fold = new Foldr<>();

        func = new Function2<Double, Double, Double>() {
            @Override
            public Double apply(Double arg1, Double arg2) {
                return arg1 / arg2;
            }
        };

        list = new ArrayList<>();
        list.add(8.0);
        list.add(12.0);
        list.add(24.0);
        list.add(4.0);
    }

    @Test
    public void testApply() throws Exception {
        Double res = fold.apply(func, 2.0).apply(list);
        Assert.assertTrue(res == 8.0);
    }
}
