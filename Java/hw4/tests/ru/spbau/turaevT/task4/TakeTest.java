package ru.spbau.turaevT.task4;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class TakeTest {
    private Take<Number> take;
    private List<Integer> list;

    @Before
    public void setUp() throws Exception {
        take = new Take<>();

        list = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            list.add(i);
        }
    }

    @Test
    public void testApply() throws Exception {
        Assert.assertEquals(take.apply(7, list).size(), 7);
        Assert.assertEquals(take.apply(100, list).size(), 100);
        Assert.assertEquals(take.apply(150, list).size(), 100);
        Assert.assertEquals(take.apply(0, list).size(), 0);
        Assert.assertEquals(take.apply(-1, list).size(), 0);
        Assert.assertEquals(take.apply(-150, list).size(), 0);
    }
}
