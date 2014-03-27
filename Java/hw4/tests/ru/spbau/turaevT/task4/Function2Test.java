package ru.spbau.turaevT.task4;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class Function2Test {
    private Function2<Integer, Double, Boolean> round;
    private Function<Object, Integer> f42;
    private Function<Object, Object> id;
    private Function2<Integer, Integer, Integer> product;

    @Before
    public void setUp() throws Exception {
        round = new Function2<Integer, Double, Boolean>() {
            @Override
            public Boolean apply(Integer arg1, Double arg2) {
                return arg1 == Math.round(arg2);
            }
        };
        f42 = new Function<Object, Integer>() {
            @Override
            public Integer apply(Object arg) {
                return 42;
            }
        };
        id = new Function<Object, Object>() {
            @Override
            public Object apply(Object arg) {
                return arg;
            }
        };

        product = new Function2<Integer, Integer, Integer>() {
            @Override
            public Integer apply(Integer arg1, Integer arg2) {
                return arg1 * arg2;
            }
        };
    }

    @Test
    public void testApply() throws Exception {
        Assert.assertTrue(round.apply(42, 42.35431));
        Assert.assertFalse(round.apply(42, 42.5));
        Assert.assertFalse(round.apply(42, 42.999999));
    }

    @Test
    public void testThen() throws Exception {
        Assert.assertTrue(round.then(f42).then(id).apply(545, 23.2).equals(42));
    }

    @Test
    public void testBind1() throws Exception {
        Assert.assertEquals(product.bind1(2).apply(7).intValue(), 14);
    }

    @Test
    public void testBind2() throws Exception {
        Assert.assertEquals(product.bind2(2).apply(-7).intValue(), -14);
    }
}
