package ru.spbau.turaevT.task4;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class FunctionTest {
    private Function<Integer, Integer> fun1;
    private Function<Object, Object> id;
    private Function<Number, String> toString;

    @Before
    public void setUp() throws Exception {
        fun1 = new Function<Integer, Integer>() {
            @Override
            public Integer apply(Integer arg) {
                return 2;
            }
        };
        toString = new Function<Number, String>() {
            @Override
            public String apply(Number arg) {
                return arg.toString();
            }
        };
        id = new Function<Object, Object>() {
            @Override
            public Object apply(Object arg) {
                return arg;
            }
        };
    }


    @Test
    public void testApply() throws Exception {
        Assert.assertEquals(fun1.apply(2014).intValue(), 2);
        Assert.assertEquals(id.apply(2), 2);
        Assert.assertTrue("293847".equals(toString.apply(293847)));
        Assert.assertTrue("-39128391".equals(toString.apply(-39128391)));
    }

    @Test
    public void testThen() throws Exception {
        Assert.assertEquals(fun1.then(id).then(id).then(id).apply(89), 2);
        Assert.assertEquals(fun1.then(toString).apply(472837), "2");
        Assert.assertEquals(toString.then(id).apply(Math.PI), Double.valueOf(Math.PI).toString());
    }
}
