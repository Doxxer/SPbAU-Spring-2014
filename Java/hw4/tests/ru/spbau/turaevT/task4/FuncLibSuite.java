package ru.spbau.turaevT.task4;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        FunctionTest.class,
        PredicateTest.class,
        Function2Test.class,
        MapTest.class,
        FilterTest.class,
        TakeTest.class,
        FunctionalComparatorTest.class,
        FoldrTest.class
})
public class FuncLibSuite {
}
