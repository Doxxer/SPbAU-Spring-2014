package Parsing;

import java.util.Iterator;

public interface Selector<T> extends Iterator<T> {
    T current();
}
