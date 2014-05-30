package ru.spbau.turaevT;

import java.util.ArrayList;
import java.util.List;

interface Troll {

}

class MyObject implements Troll {
    MyObject() {
    }
}

class Fruit extends MyObject {
    Fruit() {
    }
}

class Apple extends Fruit {
    // public static final synchronized native int a();
}

public class Main {
    public static void method1(List<? extends Fruit> l) {
        for (Fruit fruit : l) {

        }
    }

    public static void method2(List<? super Fruit> l) {
        for (Object o : l) {

        }

    }

    public static void main(String[] args) {
        List<? super Fruit> list1 = new ArrayList<>();

        List<? extends Fruit> list2 = new ArrayList<>();

        Object o = list1.get(0);
        list1.add(new Apple());
        list1.add(new Fruit());

        Fruit fruit = list2.get(0);
        list2.add(null);

        // ---------

        method1(new ArrayList<Fruit>());
        method1(new ArrayList<Apple>());

        method2(new ArrayList<Fruit>());
        method2(new ArrayList<MyObject>());
    }
}
