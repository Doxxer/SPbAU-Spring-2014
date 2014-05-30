package ru.spbau.turaevT;

class A {
    private static int a = m();

    private static int m() {
        System.out.println("a");
        return 0;
    }
}

class B {
    private static int a = m();

    public static int m() {
        System.out.println("b");
        return 0;
    }
}

class C {
    private static int a = m();

    public static int m() {
        System.out.println("c");
        return 0;
    }
}

public class TestThread {
    public static void main(String args[]) {
        B b;

        C.m();

        b = new B();
        b.m();
    }
}