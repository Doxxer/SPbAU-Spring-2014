package com.aptu.sd.coffeemachine.machine;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/22/12, 11:27 PM
 */
public class NoSuchProductException extends Exception {
    public NoSuchProductException() {
    }

    public NoSuchProductException(String s) {
        super(s);
    }

    public NoSuchProductException(String s, Throwable throwable) {
        super(s, throwable);
    }

    public NoSuchProductException(Throwable throwable) {
        super(throwable);
    }
}
