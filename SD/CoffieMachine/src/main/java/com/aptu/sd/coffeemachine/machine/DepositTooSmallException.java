package com.aptu.sd.coffeemachine.machine;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/22/12, 11:34 PM
 */
public class DepositTooSmallException extends Exception {
    public DepositTooSmallException() {
    }

    public DepositTooSmallException(String s) {
        super(s);
    }

    public DepositTooSmallException(String s, Throwable throwable) {
        super(s, throwable);
    }

    public DepositTooSmallException(Throwable throwable) {
        super(throwable);
    }
}
