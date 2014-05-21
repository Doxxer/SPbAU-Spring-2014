package com.aptu.sd.coffeemachine.machine;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/23/12, 9:41 AM
 */
public class NonPositiveDepositException extends Exception {
    public NonPositiveDepositException() {
    }

    public NonPositiveDepositException(String s) {
        super(s);
    }

    public NonPositiveDepositException(String s, Throwable throwable) {
        super(s, throwable);
    }

    public NonPositiveDepositException(Throwable throwable) {
        super(throwable);
    }
}
