package com.aptu.sd.coffeemachine.machine;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/23/12, 9:41 AM
 */
public class NonPositiveDepositException extends Exception {

    public NonPositiveDepositException(String s) {
        super(s);
    }

}
