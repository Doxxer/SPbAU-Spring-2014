package com.aptu.sd.coffeemachine.machine;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/22/12, 11:38 PM
 */
public interface VendingMachine {

    long getDeposit();

    void deposit(long amount) throws NonPositiveDepositException, AutomatException;

    long cancel() throws AutomatException;

    void purchaseProduct(String productName) throws NoSuchProductException, DepositTooSmallException, AutomatException;

    long encash() throws AutomatException;
}