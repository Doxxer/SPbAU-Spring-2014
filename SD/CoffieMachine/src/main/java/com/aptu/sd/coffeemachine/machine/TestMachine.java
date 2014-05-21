package com.aptu.sd.coffeemachine.machine;

public class TestMachine implements VendingMachine {

    @Override
    public long getDeposit()
    {
        return 0;
    }

    @Override
    public void deposit(long amount) throws NonPositiveDepositException, AutomatException {
        throw new AutomatException("It's test automat");
    }

    @Override
    public long cancel() throws AutomatException {
        throw new AutomatException("It's test automat");
    }

    @Override
    public void purchaseProduct(String productName) throws NoSuchProductException, DepositTooSmallException, AutomatException {
        throw new AutomatException("It's test automat");
    }

    @Override
    public long encash() throws AutomatException {
        throw new AutomatException("It's test automat");
    }
}
