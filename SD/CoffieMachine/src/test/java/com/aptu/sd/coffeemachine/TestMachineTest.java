package com.aptu.sd.coffeemachine;

import com.aptu.sd.coffeemachine.machine.*;
import org.junit.Before;
import org.junit.Test;

public class TestMachineTest {
    TestMachine machine = new TestMachine();

    @Before
    public void init() {

    }

    @Test(expected = AutomatException.class)
    public void cancel() throws AutomatException {
        machine.cancel();
    }

    @Test(expected = AutomatException.class)
    public void deposit() throws NonPositiveDepositException, NoSuchProductException, DepositTooSmallException, AutomatException {
        machine.deposit(10);
    }

    @Test(expected = AutomatException.class)
    public void purchase() throws DepositTooSmallException, NoSuchProductException, AutomatException {
        machine.purchaseProduct("water");
    }

    @Test(expected = AutomatException.class)
    public void encash() throws DepositTooSmallException, NoSuchProductException, NonPositiveDepositException, AutomatException {
        machine.encash();
    }
}
