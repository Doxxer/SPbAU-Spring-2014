package com.aptu.sd.coffeemachine;

import com.aptu.sd.coffeemachine.machine.*;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

import static junit.framework.Assert.assertEquals;

public class SimpleMachineTest {
    SimpleMachine machine = new SimpleMachine();

    @Before
    public void init() {
        machine.setProducts(Arrays.asList(new Product("latte", 6, 10)));
    }

    @Test
    public void zeroDepositIfNothingInserted() {
        assertEquals(0, machine.cancel());
    }

    @Test
    public void returnDepositOnCancel() throws NonPositiveDepositException {
        machine.deposit(10);
        assertEquals(10, machine.cancel());
    }

    @Test(expected = NonPositiveDepositException.class)
    public void negativeDepositIsNotAllowed() throws NonPositiveDepositException {
        machine.deposit(-10);
    }

    @Test(expected = NoSuchProductException.class)
    public void selectNonExistentProduct() throws DepositTooSmallException, NoSuchProductException {
        machine.purchaseProduct("water");
    }

    @Test(expected = DepositTooSmallException.class)
    public void depositTooSmall() throws DepositTooSmallException, NoSuchProductException, NonPositiveDepositException {
        machine.deposit(1);
        machine.purchaseProduct("latte");
    }

    @Test
    public void purchaseProduct() throws DepositTooSmallException, NoSuchProductException, NonPositiveDepositException {
        machine.deposit(10);
        machine.purchaseProduct("latte");
        assertEquals(4, machine.cancel());
    }
}
