package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.AutomatException;
import com.aptu.sd.coffeemachine.machine.DepositTooSmallException;
import com.aptu.sd.coffeemachine.machine.NoSuchProductException;
import com.aptu.sd.coffeemachine.machine.VendingMachine;

import static com.aptu.sd.coffeemachine.shell.CommandUtil.assertArgsLength;

public class Select implements Command {
    @Override
    public void execute(String[] args, VendingMachine machine) throws CommandParseException {
        assertArgsLength(args, 1);
        String product = args[0];
        try {
            machine.purchaseProduct(product);
            System.out.println("Take your " + product);
        } catch (NoSuchProductException | DepositTooSmallException | AutomatException e) {
            System.out.println(e.getMessage());
        }
    }
}
