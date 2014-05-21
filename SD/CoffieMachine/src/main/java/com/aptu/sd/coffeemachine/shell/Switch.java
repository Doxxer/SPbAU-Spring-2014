package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.VendingMachine;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Switch implements Command {
    @Override
    public void execute(String[] args, VendingMachine machine) throws CommandParseException {
        Shell2.getInstance().switchMachines();
        System.out.println("Machines switched. Current machine = " + Shell2.getInstance().getCurrentMachine().getClass().getSimpleName());
    }
}
