package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.AutomatException;
import com.aptu.sd.coffeemachine.machine.VendingMachine;

public class Encash implements Command {
    @Override
    public void execute(String[] args, VendingMachine machine) throws CommandParseException {
        try {
            long cash = machine.encash();
            System.out.println("Encashed: " + cash);
        } catch (AutomatException e) {
            System.out.println(e.getMessage());
        }
    }
}
