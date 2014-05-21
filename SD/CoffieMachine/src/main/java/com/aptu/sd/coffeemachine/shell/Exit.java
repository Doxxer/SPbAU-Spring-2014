package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.VendingMachine;

public class Exit implements Command {
    @Override
    public void execute(String[] args, VendingMachine machine) throws CommandParseException {
        System.exit(0);
    }

}
