package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.VendingMachine;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/23/12, 12:40 AM
 */
public class Exit implements Command {
    @Override
    public void execute(String[] args, VendingMachine machine) throws CommandParseException {

        System.exit(0);
    }
}
