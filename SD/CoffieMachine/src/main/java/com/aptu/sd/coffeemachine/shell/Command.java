package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.VendingMachine;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/22/12, 11:44 PM
 */
public interface Command {
    public void execute(String[] args, VendingMachine machine) throws CommandParseException;
}
