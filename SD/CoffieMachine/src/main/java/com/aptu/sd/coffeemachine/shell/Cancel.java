package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.AutomatException;
import com.aptu.sd.coffeemachine.machine.VendingMachine;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/22/12, 11:58 PM
 */
public class Cancel implements Command {

    public Cancel() {
        System.out.println("Cancel ctor called");
    }

    @Override
    public void execute(String[] args, VendingMachine machine) {
        try {
            long amount = machine.cancel();
            System.out.println("Returned: " + amount);
        } catch (AutomatException e) {
            System.out.println(e.getMessage());
        }

    }
}
