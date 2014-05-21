package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.AutomatException;
import com.aptu.sd.coffeemachine.machine.NonPositiveDepositException;
import com.aptu.sd.coffeemachine.machine.VendingMachine;

import static com.aptu.sd.coffeemachine.shell.CommandUtil.assertArgsLength;

public class Insert implements Command {
    @Override
    public void execute(String[] args, VendingMachine machine) throws CommandParseException {
        assertArgsLength(args, 1);
        try {
            long amount = Long.parseLong(args[0].trim());
            machine.deposit(amount);
            System.out.println("Current deposit : " + machine.getDeposit());
        } catch (NumberFormatException e) {
            throw new CommandParseException(e);
        } catch (NonPositiveDepositException | AutomatException e) {
            System.out.println(e.getMessage());
        }
    }
}
