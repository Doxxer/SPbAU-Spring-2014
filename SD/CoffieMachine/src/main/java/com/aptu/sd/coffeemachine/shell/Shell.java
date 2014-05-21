package com.aptu.sd.coffeemachine.shell;

import com.aptu.sd.coffeemachine.machine.VendingMachine;
import org.apache.commons.lang.StringUtils;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/22/12, 11:41 PM
 */
public class Shell {
    private Map<String, Command> commands = new HashMap<String, Command>();
    private VendingMachine machine;

    public void setCommands(List<Command> commands) {
        for (Command command : commands) {
            this.commands.put(command.getClass().getSimpleName().toLowerCase(), command);
        }
    }

    public void run() {
        System.out.println("Supported commands [" + StringUtils.join(commands.keySet(), ",") + "] ");
        System.out.print(">");
        Scanner scanner = new Scanner(System.in);
        while (true) {
            String line = scanner.nextLine();
            String[] split = line.split("\\s");
            if (split.length > 0) {
                String cmdName = split[0];
                Command command = commands.get(cmdName);
                if (command == null) {
                    System.out.println("Unknown command: " + cmdName);
                } else {
                    String[] args = Arrays.copyOfRange(split, 1, split.length);
                    try {
                        command.execute(args, machine);
                    } catch (CommandParseException e) {
                        System.out.println("Invalid " + cmdName + " arguments: " + StringUtils.join(args, " "));
                    }
                }
            }
        }
    }

    public void setMachine(VendingMachine machine) {
        this.machine = machine;
    }
}