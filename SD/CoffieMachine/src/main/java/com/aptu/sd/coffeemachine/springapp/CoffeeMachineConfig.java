package com.aptu.sd.coffeemachine.springapp;

import com.aptu.sd.coffeemachine.machine.Product;
import com.aptu.sd.coffeemachine.machine.SimpleMachine;
import com.aptu.sd.coffeemachine.machine.TestMachine;
import com.aptu.sd.coffeemachine.shell.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Arrays;

/**
 * Created with IntelliJ IDEA.
 * User: avoskobovitch
 * Date: 20.05.14
 * Time: 19:02
 */
@Configuration
public class CoffeeMachineConfig {

    @Bean()
    public Shell2 shell() {
        Shell2 shell = Shell2.getInstance();
        shell.setCommands(Arrays.asList(insert(), select(), cancel(), exit(), encash(), switch_cmd()));
        shell.setFirstMachine(firstMachine());
        shell.setSecondMachine(secondMachine());
        return shell;
    }

    @Bean
    public Switch switch_cmd() {
        return new Switch();
    }

    @Bean
    public SimpleMachine firstMachine() {
        SimpleMachine simpleMachine = new SimpleMachine();
        simpleMachine.setProducts(Arrays.asList(latte(), capuccino()));
        return simpleMachine;
    }

    @Bean
    public TestMachine secondMachine() {
        return new TestMachine();
    }

    @Bean
    public Product latte() {
        return new Product("latte", 4, 2);
    }

    @Bean
    public Product capuccino() {
        return new Product("capuccino", 4, 2);
    }

    @Bean
    public Insert insert() {
        return new Insert();
    }

    @Bean
    public Select select() {
        return new Select();
    }

    @Bean
    public Cancel cancel() {
        return new Cancel();
    }

    @Bean
    public Encash encash() {
        return new Encash();
    }

    @Bean
    public Exit exit() {
        return new Exit();
    }
}