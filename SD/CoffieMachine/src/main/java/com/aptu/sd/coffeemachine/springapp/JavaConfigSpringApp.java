package com.aptu.sd.coffeemachine.springapp;

import com.aptu.sd.coffeemachine.shell.Shell;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * Created with IntelliJ IDEA.
 * User: avoskobovitch
 * Date: 20.05.14
 * Time: 19:05
 */
public class JavaConfigSpringApp {
    public static void main(String[] args) {
        ApplicationContext applicationContext = new AnnotationConfigApplicationContext(CoffeeMachineConfig.class);
        Shell shell = (Shell) applicationContext.getBean("shell");
        shell.run();
    }
}
