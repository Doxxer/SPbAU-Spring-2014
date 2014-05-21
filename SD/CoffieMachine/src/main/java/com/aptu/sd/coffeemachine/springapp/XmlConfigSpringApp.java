package com.aptu.sd.coffeemachine.springapp;

import com.aptu.sd.coffeemachine.shell.Shell2;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/23/12, 8:35 AM
 */
public class XmlConfigSpringApp {
    public static void main( String[] args ) {
        ApplicationContext applicationContext = new ClassPathXmlApplicationContext("com/aptu/sd/springapp/app-context.xml");
        Shell2 shell = (Shell2) applicationContext.getBean("shell");
        shell.run();
    }

}
