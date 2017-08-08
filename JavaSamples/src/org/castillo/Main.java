package org.castillo;

import java.util.regex.Pattern;
import java.util.function.*;

public class Main {

    public static void main(String[] args) {
        Function<String, String> nice = Greeters.create2("Hi #{name}!");
        Function<String, String> mean = Greeters.create("Go away #{name}!");
        System.out.println(nice.apply("Joe"));
        System.out.println(mean.apply("Bob"));
    }
}
