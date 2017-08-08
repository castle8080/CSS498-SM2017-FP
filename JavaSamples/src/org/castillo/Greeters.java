package org.castillo;

import java.util.function.Function;
import java.util.regex.Pattern;

public class Greeters {
    public static Function<String, String> create(String format) {
        String[] parts = format.split(Pattern.quote("#{name}"));
        return (String name) -> String.join(name, parts);
    }
    public static Function<String, String> create2(String format) {
        String[] parts = format.split(Pattern.quote("#{name}"));
        return new Function<String, String>() {
            public String apply(String name) {
                return String.join(name, parts);
            }
        };
    }
}
